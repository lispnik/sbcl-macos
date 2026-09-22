# Lisp Listener.
#
# `check' runs anywhere, including on Linux with no Objective-C runtime.
# Everything below it wants macOS, and an SBCL built --with-sb-safepoint.

SBCL ?= sbcl

.PHONY: deps check syntax-check compile-check test test-ecl run app \
        ios-toolchain ios run-ios clean

ECL ?= ecl

## Restore the dependencies this project pins, into ./ocicl/.
##
## ocicl.csv is a LOCKFILE: it names each dependency by its registry digest,
## not by a version that can be re-cut, so this restores the same sources on
## every machine and every run.  It covers objc and asdf-macos-app as well, so
## a fresh clone needs no sibling checkouts -- see the README.
##
## Needs the ocicl tool itself: https://github.com/ocicl/ocicl
deps:
	ocicl install

## All three off-macOS checks.  The first two cannot tell you the program
## works; the third can, for everything that is not a window.  With ECL on the
## PATH the third runs twice, once on each Lisp the listener ships on.
check: syntax-check compile-check test
	@if command -v $(ECL) >/dev/null 2>&1; then $(MAKE) --no-print-directory test-ecl; \
	 else echo "check: no $(ECL) on the PATH, so the ECL half of the test is skipped"; fi

## Does it parse?  Reads every form with *READ-SUPPRESS*; needs nothing at all.
syntax-check:
	$(SBCL) --script tools/syntax-check.lisp

## Does it compile?  Builds src/ against tools/stubs/, so the compiler can
## report undefined functions, wrong argument counts and macros that will not
## expand -- none of which a parse check can see.
compile-check:
	$(SBCL) --script tools/compile-check.lisp macos
	$(SBCL) --script tools/compile-check.lisp ios

## Does it WORK?  Runs a real listener on the stubs -- real thread, real
## streams, real reader, evaluator and debugger -- and drives it through a
## session, the debugger, the interactive restarts, Y-OR-N-P and abort.  Only
## Cocoa is hollow, so the window, the panel and the table are untested here.
test:
	$(SBCL) --script tools/headless-test.lisp

## The same listener on ECL, the Lisp an iOS app runs: the core and the iOS
## front end on the stubs.  A stock ECL is enough; this needs no iOS toolchain.
test-ecl:
	$(ECL) --norc --load tools/headless-test.lisp

## A listener from a REPL, on thread 1.  Needs objc on the source registry.
run:
	$(SBCL) --eval '(asdf:load-system "lisp-listener")' \
	        --eval '(lisp-listener:run-listener)' --quit

## Build the bundle.  Run this with the SAFEPOINT SBCL: asdf-macos-app copies
## the runtime of whichever SBCL performs the build, so a stock one here pairs
## a stock runtime with this core and gives back the very fragility the
## safepoint build exists to remove.
app:
	$(SBCL) --eval '(asdf:make "lisp-listener-app")' --quit

## The iOS app.  ios-toolchain builds, once, the host and simulator ECLs that
## asdf-ios-app cross-compiles with (about ten minutes); set
## IOS_SIGNING_IDENTITY to build for a device as well.  asdf-ios-app is itself
## ECL code, so these run under ECL, not SBCL.
IOS_REGISTRY = CL_SOURCE_REGISTRY="$(CURDIR)//:$(CL_SOURCE_REGISTRY)"

ios-toolchain:
	$(IOS_REGISTRY) $(ECL) --norc --eval '(require :asdf)' \
	    --eval '(asdf:load-system "asdf-ios-app")' \
	    --eval '(asdf-ios-app:bootstrap-ecl)' --eval '(ext:quit 0)'

ios:
	$(IOS_REGISTRY) $(ECL) --norc --eval '(require :asdf)' \
	    --eval '(asdf:make "lisp-listener-ios")' --eval '(ext:quit 0)'

## Needs a booted simulator: open -a Simulator.
run-ios:
	$(IOS_REGISTRY) $(ECL) --norc --eval '(require :asdf)' \
	    --eval '(asdf:load-system "asdf-ios-app")' \
	    --eval '(princ (asdf-ios-app:run-in-simulator "lisp-listener-ios"))' \
	    --eval '(ext:quit 0)'

clean:
	rm -rf build
	find . -name '*.fasl' -delete
