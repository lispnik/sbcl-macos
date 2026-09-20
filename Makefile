# Lisp Listener.
#
# `check' runs anywhere, including on Linux with no Objective-C runtime.
# Everything below it wants macOS, and an SBCL built --with-sb-safepoint.

SBCL ?= sbcl

.PHONY: deps check syntax-check compile-check test run app clean

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
## works; the third can, for everything that is not a window.
check: syntax-check compile-check test

## Does it parse?  Reads every form with *READ-SUPPRESS*; needs nothing at all.
syntax-check:
	$(SBCL) --script tools/syntax-check.lisp

## Does it compile?  Builds src/ against tools/stubs/, so the compiler can
## report undefined functions, wrong argument counts and macros that will not
## expand -- none of which a parse check can see.
compile-check:
	$(SBCL) --script tools/compile-check.lisp

## Does it WORK?  Runs a real listener on the stubs -- real thread, real
## streams, real reader, evaluator and debugger -- and drives it through a
## session, the debugger, the interactive restarts, Y-OR-N-P and abort.  Only
## Cocoa is hollow, so the window, the panel and the table are untested here.
test:
	$(SBCL) --script tools/headless-test.lisp

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

clean:
	rm -rf build
	find . -name '*.fasl' -delete
