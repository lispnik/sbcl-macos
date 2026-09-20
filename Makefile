# Lisp Listener.
#
# `check' runs anywhere, including on Linux with no Objective-C runtime.
# Everything below it wants macOS, and an SBCL built --with-sb-safepoint.

SBCL ?= sbcl

.PHONY: check syntax-check compile-check run app clean

## Both off-macOS checks.  Neither can tell you the program works.
check: syntax-check compile-check

## Does it parse?  Reads every form with *READ-SUPPRESS*; needs nothing at all.
syntax-check:
	$(SBCL) --script tools/syntax-check.lisp

## Does it compile?  Builds src/ against tools/stubs/, so the compiler can
## report undefined functions, wrong argument counts and macros that will not
## expand -- none of which a parse check can see.
compile-check:
	$(SBCL) --script tools/compile-check.lisp

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
