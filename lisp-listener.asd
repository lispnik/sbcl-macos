;;;; lisp-listener.asd -- a Lisp Listener in a Cocoa window, or a UIKit one.
;;;;
;;;; Three systems.  LISP-LISTENER/CORE is everything that is not a toolkit --
;;;; the listener thread, the queues, the streams, the debugger, the transcript
;;;; over a text view's storage -- and runs on SBCL and on ECL.  LISP-LISTENER
;;;; is the core plus the AppKit front end, and keeps the name it always had.
;;;; LISP-LISTENER/IOS is the core plus the UIKit front end, for ECL on iOS.
;;;;
;;;; Component order is load-bearing within each, and tools/compile-check.lisp
;;;; and tools/headless-test.lisp carry the same lists by hand.
;;;;
;;;; The .app bundle is a SEPARATE system in lisp-listener-app.asd, on purpose:
;;;; :DEFSYSTEM-DEPENDS-ON is resolved when a .asd file is READ, not when the
;;;; system it belongs to is built, so declaring the bundle here would make
;;;; asdf-macos-app a hard requirement for anyone who only wants to load the
;;;; library.  utc-status-app carries the bug report that taught this.

(defsystem "lisp-listener/core"
  :description "The Lisp Listener's toolkit-free half, for SBCL and ECL."
  :author "Matthew Kennedy <burnsidemk@gmail.com>"
  :license "MIT"
  :version "0.1.0"
  :depends-on ("objc" "bordeaux-threads")
  :components ((:module "src"
                :serial t
                :components
                ((:file "package")
                 (:file "impl")
                 (:file "main-thread")
                 (:file "queue")
                 (:file "listener")
                 (:file "history")
                 (:file "sexp")
                 (:file "paredit")
                 (:file "keymap")
                 (:file "transcript")
                 (:file "completion")
                 (:file "paren-highlight")
                 (:file "paredit-view")
                 (:file "history-search")
                 (:file "streams")
                 (:file "config")
                 (:file "restarts")
                 (:file "repl")))))

(defsystem "lisp-listener"
  :description "A Lisp Listener in a native Cocoa window, for SBCL on macOS."
  :long-description
  "A read-eval-print loop living in an NSTextView: you type forms into the
window and the values, the output and the debugger come back in it.  Cocoa runs
on thread 1 and the listener runs on an ordinary SBCL thread; they meet at two
queues, so a long computation never freezes the window.

Every Objective-C class here is defined from Lisp through lispnik/objc.  Needs
an SBCL built --with-sb-safepoint; see the README for why."
  :author "Matthew Kennedy <burnsidemk@gmail.com>"
  :license "MIT"
  :version "0.1.0"
  :homepage "https://github.com/lispnik/sbcl-macos"
  :source-control (:git "https://github.com/lispnik/sbcl-macos.git")
  :depends-on ("lisp-listener/core")
  :components ((:module "src/macos"
                :pathname "src/macos/"
                :serial t
                :components
                ((:file "view")
                 (:file "window")
                 (:file "restarts-panel")
                 (:file "history-panel")
                 (:file "screenshot")
                 (:file "app")))))

(defsystem "lisp-listener/ios"
  :description "The Lisp Listener in a UITextView, for ECL on iOS."
  :author "Matthew Kennedy <burnsidemk@gmail.com>"
  :license "MIT"
  :version "0.1.0"
  :depends-on ("lisp-listener/core" "objc/uikit")
  :components ((:module "src/ios"
                :pathname "src/ios/"
                :serial t
                :components
                ((:file "view")
                 (:file "restarts-sheet")
                 (:file "history-sheet")
                 (:file "app")))))
