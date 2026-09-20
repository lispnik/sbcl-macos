;;;; lisp-listener.asd -- a Lisp Listener in a Cocoa window.
;;;;
;;;; The .app bundle is a SEPARATE system in lisp-listener-app.asd, on purpose:
;;;; :DEFSYSTEM-DEPENDS-ON is resolved when a .asd file is READ, not when the
;;;; system it belongs to is built, so declaring the bundle here would make
;;;; asdf-macos-app a hard requirement for anyone who only wants to load the
;;;; library.  utc-status-app carries the bug report that taught this.

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
  :serial t
  :depends-on ("objc" "bordeaux-threads")
  :components ((:module "src"
                :serial t
                :components
                ((:file "package")
                 (:file "main-thread")
                 (:file "queue")
                 (:file "listener")
                 (:file "view")
                 (:file "streams")
                 (:file "repl")
                 (:file "window")
                 (:file "screenshot")
                 (:file "app")))))
