;;;; lisp-listener-app.asd -- Lisp Listener.app, built by asdf-macos-app.
;;;;
;;;;     (asdf:make "lisp-listener-app")      ; => build/Lisp Listener.app
;;;;
;;;; SEPARATE FROM lisp-listener.asd ON PURPOSE.  :DEFSYSTEM-DEPENDS-ON is
;;;; resolved when a .asd file is READ, not when the system it belongs to is
;;;; built -- so with this system in the main file, loading #:lisp-listener at
;;;; all would require asdf-macos-app to be present, and someone who had cloned
;;;; only objc would get `Component "asdf-macos-app" not found'.  utc-status-app
;;;; shipped that bug once; CI never saw it, because CI always has both.
;;;;
;;;; BUILD THIS WITH THE SAFEPOINT SBCL ITSELF.  asdf-macos-app copies the
;;;; PARENT's SB-EXT:*RUNTIME-PATHNAME* into Contents/MacOS/, so building from a
;;;; stock SBCL pairs a stock runtime with this core and hands back exactly the
;;;; libdispatch fragility the whole thing is meant to avoid.

(defsystem "lisp-listener-app"
  :defsystem-depends-on ("asdf-macos-app")
  :class :macos-app-system
  :build-operation "macos-app-op"
  :entry-point "lisp-listener:main"
  :description "A Lisp Listener in a Cocoa window, as an application."
  :author "Matthew Kennedy <burnsidemk@gmail.com>"
  :license "MIT"
  :version "0.1.0"
  :depends-on ("lisp-listener")

  :bundle-identifier "org.lispnik.sbcl-macos.lisp-listener"
  :bundle-name "Lisp Listener"
  ;; A PNG, which asdf-macos-app turns into an .icns with sips and iconutil.
  ;; res/icon.png is the alien inset in a rounded rectangle, which is what a
  ;; Mac icon is; iOS takes the full-bleed square and rounds it itself.
  :bundle-icon "res/icon.png"
  :bundle-executable "lisp-listener"
  ;; NSPrincipalClass, so AppKit is brought up as it would be for any Cocoa
  ;; application rather than halfway through our own startup.
  :bundle-principal-class "NSApplication"
  :bundle-category "public.app-category.developer-tools"
  :bundle-copyright "MIT"
  ;; Left at the default T: it catches anything that goes wrong before there is
  ;; a window, which is the one window of time the transcript cannot report on.
  :bundle-log t
  :bundle-output-directory
  #.(merge-pathnames "build/"
                     (uiop:pathname-directory-pathname
                      (or *load-truename* *default-pathname-defaults*)))
  ;; #. rather than a plain call: ASDF does not evaluate a defsystem initarg,
  ;; so the value has to be computed when this file is READ.
  :code-signing-identity #.(or (uiop:getenv "MACOS_SIGNING_IDENTITY") "-"))
