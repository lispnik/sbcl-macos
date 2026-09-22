;;;; lisp-listener-ios.asd -- the Lisp Listener as an iOS app, built by
;;;; asdf-ios-app with ECL.
;;;;
;;;;     make ios            ; => build/iphonesimulator/Lisp Listener.app
;;;;     make run-ios        ; build, install and launch in the booted simulator
;;;;
;;;; SEPARATE FROM lisp-listener.asd for the reason lisp-listener-app.asd is:
;;;; :DEFSYSTEM-DEPENDS-ON is resolved when a .asd is READ, so the bundle here
;;;; would make asdf-ios-app a requirement for loading the library at all.
;;;;
;;;; Built with ECL, not SBCL -- asdf-ios-app cross-compiles with the host ECL
;;;; that (asdf-ios-app:bootstrap-ecl) builds, which is what `make ios-toolchain'
;;;; runs.  The simulator is the default; a device is built for only when the
;;;; environment says how to sign for one, and nobody's identity is committed.

(defsystem "lisp-listener-ios"
  :defsystem-depends-on ("asdf-ios-app")
  :class :ios-app-system
  :build-operation "ios-app-op"
  :entry-point "lisp-listener:ios-start"
  :description "A Lisp Listener in a UITextView: the REPL, the debugger and its restarts, on the phone."
  :version "0.1.0"
  :depends-on ("lisp-listener/ios")

  :bundle-identifier "org.lispnik.lisp-listener"
  :bundle-name "Lisp Listener"
  :bundle-executable "lisp-listener"
  :bundle-platforms #.(if (uiop:getenv "IOS_SIGNING_IDENTITY")
                          '(:simulator :device)
                          '(:simulator))
  :bundle-orientations (:portrait :landscape-left :landscape-right)
  :code-signing-identity #.(or (uiop:getenv "IOS_SIGNING_IDENTITY") :automatic)
  :development-team #.(uiop:getenv "IOS_DEVELOPMENT_TEAM")
  :provisioning-profile #.(uiop:getenv "IOS_PROVISIONING_PROFILE"))
