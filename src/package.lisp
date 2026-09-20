;;;; src/package.lisp
;;;;
;;;; One package.  OBJC is not :USEd: it exports INVOKE, DESCRIPTION, RELEASE
;;;; and RETAIN, and a listener is exactly the program where an accidental
;;;; capture of one of those is hardest to see.

(defpackage #:lisp-listener
  (:use #:cl)
  (:export
   ;; Entry points.
   #:main
   #:run-listener
   ;; The pieces, for a caller who wants the view somewhere else.
   #:make-listener
   #:make-listener-view
   #:make-listener-window
   #:listener
   #:listener-p
   #:listener-view
   #:listener-window
   #:listener-thread
   #:*listener*
   ;; Control.
   #:abort-evaluation
   #:clear-transcript
   #:safepoint-build-p))
