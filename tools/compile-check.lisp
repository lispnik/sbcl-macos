;;;; tools/compile-check.lisp -- compile the listener with the frameworks stubbed.
;;;;
;;;;     sbcl --script tools/compile-check.lisp       (or: make compile-check)
;;;;
;;;; What this is for: off macOS the system cannot be LOADED at all -- lispnik/objc
;;;; opens libobjc the moment it initializes -- so the only way to find out
;;;; whether the code is even well formed is to compile it against a stand-in.
;;;; tools/stubs/stubs.lisp supplies exactly the names src/ uses and nothing
;;;; else; the compiler then reports undefined functions and variables, wrong
;;;; argument counts, and macros that will not expand.
;;;;
;;;; It cannot report a wrong selector, a wrong Cocoa argument order, or
;;;; anything at all about what the program does.  A Mac does that.
;;;;
;;;; Exits non-zero on a compiler ERROR or WARNING.  STYLE-WARNINGs are printed
;;;; and counted but do not fail the run, because a forward reference between
;;;; two files in a :SERIAL system is ordinary and not a defect.

(in-package #:cl-user)

;;; `sbcl --script' loads neither ASDF nor UIOP, and src/app.lisp reads
;;; UIOP:GETENV -- a package that does not exist is a READ error, so this has
;;; to happen before anything is compiled.  In a real build ASDF is always
;;; present; here it has to be asked for.
(handler-bind ((warning #'muffle-warning))
  (require :asdf))

(defparameter *here*
  (make-pathname :name nil :type nil :version nil
                 :defaults (or *load-truename* *default-pathname-defaults*)))
(defparameter *root*
  (make-pathname :directory (butlast (pathname-directory *here*)) :defaults *here*))
(defparameter *output*
  (merge-pathnames "build/compile-check/" *root*))

;;; The order is lisp-listener.asd's, which is :SERIAL.
(defparameter *files*
  '("package" "main-thread" "queue" "listener" "view" "streams" "restarts"
    "repl" "window" "screenshot" "app"))

(defvar *errors* 0)
(defvar *warnings* 0)
(defvar *style-warnings* 0)

(handler-bind ((warning #'muffle-warning))
  (load (merge-pathnames "tools/stubs/stubs.lisp" *root*)
        :external-format :utf-8))

(ensure-directories-exist *output*)

(dolist (name *files*)
  (let ((source (merge-pathnames (format nil "src/~a.lisp" name) *root*))
        (fasl (merge-pathnames (format nil "~a.fasl" name) *output*)))
    (format t "~&;; ~a~%" name)
    (handler-bind
        ((style-warning (lambda (condition)
                          ;; COMPILE-FILE followed by LOAD defines each macro
                          ;; twice, once at compile time and once on load.
                          ;; That is how the two-pass check works, not a defect.
                          (let ((text (princ-to-string condition)))
                            (unless (search "redefin" text)
                              (incf *style-warnings*)
                              (format t "~&  style: ~a~%" text)))
                          (muffle-warning condition)))
         (warning (lambda (condition)
                    (incf *warnings*)
                    (format t "~&  WARNING: ~a~%" condition)
                    (muffle-warning condition))))
      (handler-case
          (multiple-value-bind (output warnings-p failure-p)
              ;; :EXTERNAL-FORMAT because src/ is no longer all ASCII -- one
              ;; restart label carries an ellipsis -- and a bare COMPILE-FILE
              ;; takes its encoding from the locale, which on a CI runner is
              ;; whatever the image happens to set.  ASDF already defaults to
              ;; UTF-8, so this only brings the check into line with the build.
              (compile-file source :output-file fasl :verbose nil :print nil
                                   :external-format :utf-8)
            (declare (ignore warnings-p))
            (when failure-p (incf *errors*))
            (when output (load output)))
        (error (condition)
          (incf *errors*)
          (format t "~&  ERROR: ~a~%" condition))))))

(format t "~&~%compile-check: ~d error~:p, ~d warning~:p, ~d style warning~:p~%"
        *errors* *warnings* *style-warnings*)
(sb-ext:exit :code (if (and (zerop *errors*) (zerop *warnings*)) 0 1))
