;;;; tools/syntax-check.lisp -- read every source file; evaluate nothing.
;;;;
;;;;     sbcl --script tools/syntax-check.lisp        (or: make check)
;;;;
;;;; The point is that this runs ANYWHERE, including on a machine with no
;;;; Objective-C runtime, where the system itself cannot be loaded at all.  So
;;;; it uses nothing but the host: no ASDF, no UIOP -- `sbcl --script' loads
;;;; neither, and reaching for UIOP here failed exactly that way once.
;;;;
;;;; *READ-SUPPRESS* is what makes the check possible.  With it bound, the
;;;; reader parses structure only: no package is consulted, no symbol is
;;;; interned and nothing is evaluated, so `objc:invoke' and
;;;; `sb-gray:stream-write-char' cost nothing while an unbalanced parenthesis,
;;;; a bad reader macro or a stray character still signals.  A plain READ
;;;; cannot do this job -- a single-colon reference to a symbol that some stub
;;;; package has not exported signals long before it reaches the parenthesis
;;;; you actually got wrong.
;;;;
;;;; It is a parse check and nothing more.  It will not find an undefined
;;;; function, a wrong argument count, or a typo in a selector.

(in-package #:cl-user)

(defparameter *here*
  (make-pathname :name nil :type nil :version nil
                 :defaults (or *load-truename* *default-pathname-defaults*)))

(defparameter *root*
  (make-pathname :directory (butlast (pathname-directory *here*))
                 :defaults *here*))

(defun sorted-files (pattern)
  (sort (directory (merge-pathnames pattern *root*) :resolve-symlinks nil)
        #'string< :key #'namestring))

(defun check-file (path)
  (handler-case
      (with-open-file (stream path :external-format :utf-8)
        (let ((*read-suppress* t))
          (loop for form = (read stream nil :eof) until (eq form :eof))
          (format t "~&  ok    ~a~%" (enough-namestring path *root*))
          t))
    (error (condition)
      (format t "~&  FAIL  ~a~%        ~a~%" (enough-namestring path *root*) condition)
      nil)))

(let ((files (append (sorted-files "src/*.lisp")
                     (sorted-files "src/macos/*.lisp")
                     (sorted-files "src/ios/*.lisp")
                     (sorted-files "tools/*.lisp")
                     (sorted-files "*.asd"))))
  ;; "No offenders" is true of an empty scan, so the count is what makes the
  ;; result mean anything.  Check the checker by breaking a file on purpose.
  (when (< (length files) 10)
    (format t "~&syntax-check: only ~d file~:p found under ~a -- that is not the ~
whole repository.~%" (length files) *root*)
    (sb-ext:exit :code 2))
  (format t "~&syntax-check: ~d files under ~a~%" (length files) *root*)
  (let ((failures (count nil (mapcar #'check-file files))))
    (format t "~&syntax-check: ~[all clean~:;~:*~d failure~:p~]~%" failures)
    (sb-ext:exit :code (if (zerop failures) 0 1))))
