;;;; src/history.lisp -- the history, kept between launches.
;;;;
;;;; ONE history for the application, not one per window.  A window has no
;;;; identity that survives being closed, so "this window's history" could not
;;;; be found again at the next launch; a shell keeps one list per user and that
;;;; is what this is.  Two listener windows therefore load the same lines and
;;;; both append to the same file, newest last.
;;;;
;;;; Written AS EACH LINE IS SUBMITTED rather than when the application quits.
;;;; iOS does not promise to tell an app it is going away -- it may simply be
;;;; killed -- so anything saved at the end is the part most likely to be lost.
;;;;
;;;; One printed Lisp string per line.  A submitted form can be several lines
;;;; long, so the lines cannot be the records themselves; PRIN1 of a string
;;;; escapes the newlines and READ gives it back exactly, which is the whole
;;;; format, and it stays readable in a text editor.
;;;;
;;;; NOTHING HERE MAY SIGNAL.  The history is a convenience, and a listener that
;;;; would not start because a file is unreadable -- or that stopped submitting
;;;; forms because a disk filled up -- would be a bad trade.  Every path is
;;;; wrapped, and a failure costs the history and nothing else.

(in-package #:lisp-listener)

(defparameter *history-limit* 500
  "How many lines to keep.  A listener is not a log: this is enough to find
yesterday's form and small enough to read and to rewrite in one go.")

(defvar *history-lock* (bt:make-lock "lisp-listener history")
  "Held while the file is read or written.  Thread 1 does all of it today --
submitting is a view method -- but two listeners share the file, and a lock
costs nothing next to the write it guards.")

(defvar *history-directory* nil
  "Where to keep the history, instead of the front end's own answer.
For the test, which must not write into anybody's Library folder.")

(defun history-file ()
  "Where the history lives, or NIL if the front end has nowhere to put it."
  (ignore-errors
   (let ((directory (or *history-directory* (history-directory))))
     (when directory
       (merge-pathnames "history.lisp-expr" directory)))))

(defun read-history-file (path)
  "Every line in PATH, oldest first.  A damaged line ends the read: the rest of
the file is whatever a half-written record left behind, and guessing at it is
worse than losing it."
  (with-open-file (stream path :external-format :utf-8 :if-does-not-exist nil)
    (when stream
      (loop for line = (handler-case (read stream nil nil)
                         (error () nil))
            while (stringp line)
            collect line))))

(defun write-history-file (path lines)
  "Replace PATH with LINES, oldest first."
  (ensure-directories-exist path)
  (with-open-file (stream path :direction :output :external-format :utf-8
                               :if-exists :supersede
                               :if-does-not-exist :create)
    (dolist (line lines)
      (prin1 line stream)
      (terpri stream)))
  lines)

(defun load-history ()
  "The saved history, NEWEST FIRST, which is the order the view keeps.

Trims the file when it has grown past the limit, which is the only time it is
rewritten: appending is what every other write does."
  (or (ignore-errors
       (let ((path (history-file)))
         (when path
           (let* ((lines (read-history-file path))
                  (kept (if (> (length lines) *history-limit*)
                            (last lines *history-limit*)
                            lines)))
             (bt:with-lock-held (*history-lock*)
               (when (> (length lines) (length kept))
                 (ignore-errors (write-history-file path kept))))
             (reverse kept)))))
      '()))

(defun record-history-line (line)
  "Append LINE to the file.  Returns LINE, saved or not."
  (ignore-errors
   (let ((path (history-file)))
     (when (and path (plusp (length line)))
       (bt:with-lock-held (*history-lock*)
         (ensure-directories-exist path)
         (with-open-file (stream path :direction :output :external-format :utf-8
                                      :if-exists :append
                                      :if-does-not-exist :create)
           (prin1 line stream)
           (terpri stream))))))
  line)

(defun initialize-view-history (view)
  "Give VIEW the saved history.  Called as each view is made."
  (setf (view-history view) (load-history)
        (view-history-index view) nil)
  view)
