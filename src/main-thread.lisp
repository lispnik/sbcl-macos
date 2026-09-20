;;;; src/main-thread.lisp -- getting work onto thread 1.
;;;;
;;;; AppKit belongs to the main thread and the listener runs on another.  The
;;;; listener only ever touches Lisp state; the things that must reach AppKit --
;;;; appending to the transcript, scrolling, moving the input marker -- go
;;;; through here: a queue of closures drained by an Objective-C method on the
;;;; view, which -performSelectorOnMainThread: delivers.
;;;;
;;;; This is lem-cocoa's main-thread.lisp, which is the worked version of the
;;;; pattern, including the one measurement that is easy to get wrong -- see
;;;; +COMMON-RUN-LOOP-MODES+.

(in-package #:lisp-listener)

(defparameter +common-run-loop-modes+
  #("NSDefaultRunLoopMode" "NSEventTrackingRunLoopMode" "NSModalPanelRunLoopMode")
  "The run loop modes a queued selector is delivered in: the default one, and
the ones the main thread runs while a window is being resized or a panel is up,
so a request made then still arrives.

Named one by one rather than as the common-modes pseudo mode.  Measured in
lem-cocoa: a perform queued in the mode named kCFRunLoopCommonModes from a Lisp
thread never ran, and every perform queued after it stayed behind it.

A Lisp vector; INVOKE converts it to an NSArray of NSStrings on the way in.")

(defvar *main-thread-queue* '()
  "Closures waiting to run on the main thread, newest first.")

(defvar *main-thread-queue-lock* (bt:make-lock "lisp-listener main-thread queue"))

(defvar *main-thread-target* nil
  "The Objective-C object whose -listenerDrainQueue runs the queue: the view.
NIL until MAKE-LISTENER-WINDOW has made one, which is why the listener thread
is started last.")

(defvar *log* nil
  "Where to report something that went wrong in a place that cannot be told
about it -- an Objective-C callback, or a main-thread closure.

Captured in MAIN from the *STANDARD-OUTPUT* asdf-macos-app binds to the
bundle's log file, because by the time one of these fails the transcript is
quite possibly the thing that failed.")

(defun note (control &rest arguments)
  "Report CONTROL to the log, never signalling and never reaching the
transcript."
  (ignore-errors
   (let ((stream (or *log* *error-output*)))
     (format stream "~&lisp-listener: ~?~%" control arguments)
     (finish-output stream)))
  nil)

(defun drain-main-thread-queue ()
  "Run every queued closure, oldest first.  Main thread only.

The lock is held only long enough to take the list.  Holding it across the
closures would deadlock the first time one of them wanted to queue more work."
  (let ((thunks (bt:with-lock-held (*main-thread-queue-lock*)
                  (prog1 (nreverse *main-thread-queue*)
                    (setf *main-thread-queue* '())))))
    (dolist (thunk thunks)
      (handler-case (funcall thunk)
        (error (condition)
          (note "main thread: ~a" condition))))
    (length thunks)))

(defun %perform-drain (wait)
  (objc:invoke *main-thread-target*
               "performSelectorOnMainThread:withObject:waitUntilDone:modes:"
               (objc:coerce-to-selector "listenerDrainQueue")
               nil wait +common-run-loop-modes+))

(defun call-on-main-thread (function &key wait)
  "Run FUNCTION on the main thread.  With WAIT, block until it has run and
return its values; without -- the default, and what everything here uses --
return at once.

WAIT defaults to NIL deliberately.  A synchronous hop only lands if the main
thread is already in a run loop in one of +COMMON-RUN-LOOP-MODES+; during
startup, or while the main thread is inside Lisp of our own, it would wait
with no error and nothing to see.

On the main thread already FUNCTION is simply called.  That branch is what
makes this safe to call from inside an Objective-C callback, which is where
half the callers are."
  (cond
    ((objc.runloop:main-thread-p)
     (funcall function))
    ((null *main-thread-target*)
     (error "lisp-listener: no main-thread target yet; the window has not been made."))
    (wait
     (let ((values-list nil)
           (condition nil))
       (bt:with-lock-held (*main-thread-queue-lock*)
         (push (lambda ()
                 (handler-case
                     (setf values-list (multiple-value-list (funcall function)))
                   (error (c) (setf condition c))))
               *main-thread-queue*))
       (%perform-drain t)
       (when condition (error condition))
       (values-list values-list)))
    (t
     (bt:with-lock-held (*main-thread-queue-lock*)
       (push function *main-thread-queue*))
     (%perform-drain nil)
     (values))))

(defmacro on-main-thread ((&key wait) &body body)
  "Run BODY on the main thread.  See CALL-ON-MAIN-THREAD."
  `(call-on-main-thread (lambda () ,@body) :wait ,wait))
