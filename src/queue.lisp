;;;; src/queue.lisp -- the character queue the input stream blocks on.
;;;;
;;;; Main thread pushes, listener thread reads.  Unbounded on purpose: the
;;;; push happens inside -insertNewline:, on thread 1, and an AppKit callback
;;;; that blocks is an application that has stopped responding.

(in-package #:lisp-listener)

(defstruct (character-queue (:constructor make-character-queue ()))
  (lock (bt:make-lock "lisp-listener input queue"))
  (available (bt:make-condition-variable :name "lisp-listener input"))
  (buffer (make-array 256 :element-type 'character :adjustable t :fill-pointer 0))
  (start 0)
  (pushback nil)
  (eof nil))

(defun %queue-empty-p (queue)
  (>= (character-queue-start queue) (fill-pointer (character-queue-buffer queue))))

(defun %queue-reset-if-empty (queue)
  (when (%queue-empty-p queue)
    (setf (fill-pointer (character-queue-buffer queue)) 0
          (character-queue-start queue) 0)))

(defun queue-push-string (queue string)
  "Make STRING readable.  Never blocks."
  (bt:with-lock-held ((character-queue-lock queue))
    (let ((buffer (character-queue-buffer queue)))
      (loop for character across string
            do (vector-push-extend character buffer)))
    (bt:condition-notify (character-queue-available queue)))
  string)

(defun queue-read-char (queue)
  "The next character, blocking until there is one.  NIL at end of file.

The wait is a loop rather than a single CONDITION-WAIT because a condition
variable may wake spuriously, and because QUEUE-WAKE notifies without having
put anything in."
  (bt:with-lock-held ((character-queue-lock queue))
    (loop
      (let ((pushback (character-queue-pushback queue)))
        (when pushback
          (setf (character-queue-pushback queue) nil)
          (return pushback)))
      (unless (%queue-empty-p queue)
        (let ((character (aref (character-queue-buffer queue)
                               (character-queue-start queue))))
          (incf (character-queue-start queue))
          (%queue-reset-if-empty queue)
          (return character)))
      (when (character-queue-eof queue)
        (return nil))
      (bt:condition-wait (character-queue-available queue)
                         (character-queue-lock queue)))))

(defun queue-unread-char (queue character)
  "Put CHARACTER back.  One character of pushback is all CL:READ needs."
  (bt:with-lock-held ((character-queue-lock queue))
    (setf (character-queue-pushback queue) character))
  character)

(defun queue-listen (queue)
  "True when QUEUE-READ-CHAR would return without blocking."
  (bt:with-lock-held ((character-queue-lock queue))
    (and (or (character-queue-pushback queue)
             (not (%queue-empty-p queue))
             (character-queue-eof queue))
         t)))

(defun queue-clear (queue)
  "Discard everything unread, and wake anyone waiting so an interrupt
delivered alongside this one is noticed promptly."
  (bt:with-lock-held ((character-queue-lock queue))
    (setf (fill-pointer (character-queue-buffer queue)) 0
          (character-queue-start queue) 0
          (character-queue-pushback queue) nil)
    (bt:condition-notify (character-queue-available queue)))
  queue)

(defun queue-set-eof (queue)
  "Make every further read return NIL.  What closing the window does."
  (bt:with-lock-held ((character-queue-lock queue))
    (setf (character-queue-eof queue) t)
    (bt:condition-notify (character-queue-available queue)))
  queue)
