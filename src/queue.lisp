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
  (eof nil)
  ;; How Interrupt reaches a listener that is WAITING rather than computing.
  ;; WAITING says a reader is parked inside the condition wait, and is only
  ;; ever written under the lock, so ABORT-EVALUATION can decide between the
  ;; two mechanisms without a race.  See QUEUE-REQUEST-ABORT-IF-WAITING.
  (waiting nil)
  (abort-requested nil))

(defvar *in-queue-wait* nil
  "True, in the reading thread, while it is inside the condition wait.

Read by the interrupt ABORT-EVALUATION sends, which must NOT abort from there.
An abort delivered inside the wait does not unwind the thread on ECL -- it is
lost -- and worse, it leaves ECL's lock bookkeeping wrong: the next unlock
signals `Attempted to give up lock ... that is not owned by process'.  A
waiting thread is woken and aborted by the flag instead, which needs no
interrupt at all.  Measured on both Lisps; see QUEUE-REQUEST-ABORT.")

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
  "The next character, blocking until there is one.  NIL at end of file, and
:ABORT when an abort has been requested since the last read began.

The wait is a loop rather than a single CONDITION-WAIT because a condition
variable may wake spuriously, and because QUEUE-WAKE notifies without having
put anything in.

:ABORT is checked FIRST, before the buffer: Interrupt discards what was typed,
so anything still here arrived after it and is not what the reader should go
on with.  Returning a marker rather than aborting here keeps the restart out
of the lowest layer; STREAM-READ-CHAR takes it."
  (bt:with-lock-held ((character-queue-lock queue))
    (loop
      (when (character-queue-abort-requested queue)
        (setf (character-queue-abort-requested queue) nil)
        (return :abort))
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
      ;; WAITING is set and cleared under the lock, which this thread holds on
      ;; either side of the wait; *IN-QUEUE-WAIT* is the same fact for the
      ;; interrupt handler, which runs in this thread and cannot take a lock.
      (setf (character-queue-waiting queue) t)
      (unwind-protect
           (let ((*in-queue-wait* t))
             (bt:condition-wait (character-queue-available queue)
                                (character-queue-lock queue)))
        (setf (character-queue-waiting queue) nil)))))

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
             (character-queue-eof queue)
             (character-queue-abort-requested queue))
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

(defun %request-abort (queue)
  "Set the flag and wake the reader.  The lock must be held, or the caller must
be the reading thread itself -- see QUEUE-REQUEST-ABORT-FROM-WAIT."
  (setf (fill-pointer (character-queue-buffer queue)) 0
        (character-queue-start queue) 0
        (character-queue-pushback queue) nil
        (character-queue-abort-requested queue) t)
  (bt:condition-notify (character-queue-available queue))
  queue)

(defun queue-request-abort-if-waiting (queue)
  "Ask a parked reader to abort.  True when there was one to ask.

This is the half of Interrupt that reaches a listener sitting in READ, and it
exists because the other half cannot: an interrupt that aborts does not
reliably unwind a thread out of a condition wait.  On ECL it does not unwind it
at all -- the abort is lost -- and it leaves the lock held-but-not-owned, so
the next unlock signals.  A flag and a notify wake the reader by the condition
variable's ordinary contract, and it aborts ITSELF from ordinary Lisp code,
which both implementations handle.

Answered under the LOCK, and WAITING is only written under the lock, so the
answer cannot go stale between the test and the flag: a reader inside the wait
cannot leave it without taking the lock this holds.  That is what keeps
ABORT-EVALUATION's choice between flag and interrupt exact -- and it is why no
flag is ever left behind for a later read to trip over.

What was typed goes with it: the form being read is abandoned."
  (bt:with-lock-held ((character-queue-lock queue))
    (when (character-queue-waiting queue)
      (%request-abort queue)
      t)))

(defun queue-request-abort-from-wait (queue)
  "Ask for an abort from inside the wait, in the reading thread itself.

For the interrupt that arrives in the sliver between ABORT-EVALUATION finding
no parked reader and this thread becoming one.  No lock is taken, and none may
be: this thread is inside the wait, which released it, and an interrupt handler
that blocked on a lock could deadlock with the thread it interrupted.  The only
other writers hold the lock, and the value written is the one this thread will
read when it wakes."
  (%request-abort queue))

(defun queue-set-eof (queue)
  "Make every further read return NIL.  What closing the window does."
  (bt:with-lock-held ((character-queue-lock queue))
    (setf (character-queue-eof queue) t)
    (bt:condition-notify (character-queue-available queue)))
  queue)
