;;;; src/streams.lisp -- the listener's standard streams.
;;;;
;;;; Gray streams, so that everything an ordinary REPL binds -- *STANDARD-
;;;; OUTPUT*, *STANDARD-INPUT*, *QUERY-IO* -- can be the window.  That is not
;;;; decoration: it is what makes (read-line) in your own code read from the
;;;; window, and what lets INVOKE-RESTART-INTERACTIVELY ask its question there
;;;; when you pick a restart that needs a value.
;;;;
;;;; SB-GRAY rather than trivial-gray-streams: this repository is sbcl-macos.
;;;;
;;;; ONE RULE, and the deadlock is real if it is broken: the buffer lock is
;;;; never held across a hop to the main thread, and the hop is never made with
;;;; :WAIT T.  The flush closure runs on thread 1 and takes the same lock; a
;;;; writer that waited for it while holding it would stop both threads.

(in-package #:lisp-listener)

;;; Output --------------------------------------------------------------------

(defclass listener-output-stream (sb-gray:fundamental-character-output-stream)
  ((listener :initarg :listener :reader stream-listener)
   (lock :initform (bt:make-lock "lisp-listener output") :reader stream-lock)
   (segments :initform '() :accessor stream-segments
             :documentation "Pending (KIND . TEXT) runs, newest first.")
   (column :initform 0 :accessor stream-column)
   (kind :initform :output :accessor stream-kind
         :documentation "What the next write is.  Listener thread only.")
   (flush-scheduled :initform nil :accessor stream-flush-scheduled))
  (:documentation "A character stream whose output lands in the transcript.

Buffered and coalesced: a thousand WRITE-CHARs become one hop to the main
thread, because the hop is a -performSelectorOnMainThread: and a REPL that made
one per character would spend its life in the run loop."))

(defun %stream-emit (stream text)
  (when (plusp (length text))
    (bt:with-lock-held ((stream-lock stream))
      (let ((kind (stream-kind stream))
            (segments (stream-segments stream)))
        (if (and segments (eq (car (first segments)) kind))
            (setf (cdr (first segments))
                  (concatenate 'string (cdr (first segments)) text))
            (push (cons kind (copy-seq text)) (stream-segments stream))))
      ;; STREAM-LINE-COLUMN is what makes FRESH-LINE, ~& and the pretty printer
      ;; behave.  Without it every one of them emits a newline unconditionally.
      (let ((newline (position #\Newline text :from-end t)))
        (setf (stream-column stream)
              (if newline
                  (- (length text) newline 1)
                  (+ (stream-column stream) (length text))))))
    (schedule-flush stream))
  text)

(defun schedule-flush (stream)
  "Ask the main thread to move the buffer into the transcript, once.

The flag is set under the lock and the hop is made after releasing it, and it
is only ever set when a hop really follows -- setting it and then declining to
schedule would wedge the stream, since the next writer would see a flush
already pending and do nothing."
  (cond
    ;; Before the window exists there is nowhere to put the text.  Leave it in
    ;; the buffer; MAIN flushes once there is a view, so the banner survives.
    ((null *main-thread-target*) nil)
    ((objc.runloop:main-thread-p) (flush-transcript stream))
    (t
     (let ((schedule nil))
       (bt:with-lock-held ((stream-lock stream))
         (unless (stream-flush-scheduled stream)
           (setf (stream-flush-scheduled stream) t
                 schedule t)))
       (when schedule
         (on-main-thread () (flush-transcript stream))))))
  stream)

(defun flush-transcript (stream)
  "Move everything buffered into the transcript.  Main thread only."
  (let ((segments (bt:with-lock-held ((stream-lock stream))
                    (setf (stream-flush-scheduled stream) nil)
                    (prog1 (nreverse (stream-segments stream))
                      (setf (stream-segments stream) '())))))
    (when segments
      (let* ((listener (stream-listener stream))
             (view (listener-view-object listener))
             (pointer (listener-view listener)))
        (cond
          (pointer
           (dolist (segment segments)
             (transcript-insert view (cdr segment) (car segment)))
           ;; -scrollRangeToVisible: and NOT SCROLL-TO-END, which also moves the
           ;; selection.  Output lands while the user is typing, and yanking the
           ;; caret to the end of the transcript on every flush would make the
           ;; window unusable during anything that prints.
           (objc:invoke pointer "scrollRangeToVisible:"
                        (cons (transcript-length pointer) 0)))
          (t
           ;; Unreachable while SCHEDULE-FLUSH declines to schedule without a
           ;; target -- but the segments are already out of the buffer, and
           ;; losing what someone printed is not an acceptable way to be wrong.
           (bt:with-lock-held ((stream-lock stream))
             (setf (stream-segments stream)
                   (append (reverse segments) (stream-segments stream)))))))))
  nil)

(defmethod sb-gray:stream-write-char ((stream listener-output-stream) character)
  (%stream-emit stream (string character))
  character)

(defmethod sb-gray:stream-write-string ((stream listener-output-stream) string
                                        &optional (start 0) end)
  (%stream-emit stream (subseq string start end))
  string)

(defmethod sb-gray:stream-line-column ((stream listener-output-stream))
  (stream-column stream))

(defparameter *transcript-line-length* 100
  "How wide the pretty printer may assume the transcript is.

Without STREAM-LINE-LENGTH the printer has to guess, and it guesses narrow: a
TYPE-ERROR's report came out as five ragged lines -- `The value', `7', `is not
of type', `LIST' -- which reads like a bug in the listener rather than a
sentence.  The window is about 108 monospaced columns at its default size.")

(defmethod sb-gray:stream-line-length ((stream listener-output-stream))
  *transcript-line-length*)

(defmethod sb-gray:stream-force-output ((stream listener-output-stream))
  (schedule-flush stream)
  nil)

(defmethod sb-gray:stream-finish-output ((stream listener-output-stream))
  ;; Scheduled, not waited for.  FINISH-OUTPUT's contract is that the data has
  ;; been handed on, and waiting for thread 1 here is the deadlock this file's
  ;; header is about.
  (schedule-flush stream)
  nil)

(defmacro with-output-kind ((stream kind) &body body)
  "Write BODY's output as KIND.  Listener thread only: STREAM-KIND is not
locked, because only one thread ever sets it."
  (let ((s (gensym "STREAM")) (saved (gensym "SAVED")))
    `(let* ((,s ,stream)
            (,saved (stream-kind ,s)))
       (unwind-protect (progn (setf (stream-kind ,s) ,kind) ,@body)
         (setf (stream-kind ,s) ,saved)
         (force-output ,s)))))

;;; Input ---------------------------------------------------------------------

(defclass listener-input-stream (sb-gray:fundamental-character-input-stream)
  ((listener :initarg :listener :reader stream-listener))
  (:documentation "A character stream fed by the view, one submitted line at a
time.  READ blocks on it, which is exactly what makes an incomplete form work:
no new prompt appears and the user carries on typing."))

(defmethod sb-gray:stream-read-char ((stream listener-input-stream))
  (or (queue-read-char (listener-input (stream-listener stream)))
      :eof))

(defmethod sb-gray:stream-unread-char ((stream listener-input-stream) character)
  (queue-unread-char (listener-input (stream-listener stream)) character)
  nil)

(defmethod sb-gray:stream-listen ((stream listener-input-stream))
  (queue-listen (listener-input (stream-listener stream))))

(defmethod sb-gray:stream-read-char-no-hang ((stream listener-input-stream))
  (when (queue-listen (listener-input (stream-listener stream)))
    (sb-gray:stream-read-char stream)))

(defmethod sb-gray:stream-clear-input ((stream listener-input-stream))
  (queue-clear (listener-input (stream-listener stream)))
  nil)

;;; Making them ---------------------------------------------------------------

(defun make-listener ()
  "A listener with its queues and streams, and no Cocoa in it yet.

Run time only.  The streams are ordinary CLOS objects and would survive a dump,
but the view pointer one of them ends up holding would not."
  (let ((listener (%make-listener)))
    (setf (listener-output listener)
          (make-instance 'listener-output-stream :listener listener))
    listener))

(defun listener-input-stream-for (listener)
  (make-instance 'listener-input-stream :listener listener))
