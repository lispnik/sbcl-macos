;;;; src/restarts.lisp -- the restarts on screen: choosing instead of typing.
;;;;
;;;; WHAT LISPWORKS DOES, since this is modelled on it.  When a condition is
;;;; signalled outside the IDE's own tools, LispWorks raises a NOTIFIER window:
;;;; the condition's report at the top, then the available restarts listed one
;;;; per line in the order COMPUTE-RESTARTS returns them, and you pick one.  The
;;;; IDE's Debugger tool shows the same list in a Restarts pane beside the
;;;; backtrace.  Two things about it are worth copying and are copied here: the
;;;; list is the restarts themselves rather than a fixed set of buttons, so
;;;; whatever a handler established shows up; and picking one is the same act as
;;;; choosing it at the prompt, not a separate mechanism.
;;;;
;;;; THIS IS AN ADDITION.  The transcript still prints the numbered list and the
;;;; [1] CL-USER> prompt still takes a number, exactly as before; the panel is a
;;;; second way to reach the same thing.  Which is why it works the way it does:
;;;;
;;;;   *** THE BUTTONS TYPE FOR YOU. ***
;;;;
;;;; A button's action runs on thread 1, but a restart has to be invoked on the
;;;; listener thread, inside the dynamic extent of the debugger that established
;;;; it -- transfer control from the wrong thread and it is not that restart at
;;;; all.  The listener thread is already sitting in READ-LINE waiting for
;;;; exactly this answer, so the button pushes the number into the input queue
;;;; and the existing path does the rest.  No second mechanism, no cross-thread
;;;; control transfer, and nothing new that can deadlock.
;;;;
;;;; This file is what the two front ends share: the titles, which restart
;;;; Cancel means, and the hop to put them up and take them down.  The panel
;;;; itself is the front end's -- an NSPanel with a table in
;;;; src/macos/restarts-panel.lisp, an action sheet in src/ios/restarts-sheet.lisp
;;;; -- behind SHOW-RESTARTS-PANEL, HIDE-RESTARTS-PANEL and
;;;; RESTARTS-PANEL-VISIBLE-P.
;;;;
;;;; The panel is NOT modal, and that is also deliberate.  A modal panel would
;;;; hold thread 1 in a nested run loop while the listener thread waited for a
;;;; click -- fine in principle, and an immediate deadlock for anything driving
;;;; the listener from thread 1, which is what the screenshot script does.

(in-package #:lisp-listener)

(defparameter *restarts-panel-enabled* t
  "Whether entering the debugger also puts the restarts on screen.
NIL leaves the transcript's numbered list as the only way in, which is what it
was before this file existed.")

;;; The controller -------------------------------------------------------------

(objc:define-objc-class restarts-controller ()
  ((cancel-index :initform nil :accessor controller-cancel-index
                 :documentation "Which row Cancel takes: the index, in the rows
being shown, of the restart that returns to the listener's top level.  NIL when
it is not among them, in which case Cancel can only close the panel.")
   (listener :initform nil :accessor controller-listener
             :documentation "The listener whose panel this controller drives.

One per listener, unlike the menu bar's controller.  The panel's buttons have
to reach the listener that established the restarts, and `whichever window is
in front' would be the wrong answer: a background window is perfectly able to
be the one sitting in the debugger.")
   (titles :initform '() :accessor controller-titles
           :documentation "The rows the table is showing.

Held on the controller because a data source is asked for its rows whenever
AppKit feels like redrawing, long after the panel was built.  One debugger
level has a panel at a time, so one list is enough; HIDE-RESTARTS-PANEL
clears it."))
  (:objc-class-name "LispListenerRestartsController"))

(defun choose-restart (index)
  "Take restart INDEX.  Thread 1.

See the header: this TYPES the number rather than invoking the restart, because
the restart belongs to the listener thread and to the dynamic extent of the
debugger that established it."
  (let ((listener *listener*))
    (when (and listener (>= index 0))
      (hide-restarts-panel listener)
      (queue-push-string (listener-input listener) (format nil "~d~%" index))
      t)))

(defun restart-asks-p (restart)
  "True when invoking RESTART will stop and prompt for a value.

That is what a restart's interactive function IS -- INVOKE-RESTART-INTERACTIVELY
calls it, and SBCL's for USE-VALUE and STORE-VALUE print `Enter a form to be
evaluated: ' and READ one back.  So clicking such a row does not finish the
job, it starts a conversation in the transcript, and a trailing ellipsis is the Mac
convention for exactly that: a control that opens a prompt rather than acting.
Both the panel's rows and the transcript's numbered list are marked from here,
because they are two doors onto one list and must agree about it.

There is no portable predicate for this, so this reads an internal -- see
RESTART-INTERACTIVE-FUNCTION -- which answers NIL if it ever goes away.  Losing
an ellipsis is the whole cost of being wrong here; that is why a guarded
internal is acceptable for this and would not be for anything the behaviour
depends on."
  (and (restart-interactive-function restart) t))

(defun restart-button-title (index restart)
  (format nil "~d:   [~a]   ~a~@[~a~]"
          index
          (or (restart-name restart) "ANONYMOUS")
          (handler-case (princ-to-string restart)
            (error () "(unprintable restart)"))
          (and (restart-asks-p restart) " …")))

(defun restart-titles (restarts)
  "The button labels, computed HERE -- on the listener thread.

A restart's report may read the CURRENT thread rather than the one it was
established on.  SBCL's per-thread abort restart is exactly that: it reports as
`abort thread (#<THREAD ...>)' using SB-THREAD:*CURRENT-THREAD* at print time.
Printed from thread 1 while laying out the panel, it named the main thread and
was quietly wrong about which thread it would abort -- while the transcript,
printed on the listener thread, had it right all along.

Measured: a restart established on one thread and printed from another reports
the printing thread's name.  So the strings are made here and the panel is
handed text it cannot get wrong."
  (loop for restart in restarts
        for index from 0
        collect (restart-button-title index restart)))

(defun squeeze-whitespace (text)
  "TEXT with each run of whitespace reduced to one space, and trimmed.

A condition report is laid out over several indented lines; flattened into a
one-line heading, that indentation would survive as ragged gaps."
  (let ((out (make-string-output-stream))
        (pending nil)
        (started nil))
    (loop for character across text
          do (if (member character '(#\Space #\Tab #\Newline #\Return))
                 (when started (setf pending t))
                 (progn
                   (when pending (write-char #\Space out) (setf pending nil))
                   (write-char character out)
                   (setf started t))))
    (get-output-stream-string out)))

(defun condition-summary (condition)
  "One line for the panel's heading.  The transcript has the full report; this
only has to say which condition the buttons belong to.

Computed on the listener thread, for the same reason the titles are."
  (let ((squeezed (squeeze-whitespace (report-condition condition))))
    (format nil "~a: ~a"
            (type-of condition)
            (if (> (length squeezed) 90)
                (concatenate 'string (subseq squeezed 0 87) "...")
                squeezed))))

(defun forget-restarts (listener)
  "Clear what a panel that is going away leaves behind.  Thread 1.

The controller outlives every panel, so its rows have to go with this one: a
stale list would be answered to the next panel that asks."
  (when listener
    (setf (listener-restarts-panel listener) nil
          (listener-restarts-table listener) nil
          (listener-restarts-invoke listener) nil)
    (let ((controller (getf (listener-retained listener) :restarts-controller)))
      (when controller
        (setf (controller-titles controller) '()
              (controller-cancel-index controller) nil))))
  t)

(defun toplevel-restart-row (&optional (listener *listener*))
  "The row of the restart that returns to the listener's top level, or NIL.
Thread 1.

The panel already worked this out when it was built -- CANCEL-INDEX -- and
this is the same number under a name that says what it is for.  Anything that
wants to take that restart asks for its row rather than assuming one, because
the assumption that would be natural, zero, is wrong for the commonest error
there is: on an unbound variable SBCL puts CONTINUE, USE-VALUE and STORE-VALUE
in front of it, and row 0 is `Retry using *FOO*' -- which retries, and retries."
  (let ((controller (and listener
                         (getf (listener-retained listener) :restarts-controller))))
    (and controller (controller-cancel-index controller))))

(defun cancel-to-top-level (&optional (listener *listener*))
  "Take the restart that returns to the listener's top level, if one is on
offer.  Thread 1.  Returns whether it did."
  (let ((index (toplevel-restart-row listener)))
    (when index
      (choose-restart index)
      t)))

;;; What the debugger calls ------------------------------------------------------

(defun offer-restarts (listener condition restarts &optional backtrace cancel-index)
  "Show the restarts, from the listener thread.  Never blocks it.

:WAIT NIL, so the listener thread goes straight on to its prompt: the panel and
the prompt are two doors into the same room, and waiting for the panel would
shut the other one."
  (when (and *restarts-panel-enabled* *main-thread-target*)
    (ignore-errors
     ;; Printed HERE, on the listener thread, and handed over as text.
     (let ((heading (condition-summary condition))
           (titles (restart-titles restarts)))
       (on-main-thread ()
         (show-restarts-panel listener heading backtrace titles cancel-index)))))
  restarts)

(defun withdraw-restarts (listener)
  "Take the panel down as the debugger level exits."
  (when *main-thread-target*
    (ignore-errors
     (on-main-thread () (hide-restarts-panel listener))))
  t)
