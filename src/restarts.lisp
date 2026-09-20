;;;; src/restarts.lisp -- the restarts panel: clicking instead of typing.
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
;;;; The panel is NOT modal, and that is also deliberate.  A modal panel would
;;;; hold thread 1 in a nested run loop while the listener thread waited for a
;;;; click -- fine in principle, and an immediate deadlock for anything driving
;;;; the listener from thread 1, which is what the screenshot script does.

(in-package #:lisp-listener)

(defconstant +ns-window-style-utility+ 16
  "NSWindowStyleMaskUtilityWindow; only meaningful for an NSPanel.")

(defparameter *restarts-panel-enabled* t
  "Whether entering the debugger also puts the restarts on screen.
NIL leaves the transcript's numbered list as the only way in, which is what it
was before this file existed.")

(defparameter *restarts-panel-width* 580d0)
(defparameter *restarts-panel-margin* 14d0)
(defparameter *restarts-panel-label-height* 38d0)
(defparameter *restart-row-height* 20d0)
(defparameter *restarts-table-max-height* 132d0
  "The table scrolls, so this is a window onto the restarts, not a limit.")
(defparameter *push-button-height* 32d0)
(defparameter *push-button-width* 96d0)
(defparameter *panel-gap* 10d0)
(defparameter *backtrace-pane-height* 150d0
  "The MOST the backtrace pane grows to.  It scrolls, so this is a window onto
the frames rather than a limit on them -- but a fixed height left three frames
sitting in a mostly empty box, so BACKTRACE-PANE-HEIGHT fits the content up to
this.")

(defparameter *backtrace-line-height* 14d0)

(defun backtrace-pane-height (lines)
  (if lines
      (min *backtrace-pane-height*
           (max (* 3 *backtrace-line-height*)
                (+ 8d0 (* (length lines) *backtrace-line-height*))))
      0d0))

;;; The controller -------------------------------------------------------------

(objc:define-objc-class restarts-controller ()
  ((cancel-index :initform nil :accessor controller-cancel-index
                 :documentation "Which row Cancel takes: the index, in the rows
being shown, of the restart that returns to the listener's top level.  NIL when
it is not among them, in which case Cancel can only close the panel.")
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

;;; NSInteger is (:SIGNED :LONG-LONG).  NOT (:SIGNED :LONG): objc's CLAUDE.md
;;; records that 'l' and 'L' are 32 bits even on LP64 while NSInteger encodes
;;; as 'q', and collections.lisp's -hash and pasteboard.lisp's -draggingEntered:
;;; both spell their NSUInteger the long-long way.

(objc:define-objc-method ("numberOfRowsInTableView:" (:signed :long-long))
    ((self restarts-controller) (table objc:objc-object-pointer))
  (declare (ignorable table))
  (handler-case (length (controller-titles self))
    (error (condition) (note "numberOfRowsInTableView: ~a" condition) 0)))

(objc:define-objc-method ("tableView:viewForTableColumn:row:" objc:objc-object-pointer)
    ((self restarts-controller)
     (table objc:objc-object-pointer)
     (column objc:objc-object-pointer)
     (row (:signed :long-long)))
  (declare (ignorable table column))
  (handler-case
      (let ((titles (controller-titles self)))
        (if (and (>= row 0) (< row (length titles)))
            (make-row-view (nth row titles))
            (cffi:null-pointer)))
    (error (condition)
      (note "viewForTableColumn: ~a" condition)
      (cffi:null-pointer))))

(objc:define-objc-method ("invokeSelectedRestart:" :void)
    ((self restarts-controller) (sender objc:objc-object-pointer))
  (declare (ignorable sender))
  (handler-case
      (let* ((listener *listener*)
             (table (and listener (listener-restarts-table listener))))
        (when (and table (cffi:pointerp table) (not (cffi:null-pointer-p table)))
          (choose-restart (objc:invoke table "selectedRow"))))
    (error (condition) (note "invokeSelectedRestart: ~a" condition))))

(objc:define-objc-method ("dismissRestarts:" :void)
    ((self restarts-controller) (sender objc:objc-object-pointer))
  (declare (ignorable sender))
  ;; Cancel RETURNS TO THE TOP LEVEL, through the restart -- it does not merely
  ;; close the panel.  Closing it alone would leave the listener sitting at its
  ;; [1] prompt with the way out just taken off the screen, which is the
  ;; opposite of what a Cancel button promises.
  ;;
  ;; Through the restart, and not through an interrupt: the abort restart is
  ;; the listener's own, the reader is waiting for exactly this answer, and
  ;; taking it is the same act as typing its number.
  (handler-case
      (let ((index (controller-cancel-index self)))
        (if index
            (choose-restart index)
            (hide-restarts-panel *listener*)))
    (error (condition) (note "dismissRestarts: ~a" condition))))

(defun restart-asks-p (restart)
  "True when invoking RESTART will stop and prompt for a value.

That is what a restart's interactive function IS -- INVOKE-RESTART-INTERACTIVELY
calls it, and SBCL's for USE-VALUE and STORE-VALUE print `Enter a form to be
evaluated: ' and READ one back.  So clicking such a row does not finish the
job, it starts a conversation in the transcript, and a trailing ellipsis is the
Mac convention for exactly that: a control that opens a prompt rather than
acting.

There is no portable predicate for this, so this reads an SBCL internal behind
IGNORE-ERRORS and answers NIL if it ever goes away.  Losing an ellipsis is the
whole cost of being wrong here; that is why a guarded internal is acceptable for
this and would not be for anything the behaviour depends on."
  (and (ignore-errors (sb-kernel::restart-interactive-function restart)) t))

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

(defun make-row-view (title)
  "One row of the table, AUTORELEASED.

The autorelease is not tidiness.  This comes back from a delegate method
returning an id the caller does not own, and AppKit asks for it again on every
redraw; a +1 object here would leak one per row per repaint.  objc's
convert.lisp is explicit that an object returned from a Lisp method is the
caller's to release, and NSTableView will not."
  (let ((field (objc:invoke (objc:invoke "NSTextField" "alloc") "initWithFrame:"
                            (vector 0d0 0d0
                                    (- *restarts-panel-width*
                                       (* 2 *restarts-panel-margin*) 24d0)
                                    *restart-row-height*))))
    (objc:invoke field "setStringValue:" (or title ""))
    (objc:invoke field "setBezeled:" nil)
    (objc:invoke field "setDrawsBackground:" nil)
    (objc:invoke field "setEditable:" nil)
    (objc:invoke field "setSelectable:" nil)
    (objc:invoke field "setFont:"
                 (objc:invoke "NSFont" "monospacedSystemFontOfSize:weight:" 11d0 0d0))
    ;; NSLineBreakByTruncatingTail.  A restart's report can be far wider than
    ;; the column -- the per-thread abort one names the whole thread object --
    ;; and without this the text simply stops mid-word, which reads as a
    ;; rendering fault rather than as "there is more here".
    (objc:invoke (objc:invoke field "cell") "setLineBreakMode:" 4)
    (objc:autorelease field)))

(defun select-restart-row (table row)
  (objc:invoke table "selectRowIndexes:byExtendingSelection:"
               (objc:invoke "NSIndexSet" "indexSetWithIndex:" row)
               nil)
  table)

(defun make-restarts-table (listener count target width y height)
  "The restarts, as a list.  Main thread only.

A list rather than a column of buttons because that is what a Mac uses to
choose one thing from several, and because LispWorks' notifier is itself a
list box.  The push buttons below it are push buttons doing what push buttons
are for."
  (let* ((frame (vector *restarts-panel-margin* y
                        (- width (* 2 *restarts-panel-margin*)) height))
         (scroll (objc:invoke (objc:invoke "NSScrollView" "alloc")
                              "initWithFrame:" frame))
         (table (objc:invoke (objc:invoke "NSTableView" "alloc")
                             "initWithFrame:" frame))
         (column (objc:invoke (objc:invoke "NSTableColumn" "alloc")
                              "initWithIdentifier:" "restart")))
    (objc:invoke column "setWidth:" (- width (* 2 *restarts-panel-margin*) 24d0))
    (objc:invoke table "addTableColumn:" column)
    (objc:release column)
    ;; No header: one unnamed column of restarts needs no column title.
    (objc:invoke table "setHeaderView:" nil)
    (objc:invoke table "setRowHeight:" *restart-row-height*)
    (objc:invoke table "setUsesAlternatingRowBackgroundColors:" t)
    (objc:invoke table "setAllowsMultipleSelection:" nil)
    (objc:invoke table "setDataSource:" target)
    (objc:invoke table "setDelegate:" target)
    (objc:invoke table "setTarget:" target)
    (objc:invoke table "setDoubleAction:"
                 (objc:coerce-to-selector "invokeSelectedRestart:"))
    (objc:invoke scroll "setHasVerticalScroller:" t)
    (objc:invoke scroll "setBorderType:" 2)       ; NSBezelBorder
    (objc:invoke scroll "setDocumentView:" table)
    (objc:invoke table "reloadData")
    ;; Something selected from the start, so Invoke means something the moment
    ;; the panel appears.
    (when (plusp count) (select-restart-row table 0))
    (setf (listener-restarts-table listener) table)
    ;; -setDocumentView: retains it; the +1 from -alloc is ours to drop.
    (objc:release table)
    scroll))

(defun make-push-button (title selector target x y key)
  "An ordinary push button at its natural height, which is what the bezel
style is designed for."
  (let ((button (objc:invoke (objc:invoke "NSButton" "alloc") "initWithFrame:"
                             (vector x y *push-button-width* *push-button-height*))))
    (objc:invoke button "setTitle:" title)
    (objc:invoke button "setBezelStyle:" 1)
    (objc:invoke button "setTarget:" target)
    (objc:invoke button "setAction:" (objc:coerce-to-selector selector))
    (when key (objc:invoke button "setKeyEquivalent:" key))
    button))

(defun make-condition-label (text width y)
  (let ((field (objc:invoke (objc:invoke "NSTextField" "alloc") "initWithFrame:"
                            (vector *restarts-panel-margin* y
                                    (- width (* 2 *restarts-panel-margin*))
                                    *restarts-panel-label-height*))))
    (objc:invoke field "setStringValue:" text)
    (objc:invoke field "setBezeled:" nil)
    (objc:invoke field "setDrawsBackground:" nil)
    (objc:invoke field "setEditable:" nil)
    (objc:invoke field "setSelectable:" t)
    field))

(defun make-backtrace-pane (lines width y height)
  "A read-only scrolling view of the frames.  Main thread only.

The LispWorks Debugger tool puts a backtrace beside its restarts, which is the
arrangement this borrows: the restarts say what you can do, the frames say
where you are, and neither is much use without the other."
  (let* ((frame (vector *restarts-panel-margin* y
                        (- width (* 2 *restarts-panel-margin*)) height))
         (scroll (objc:invoke (objc:invoke "NSScrollView" "alloc")
                              "initWithFrame:" frame))
         (text (objc:invoke (objc:invoke "NSTextView" "alloc")
                            "initWithFrame:" frame)))
    (objc:invoke scroll "setHasVerticalScroller:" t)
    (objc:invoke scroll "setBorderType:" 2)       ; NSBezelBorder
    (objc:invoke text "setEditable:" nil)
    (objc:invoke text "setRichText:" nil)
    (objc:invoke text "setFont:"
                 (objc:invoke "NSFont" "monospacedSystemFontOfSize:weight:" 10d0 0d0))
    (objc:invoke text "setString:" (format nil "~{~a~%~}" lines))
    (objc:invoke scroll "setDocumentView:" text)
    ;; -setDocumentView: retains it; the +1 from -alloc is ours to drop.
    (objc:release text)
    scroll))

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

(defun build-restarts-panel (listener heading backtrace titles cancel-index)
  "A floating panel: the condition, the frames, the restarts as a list, and
the two push buttons that act on the selection.  Main thread only.

Takes finished strings rather than the restarts themselves: see
RESTART-TITLES for why they cannot be printed here."
  (let* ((count (length titles))
         (width *restarts-panel-width*)
         (table-height (min *restarts-table-max-height*
                            (max (* 2 *restart-row-height*)
                                 (+ 4d0 (* count *restart-row-height*)))))
         (pane-height (backtrace-pane-height backtrace))
         (pane-gap (if backtrace *panel-gap* 0d0))
         (height (+ (* 2 *restarts-panel-margin*)
                    *restarts-panel-label-height*
                    pane-height pane-gap
                    table-height *panel-gap*
                    *push-button-height*))
         (panel (objc:invoke (objc:invoke "NSPanel" "alloc")
                             "initWithContentRect:styleMask:backing:defer:"
                             (vector 0d0 0d0 width height)
                             (logior +ns-window-style-titled+
                                     +ns-window-style-closable+
                                     +ns-window-style-utility+)
                             +ns-backing-store-buffered+ nil))
         (controller (getf (listener-retained listener) :restarts-controller))
         (target (objc:objc-object-pointer controller))
         (content (objc:invoke panel "contentView")))
    (setf (controller-titles controller) titles
          (controller-cancel-index controller) cancel-index)
    (objc:invoke panel "setReleasedWhenClosed:" nil)
    (objc:invoke panel "setTitle:" "Restarts")
    (objc:invoke panel "setFloatingPanel:" t)
    (objc:invoke panel "setHidesOnDeactivate:" nil)
    ;; An NSView's origin is bottom left, so this lays out downwards from the
    ;; top: heading, frames, the list, and the buttons along the bottom.
    (let ((y (- height *restarts-panel-margin* *restarts-panel-label-height*)))
      (let ((label (make-condition-label heading width y)))
        (objc:invoke content "addSubview:" label)
        (objc:release label))
      (when backtrace
        (decf y pane-height)
        (let ((pane (make-backtrace-pane backtrace width y pane-height)))
          (objc:invoke content "addSubview:" pane)
          (objc:release pane))
        (decf y pane-gap))
      (decf y table-height)
      (let ((table (make-restarts-table listener count target width y table-height)))
        (objc:invoke content "addSubview:" table)
        (objc:release table)))
    ;; Cancel and Invoke, bottom right, Invoke rightmost as a Mac puts it.
    (let* ((invoke-x (- width *restarts-panel-margin* *push-button-width*))
           (cancel-x (- invoke-x *push-button-width* 4d0))
           (cancel (make-push-button "Cancel" "dismissRestarts:" target
                                     cancel-x *restarts-panel-margin*
                                     (string #\Escape)))
           (invoke (make-push-button "Invoke" "invokeSelectedRestart:" target
                                     invoke-x *restarts-panel-margin*
                                     (string #\Return))))
      (objc:invoke content "addSubview:" cancel)
      (objc:invoke content "addSubview:" invoke)
      (setf (listener-restarts-invoke listener) invoke)
      (objc:release cancel)
      (objc:release invoke))
    panel))

(defun position-restarts-panel (listener panel)
  "Put the panel over the listener window, near its top left."
  (handler-case
      (let ((frame (objc:invoke (listener-window listener) "frame")))
        (objc:invoke panel "setFrameTopLeftPoint:"
                     (vector (+ (aref frame 0) 48d0)
                             (- (+ (aref frame 1) (aref frame 3)) 48d0))))
    (error () (objc:invoke panel "center")))
  panel)

;;; Showing and hiding ----------------------------------------------------------

(defun show-restarts-panel (listener heading backtrace titles cancel-index)
  "Put TITLES on screen under HEADING and the frames.  Thread 1."
  (hide-restarts-panel listener)
  (let ((panel (build-restarts-panel listener heading backtrace titles
                                     cancel-index)))
    (setf (listener-restarts-panel listener) panel)
    (position-restarts-panel listener panel)
    ;; -orderFront: rather than -makeKeyAndOrderFront:.  The listener window
    ;; keeps the keyboard, so typing the number still works while the panel is
    ;; up -- which is what makes this an addition rather than a replacement.
    (objc:invoke panel "orderFront:" nil)
    panel))

(defun hide-restarts-panel (&optional (listener *listener*))
  "Take the panel down, if there is one.  Main thread only.  Idempotent."
  (let ((panel (and listener (listener-restarts-panel listener))))
    (when (and panel (cffi:pointerp panel) (not (cffi:null-pointer-p panel)))
      (objc:invoke panel "orderOut:" nil))
    (when listener
      (setf (listener-restarts-panel listener) nil
            (listener-restarts-table listener) nil
            (listener-restarts-invoke listener) nil)
      ;; The controller outlives every panel, so its rows have to go with this
      ;; one: a stale list would be answered to the next table that asks.
      (let ((controller (getf (listener-retained listener) :restarts-controller)))
        (when controller
          (setf (controller-titles controller) '()
                (controller-cancel-index controller) nil)))))
  t)

(defun click-restart (&optional (index 0) (listener *listener*))
  "Select restart INDEX and press Invoke, exactly as a person would.  Thread 1.

Selecting and then -performClick:ing the button drives the real path -- the
selection, the target, the action, the queued number -- rather than reaching
past it to CHOOSE-RESTART, which would prove only that CHOOSE-RESTART works.
Returns whether there was a table and a button to use."
  (let ((table (and listener (listener-restarts-table listener)))
        (button (and listener (listener-restarts-invoke listener))))
    (when (and table (cffi:pointerp table) (not (cffi:null-pointer-p table))
               button (cffi:pointerp button) (not (cffi:null-pointer-p button)))
      (select-restart-row table index)
      (objc:invoke button "performClick:" nil)
      t)))

(defun cancel-to-top-level (&optional (listener *listener*))
  "Take the restart that returns to the listener's top level, if one is on
offer.  Thread 1.  Returns whether it did."
  (let* ((controller (and listener
                          (getf (listener-retained listener) :restarts-controller)))
         (index (and controller (controller-cancel-index controller))))
    (when index
      (choose-restart index)
      t)))

;;; Escape, from the listener window itself.
;;;
;;; These are methods on the TEXT VIEW but they live here, with the rest of the
;;; restart behaviour, and because view.lisp loads first a definition there
;;; would be a forward reference to everything above.
;;;
;;; Both selectors, and the second is the one that does the work.  Escape is
;;; NSResponder's -cancelOperation: in most controls, which is why that one is
;;; here at all -- but inside an NSTextView the standard key bindings send
;;; Escape to -complete:, the word-completion action.  Overriding only the
;;; conventional one would have looked right and done nothing.
;;;
;;; Neither changes anything unless a restarts panel is up: with no panel they
;;; both go to super, so completion still behaves as it always did.

(define-listener-method ("cancelOperation:" :void)
    ((sender objc:objc-object-pointer))
  (unless (cancel-to-top-level)
    (objc:invoke (objc:current-super) "cancelOperation:" sender)))

(define-listener-method ("complete:" :void)
    ((sender objc:objc-object-pointer))
  (unless (cancel-to-top-level)
    (objc:invoke (objc:current-super) "complete:" sender)))

(defun click-cancel (&optional (listener *listener*))
  "Press Cancel, exactly as a person would.  Thread 1.

Found among the panel's subviews by title, because unlike Invoke it is not
worth a slot on the listener just so a test can reach it."
  (let ((panel (and listener (listener-restarts-panel listener))))
    (when (and panel (cffi:pointerp panel) (not (cffi:null-pointer-p panel)))
      (let* ((subviews (objc:invoke (objc:invoke panel "contentView") "subviews"))
             (count (objc:invoke subviews "count"))
             (button-class (objc:coerce-to-objc-class "NSButton")))
        (loop for i from 0 below count
              for view = (objc:invoke subviews "objectAtIndex:" i)
              when (and (objc:invoke-bool view "isKindOfClass:" button-class)
                        (string= "Cancel"
                                 (objc:ns-string-to-string
                                  (objc:invoke view "title"))))
                do (objc:invoke view "performClick:" nil)
                   (return t))))))

(defun restarts-table-row-count (&optional (listener *listener*))
  "How many rows the table believes it has.  Thread 1.

Asked from outside so that CI can check the data source was actually consulted:
a table that renders blank still answers this, and a table whose delegate was
never found answers zero."
  (let ((table (and listener (listener-restarts-table listener))))
    (when (and table (cffi:pointerp table) (not (cffi:null-pointer-p table)))
      (objc:invoke table "numberOfRows"))))

(defun restarts-panel-visible-p (&optional (listener *listener*))
  (let ((panel (and listener (listener-restarts-panel listener))))
    (and panel (cffi:pointerp panel) (not (cffi:null-pointer-p panel))
         (objc:invoke-bool panel "isVisible"))))

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
