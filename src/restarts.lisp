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
(defparameter *restart-button-height* 24d0)
(defparameter *restart-button-gap* 4d0)
(defparameter *restarts-panel-margin* 14d0)
(defparameter *restarts-panel-label-height* 38d0)

;;; The controller -------------------------------------------------------------

(objc:define-objc-class restarts-controller ()
  ()
  (:objc-class-name "LispListenerRestartsController"))

(objc:define-objc-method ("restartChosen:" :void)
    ((self restarts-controller) (sender objc:objc-object-pointer))
  (declare (ignorable sender))
  (handler-case
      (let ((index (objc:invoke sender "tag"))
            (listener *listener*))
        (when listener
          (hide-restarts-panel listener)
          ;; See the header: the button types the number rather than invoking
          ;; the restart, because the restart belongs to the listener thread.
          (queue-push-string (listener-input listener) (format nil "~d~%" index))))
    (error (condition) (note "restartChosen: ~a" condition))))

;;; Building it ----------------------------------------------------------------

(defun restart-button-title (index restart)
  (format nil "~d:   [~a]   ~a"
          index
          (or (restart-name restart) "ANONYMOUS")
          (handler-case (princ-to-string restart)
            (error () "(unprintable restart)"))))

(defun make-restart-button (title index target width y)
  "One restart, as a button that knows its own index through -tag."
  (let ((button (objc:invoke (objc:invoke "NSButton" "alloc") "initWithFrame:"
                             (vector *restarts-panel-margin* y
                                     (- width (* 2 *restarts-panel-margin*))
                                     *restart-button-height*))))
    (objc:invoke button "setTitle:" title)
    (objc:invoke button "setBezelStyle:" 1)
    (objc:invoke button "setTag:" index)
    (objc:invoke button "setTarget:" target)
    (objc:invoke button "setAction:" (objc:coerce-to-selector "restartChosen:"))
    (objc:invoke button "setFont:"
                 (objc:invoke "NSFont" "monospacedSystemFontOfSize:weight:" 11d0 0d0))
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

(defun condition-summary (condition)
  "One line for the panel's heading.  The transcript has the full report; this
only has to say which condition the buttons belong to."
  (let* ((text (substitute #\Space #\Newline (report-condition condition)))
         (squeezed (string-trim " " text)))
    (format nil "~a: ~a"
            (type-of condition)
            (if (> (length squeezed) 90)
                (concatenate 'string (subseq squeezed 0 87) "...")
                squeezed))))

(defun build-restarts-panel (listener condition restarts)
  "A floating panel with one button per restart.  Main thread only."
  (let* ((count (length restarts))
         (width *restarts-panel-width*)
         (row (+ *restart-button-height* *restart-button-gap*))
         (height (+ (* 2 *restarts-panel-margin*)
                    *restarts-panel-label-height*
                    (* count row)))
         (panel (objc:invoke (objc:invoke "NSPanel" "alloc")
                             "initWithContentRect:styleMask:backing:defer:"
                             (vector 0d0 0d0 width height)
                             (logior +ns-window-style-titled+
                                     +ns-window-style-closable+
                                     +ns-window-style-utility+)
                             +ns-backing-store-buffered+ nil))
         (target (objc:objc-object-pointer
                  (getf (listener-retained listener) :restarts-controller)))
         (content (objc:invoke panel "contentView")))
    (objc:invoke panel "setReleasedWhenClosed:" nil)
    (objc:invoke panel "setTitle:" "Restarts")
    (objc:invoke panel "setFloatingPanel:" t)
    (objc:invoke panel "setHidesOnDeactivate:" nil)
    ;; An NSView's origin is bottom left, so the first restart -- the one
    ;; COMPUTE-RESTARTS considers nearest -- is laid out highest.
    (let ((label (make-condition-label
                  (condition-summary condition) width
                  (- height *restarts-panel-margin* *restarts-panel-label-height*))))
      (objc:invoke content "addSubview:" label)
      (objc:release label))
    (loop for restart in restarts
          for index from 0
          for y = (- height *restarts-panel-margin* *restarts-panel-label-height*
                     (* (1+ index) row))
          do (let ((button (make-restart-button (restart-button-title index restart)
                                                index target width y)))
               (objc:invoke content "addSubview:" button)
               ;; -addSubview: retains; the +1 from -alloc is ours to drop.
               (objc:release button)))
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

(defun show-restarts-panel (listener condition restarts)
  "Put RESTARTS on screen as buttons.  Main thread only."
  (hide-restarts-panel listener)
  (let ((panel (build-restarts-panel listener condition restarts)))
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
      (objc:invoke panel "orderOut:" nil)
      (setf (listener-restarts-panel listener) nil)))
  t)

(defun click-restart (&optional (index 0) (listener *listener*))
  "Press the button for restart INDEX, exactly as a click would.  Thread 1 only.

-performClick: runs the button's action through AppKit, so this exercises the
real path -- the target, the tag, the queued number -- rather than reaching
past it and calling RESTARTCHOSEN: directly.  Returns whether a button with
that tag was found."
  (let ((panel (and listener (listener-restarts-panel listener))))
    (when (and panel (cffi:pointerp panel) (not (cffi:null-pointer-p panel)))
      (let* ((subviews (objc:invoke (objc:invoke panel "contentView") "subviews"))
             (count (objc:invoke subviews "count"))
             (button-class (objc:coerce-to-objc-class "NSButton")))
        (loop for i from 0 below count
              for view = (objc:invoke subviews "objectAtIndex:" i)
              when (and (objc:invoke-bool view "isKindOfClass:" button-class)
                        (eql index (objc:invoke view "tag")))
                do (objc:invoke view "performClick:" nil)
                   (return t))))))

(defun restarts-panel-visible-p (&optional (listener *listener*))
  (let ((panel (and listener (listener-restarts-panel listener))))
    (and panel (cffi:pointerp panel) (not (cffi:null-pointer-p panel))
         (objc:invoke-bool panel "isVisible"))))

;;; What the debugger calls ------------------------------------------------------

(defun offer-restarts (listener condition restarts)
  "Show the restarts, from the listener thread.  Never blocks it.

:WAIT NIL, so the listener thread goes straight on to its prompt: the panel and
the prompt are two doors into the same room, and waiting for the panel would
shut the other one."
  (when (and *restarts-panel-enabled* *main-thread-target*)
    (ignore-errors
     (on-main-thread () (show-restarts-panel listener condition restarts))))
  restarts)

(defun withdraw-restarts (listener)
  "Take the panel down as the debugger level exits."
  (when *main-thread-target*
    (ignore-errors
     (on-main-thread () (hide-restarts-panel listener))))
  t)
