;;;; src/macos/app.lisp -- the application: menus, delegate, entry points, self-test.
;;;;
;;;; Everything foreign is made HERE, at run time, and nothing is stashed in a
;;;; defvar at load time.  The bundle's image is a dumped core: lispnik/objc
;;;; re-queues every class and method definition across the dump and rebuilds
;;;; them when ENSURE-OBJC-INITIALIZED next runs, but a pointer saved at load
;;;; time is a pointer from the process that did the dumping.

(in-package #:lisp-listener)

(defparameter +cocoa-framework+
  "/System/Library/Frameworks/Cocoa.framework/Versions/A/Cocoa"
  "Named explicitly, and loaded before anything is realized.

DEFINE-OBJC-CLASS queues its class registration until initialization, and
LispListenerView's superclass is NSTextView -- so AppKit has to be open by the
time ENSURE-OBJC-INITIALIZED drains that queue, which it does at the end of the
very same call.  Leaving it to SHARED-APPLICATION is too late.")

(defvar *menu-controller* nil
  "The object the menu bar's own items target, or NIL before there is a menu.

Held in a global because a menu item's target is NOT retained by Cocoa and the
menu bar outlives any one window: hanging it off the first listener would
leave every item pointing into freed memory as soon as that window closed.
Made at run time like everything else foreign here, so a dumped core starts
with NIL and builds a fresh one.")

(defvar *application-delegate* nil
  "The application's delegate.  A global for the same reason, and set once.")

;;; The controller ------------------------------------------------------------
;;;
;;; Three menu items that are the listener's own, and the timer callback the
;;; self-test hangs off.  Every body is wrapped: nothing may unwind into AppKit.
;;;
;;; Each acts on (CURRENT-LISTENER) -- the key window's -- rather than on
;;; *LISTENER*.  A menu item's action says nothing about which window it came
;;; from, and with two listeners open the front one is the one meant.

(objc:define-objc-class listener-controller ()
  ()
  (:objc-class-name "LispListenerController"))

(objc:define-objc-method ("listenerNewListener:" :void)
    ((self listener-controller) (sender objc:objc-object-pointer))
  (declare (ignorable sender))
  (handler-case (new-listener)
    (error (condition) (note "listenerNewListener: ~a" condition))))

(objc:define-objc-method ("listenerInterrupt:" :void)
    ((self listener-controller) (sender objc:objc-object-pointer))
  (declare (ignorable sender))
  (handler-case (abort-evaluation (current-listener))
    (error (condition) (note "listenerInterrupt: ~a" condition))))

(objc:define-objc-method ("listenerHistory:" :void)
    ((self listener-controller) (sender objc:objc-object-pointer))
  (declare (ignorable sender))
  ;; A menu item, so that Command-R comes for free: the key equivalent is
  ;; AppKit's to deliver, and -keyDown: never sees it.
  (handler-case (open-history-popup (current-listener))
    (error (condition) (note "listenerHistory: ~a" condition))))

(objc:define-objc-method ("listenerClearTranscript:" :void)
    ((self listener-controller) (sender objc:objc-object-pointer))
  (declare (ignorable sender))
  (handler-case (clear-transcript (current-listener))
    (error (condition) (note "listenerClearTranscript: ~a" condition))))

(objc:define-objc-method ("listenerSelfTest:" :void)
    ((self listener-controller) (timer objc:objc-object-pointer))
  (declare (ignorable timer))
  (handler-case (run-self-test)
    (error (condition)
      (note "self-test: ~a" condition)
      (objc:invoke (objc.runloop:shared-application) "terminate:" nil))))

(objc:define-objc-method ("listenerScreenshots:" :void)
    ((self listener-controller) (timer objc:objc-object-pointer))
  (declare (ignorable timer))
  ;; RUN-SCREENSHOTS does not return -- it exits the process with a status that
  ;; says whether every shot was written.  The handler is for the way there.
  (handler-case (run-screenshots)
    (error (condition)
      (note "screenshots: ~a" condition)
      (finish-and-exit 4))))

;;; The application delegate --------------------------------------------------

(objc:define-objc-class listener-application-delegate ()
  ()
  (:objc-class-name "LispListenerApplicationDelegate"))

(objc:define-objc-method ("applicationShouldTerminateAfterLastWindowClosed:"
                          objc:objc-bool)
    ((self listener-application-delegate) (application objc:objc-object-pointer))
  (declare (ignorable application))
  ;; The LAST window is the whole application; closing one of several is not.
  ;; AppKit asks only once no window is left, so this keeps its meaning now
  ;; that there can be more than one -- it stops meaning "the first close".
  ;;
  ;; But NOT in a REPL session.  -terminate: exits the process, and under
  ;; RUN-LISTENER the process is somebody's SBCL: answering T there killed the
  ;; REPL instead of returning to it, and did it before RUN-LISTENER'S own
  ;; unwinding had run.  There, closing the last window stops -run instead.
  (not *stop-run-loop-on-last-close*))

;;; Building it ---------------------------------------------------------------

(defun build-listener (&key (title "Lisp Listener") (activation-policy 0))
  "Bring Cocoa up and make the listener.  Main thread only; returns it.

The order is the design: initialize with AppKit named, so the view's class can
be realized; empty the attribute cache, which may hold pointers from a previous
image; make the window; and only then start the thread, because nothing may
try to reach thread 1 before there is a view to deliver the hop to."
  (objc.runloop:check-main-thread "Starting the listener")
  (objc:ensure-objc-initialized :modules (list +cocoa-framework+))
  ;; Only for the first.  The cache this empties may hold pointers from a
  ;; PREVIOUS IMAGE, which is a question asked once per process; emptying it
  ;; again under a listener already on screen would throw away attributes its
  ;; transcript is still being written with.
  (unless *listeners*
    (reset-transcript-attributes)
    ;; Before the thread starts, so that what it sets -- the keymap, the font --
    ;; is in force from the banner onwards.  Only for the first listener: a
    ;; second window must not run somebody's init file again.
    (load-init-file))
  (objc.runloop:shared-application :activation-policy activation-policy)
  (let ((listener (make-listener))
        (restarts (make-instance 'restarts-controller))
        (history (make-instance 'history-controller)))
    ;; A button's target is NOT retained by Cocoa, so these controllers have to
    ;; be held here or they would be collected while still installed.
    (setf (controller-listener restarts) listener
          (getf (listener-retained listener) :restarts-controller) restarts
          (history-controller-listener history) listener
          (getf (listener-retained listener) :history-controller) history)
    (make-listener-window listener :title title)
    ;; Registered BEFORE the thread starts, and before anything can ask which
    ;; listener a view belongs to.
    (register-listener listener)
    (setf *listener* listener)
    (ensure-application-furniture)
    (show-listener-window listener)
    (warm-selectors listener)
    (start-listener-thread listener)
    (report-init-file listener)
    ;; The banner was written before there was anywhere to put it.
    (force-output (listener-output listener))
    listener))

(defun ensure-application-furniture ()
  "Make the menu bar and the application delegate, once per process.

Both belong to the application rather than to a window, so a second listener
reuses them: installing a second menu would replace a working one with another
whose target dies with the window that made it."
  (unless *menu-controller*
    (let ((controller (make-instance 'listener-controller))
          (delegate (make-instance 'listener-application-delegate)))
      (setf *menu-controller* controller
            *application-delegate* delegate)
      (install-menu (objc:objc-object-pointer controller))
      (objc:invoke (objc.runloop:shared-application) "setDelegate:"
                   (objc:objc-object-pointer delegate))))
  *menu-controller*)

(defun new-listener (&key title)
  "Another listener: its own window, its own thread, its own transcript.

Thread 1 only -- this is what the New Listener menu item does.  They share
nothing but the image they evaluate in, so a form that never returns in one
leaves the others alone, and a debugger level in one leaves the others at
their own top level."
  (objc.runloop:check-main-thread "Opening a listener")
  (build-listener
   :title (or title (format nil "Lisp Listener ~d" (1+ (length *listeners*))))))

;;; The self-test -------------------------------------------------------------

(defparameter +self-test-form+ "(+ 1 2)")
(defparameter +self-test-expected+ "3")

(defun run-self-test ()
  "Evaluate a form, wait for its value, write the window to a PNG, and quit.

How the built application is checked from a shell.  The wait is bounded and
for the value rather than a fixed delay: a test that sleeps two seconds and
hopes is a test that goes red on a loaded machine and teaches nobody anything."
  (let* ((listener *listener*)
         (path (uiop:getenv "LISP_LISTENER_SELFTEST"))
         (deadline (+ (get-internal-real-time)
                      (* 10 internal-time-units-per-second)))
         (found nil))
    ;; Typed into the view and submitted through SUBMIT-INPUT, not pushed
    ;; straight onto the queue.  Two reasons: the form then appears in the
    ;; transcript the way it would if someone had typed it, which is what
    ;; SELF-TEST-ANSWERED-P looks for; and the check exercises the real submit
    ;; path -- the input marker, the history, the hand-off -- rather than
    ;; stepping around it and proving only that EVAL works.
    (let ((view (listener-view-object listener))
          (pointer (listener-view listener)))
      (replace-pending-input view pointer +self-test-form+)
      (submit-input view pointer))
    ;; Pump rather than sleep: this is thread 1, and the listener's answer can
    ;; only reach the transcript through a hop that thread 1 has to service.
    (loop until (or found (> (get-internal-real-time) deadline))
          do (objc.runloop:pump-events :seconds 0.05d0 :max-seconds 0.2d0
                                       :until (constantly nil))
             (setf found (self-test-answered-p listener)))
    (when path (write-window-png (listener-window listener) path))
    (note "selftest: ~a => ~a~@[, png ~a~]"
          +self-test-form+
          (if found +self-test-expected+ "NOT FOUND")
          path)
    ;; Asked here because here is the dumped core.  Every other check of the
    ;; menu runs in an image that built its own classes from source; this one
    ;; runs in the bundle, which is where a Lisp-defined Objective-C method
    ;; stops surviving, and it is the only place the question means anything.
    (let ((menu (menu-item-present-p "Listener" "New Listener")))
      (note "selftest: New Listener menu item ~:[MISSING~;present~]" menu)
      (unless menu (setf found nil)))
    (objc:invoke (objc.runloop:shared-application) "terminate:" nil)
    found))

(defun self-test-answered-p (listener)
  "Whether the value has appeared in the transcript after the form did."
  (let* ((view (listener-view listener))
         (text (transcript-substring view 0 (transcript-length view)))
         (echo (search +self-test-form+ text)))
    (and echo
         (search +self-test-expected+ text :start2 (+ echo (length +self-test-form+)))
         t)))

(defun schedule-after (seconds selector)
  "Send SELECTOR to the menu controller SECONDS after the event loop starts.

A timer rather than a call here: -[NSApplication run] has not been entered yet,
so nothing driven from this point could pump anything."
  (objc:invoke "NSTimer"
               "scheduledTimerWithTimeInterval:target:selector:userInfo:repeats:"
               (coerce seconds 'double-float)
               (objc:objc-object-pointer (or *menu-controller*
                                             (ensure-application-furniture)))
               (objc:coerce-to-selector selector)
               nil nil))

(defun schedule-self-test (seconds)
  (schedule-after seconds "listenerSelfTest:"))

(defun schedule-screenshots (seconds)
  (schedule-after seconds "listenerScreenshots:"))

;;; Entry points --------------------------------------------------------------

(defun main ()
  "The bundle's entry point, and a fine way to start it from a shell.

Does not return: -[NSApplication run] does not."
  ;; asdf-macos-app LET-binds *STANDARD-OUTPUT* to the bundle's log file around
  ;; this call.  Capture it now: once the streams below are in place, a failure
  ;; in the transcript machinery has nowhere else to be reported, and "nothing
  ;; happened and nothing was logged" is the worst outcome available.
  (setf *log* *error-output*)
  ;; Initialize BEFORE asking about the window server.  WINDOW-SERVER-P answers
  ;; NIL on any error, so asking it first would report "no window server" for a
  ;; runtime that simply had not been brought up yet -- a plausible message for
  ;; the wrong reason.  ENSURE-OBJC-INITIALIZED is idempotent and BUILD-LISTENER
  ;; calls it again.
  (objc:ensure-objc-initialized :modules (list +cocoa-framework+))
  (require-window-server)
  (build-listener)
  (cond
    ((uiop:getenv "LISP_LISTENER_SCREENSHOT") (schedule-screenshots 1.0))
    ((uiop:getenv "LISP_LISTENER_SELFTEST") (schedule-self-test 1.5)))
  (objc.runloop:run-cocoa-application))

(defun require-window-server ()
  "Leave, with a reason, when there is nothing to draw on.

[NSScreen mainScreen] is nil in a process with no display.  Without this check
that becomes a window nobody can see and an event loop nobody can end, which
presents as a hang -- the worst way for this to fail, and the likeliest place
to meet it is an automated one."
  (unless (objc.runloop:window-server-p)
    (note "there is no window server, so there is nowhere to put a listener.")
    (ignore-errors (finish-output *log*))
    (exit-process 2)))

(defun run-listener (&key (title "Lisp Listener"))
  "Start a listener from a plain SBCL REPL, on thread 1, and return when the
LAST listener window closes.

-[NSApplication run] rather than a pump loop, and that is measured rather than
stylistic: a hand-rolled nextEventMatchingMask:/sendEvent: loop never gets to
block, because AppKit keeps a supply of AppKitDefined events coming.
lispnik/objc measured 100.9% CPU pumping against 0.4% for a real loop.

-run rather than -runModalForWindow:, which is what this used to be.  A modal
session blocks events to every OTHER window of the application, so New Listener
would have opened a window that could be seen and not typed in -- the restarts
panel escapes that only by being an NSPanel, which works during a modal
session; a second listener is an ordinary window and does not.  The cost is
that -run does not stop on its own, which is what *STOP-RUN-LOOP-ON-LAST-CLOSE*
and STOP-RUN-LOOP-SOON are for.

The keyboard is handed back afterwards.  Showing a window makes this process
the frontmost application and it STAYS frontmost when the window closes, so
without RESTORE-FRONTMOST the terminal you started from sits at its prompt
while the window server delivers every keystroke here -- which reads exactly
like a hang and is not one."
  (setf *log* *error-output*)
  (let* ((*stop-run-loop-on-last-close* t)
         (listener (build-listener :title title))
         (window (listener-window listener)))
    ;; Retained across the loop: -releasedWhenClosed is already off, but this
    ;; window is also the one the caller was handed and it outlives the frame.
    (objc:retain window)
    (unwind-protect
         (objc.runloop:run-cocoa-application)
      ;; Every listener still open, not just the one this call made: New
      ;; Listener may have added others, and -run can be stopped with them up.
      (dolist (other (copy-list *listeners*))
        (ignore-errors (queue-set-eof (listener-input other)))
        (ignore-errors (abort-evaluation other))
        (ignore-errors (objc:invoke (listener-window other) "orderOut:" nil))
        (ignore-errors (unregister-listener other)))
      (ignore-errors (retarget-main-thread))
      (ignore-errors (queue-set-eof (listener-input listener)))
      (ignore-errors (abort-evaluation listener))
      (ignore-errors
       (objc:invoke window "orderOut:" nil)
       (objc.runloop:pump-events :seconds 0.02d0 :max-seconds 0.3d0
                                 :until (constantly nil)))
      (objc:release window)
      (objc.runloop:restore-frontmost))
    t))
