;;;; src/app.lisp -- the application: menus, delegate, entry points, self-test.
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

;;; The controller ------------------------------------------------------------
;;;
;;; Two menu items that are the listener's own, and the timer callback the
;;; self-test hangs off.  Every body is wrapped: nothing may unwind into AppKit.

(objc:define-objc-class listener-controller ()
  ()
  (:objc-class-name "LispListenerController"))

(objc:define-objc-method ("listenerInterrupt:" :void)
    ((self listener-controller) (sender objc:objc-object-pointer))
  (declare (ignorable sender))
  (handler-case (abort-evaluation *listener*)
    (error (condition) (note "listenerInterrupt: ~a" condition))))

(objc:define-objc-method ("listenerClearTranscript:" :void)
    ((self listener-controller) (sender objc:objc-object-pointer))
  (declare (ignorable sender))
  (handler-case (clear-transcript *listener*)
    (error (condition) (note "listenerClearTranscript: ~a" condition))))

(objc:define-objc-method ("listenerSelfTest:" :void)
    ((self listener-controller) (timer objc:objc-object-pointer))
  (declare (ignorable timer))
  (handler-case (run-self-test)
    (error (condition)
      (note "self-test: ~a" condition)
      (objc:invoke (objc.runloop:shared-application) "terminate:" nil))))

;;; The application delegate --------------------------------------------------

(objc:define-objc-class listener-application-delegate ()
  ()
  (:objc-class-name "LispListenerApplicationDelegate"))

(objc:define-objc-method ("applicationShouldTerminateAfterLastWindowClosed:"
                          objc:objc-bool)
    ((self listener-application-delegate) (application objc:objc-object-pointer))
  (declare (ignorable application))
  ;; One window is the whole application; closing it means quit.
  t)

;;; Building it ---------------------------------------------------------------

(defun warm-selectors (listener)
  "Send, from thread 1, every selector the listener thread will later send.

The bridge's selector, class and trampoline caches are plain hash tables with
no lock -- fast, and fine in practice, but a PUTHASH racing a rehash is
formally undefined.  The listener thread only ever sends one message of its
own, the -performSelectorOnMainThread: hop, so priming that one here costs a
single call and removes the question."
  (objc:coerce-to-selector "listenerDrainQueue")
  (objc:coerce-to-selector "performSelectorOnMainThread:withObject:waitUntilDone:modes:")
  (let ((view (listener-view listener)))
    (when view
      (objc:invoke view "performSelectorOnMainThread:withObject:waitUntilDone:modes:"
                   (objc:coerce-to-selector "listenerDrainQueue")
                   nil nil +common-run-loop-modes+)))
  listener)

(defun build-listener (&key (title "Lisp Listener") (activation-policy 0))
  "Bring Cocoa up and make the listener.  Main thread only; returns it.

The order is the design: initialize with AppKit named, so the view's class can
be realized; empty the attribute cache, which may hold pointers from a previous
image; make the window; and only then start the thread, because nothing may
try to reach thread 1 before there is a view to deliver the hop to."
  (objc.runloop:check-main-thread "Starting the listener")
  (objc:ensure-objc-initialized :modules (list +cocoa-framework+))
  (reset-transcript-attributes)
  (objc.runloop:shared-application :activation-policy activation-policy)
  (let ((listener (make-listener)))
    (setf *listener* listener)
    (let ((controller (make-instance 'listener-controller))
          (delegate (make-instance 'listener-application-delegate)))
      (setf (getf (listener-retained listener) :controller) controller
            (getf (listener-retained listener) :application-delegate) delegate)
      (make-listener-window listener :title title)
      (install-menu (objc:objc-object-pointer controller))
      (objc:invoke (objc.runloop:shared-application) "setDelegate:"
                   (objc:objc-object-pointer delegate))
      (show-listener-window listener)
      (warm-selectors listener)
      (start-listener-thread listener)
      ;; The banner was written before there was anywhere to put it.
      (force-output (listener-output listener))
      listener)))

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
    (when path (write-window-png listener path))
    (note "selftest: ~a => ~a~@[, png ~a~]"
          +self-test-form+
          (if found +self-test-expected+ "NOT FOUND")
          path)
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

(defun write-window-png (listener path)
  "Write the window's content view to PATH as a PNG.  Main thread only."
  (handler-case
      (let* ((view (objc:invoke (listener-window listener) "contentView"))
             (bounds (objc:invoke view "bounds"))
             (representation (objc:invoke view "bitmapImageRepForCachingDisplayInRect:"
                                          bounds)))
        (objc:invoke view "cacheDisplayInRect:toBitmapImageRep:" bounds representation)
        (objc:invoke (objc:invoke representation "representationUsingType:properties:"
                                  +png-file-type+
                                  (objc:invoke "NSDictionary" "dictionary"))
                     "writeToFile:atomically:" path t))
    (error (condition) (note "snapshot: ~a" condition)))
  path)

(defun schedule-self-test (listener seconds)
  (objc:invoke "NSTimer"
               "scheduledTimerWithTimeInterval:target:selector:userInfo:repeats:"
               (coerce seconds 'double-float)
               (objc:objc-object-pointer (getf (listener-retained listener) :controller))
               (objc:coerce-to-selector "listenerSelfTest:")
               nil nil))

;;; Entry points --------------------------------------------------------------

(defun main ()
  "The bundle's entry point, and a fine way to start it from a shell.

Does not return: -[NSApplication run] does not."
  ;; asdf-macos-app LET-binds *STANDARD-OUTPUT* to the bundle's log file around
  ;; this call.  Capture it now: once the streams below are in place, a failure
  ;; in the transcript machinery has nowhere else to be reported, and "nothing
  ;; happened and nothing was logged" is the worst outcome available.
  (setf *log* *error-output*)
  (let ((listener (build-listener)))
    (when (uiop:getenv "LISP_LISTENER_SELFTEST")
      (schedule-self-test listener 1.5))
    (objc.runloop:run-cocoa-application)))

(defun run-listener (&key (title "Lisp Listener"))
  "Start a listener from a plain SBCL REPL, on thread 1, and return when the
window closes.

-[NSApplication runModalForWindow:] rather than a pump loop, and that is
measured rather than stylistic: a hand-rolled nextEventMatchingMask:/sendEvent:
loop never gets to block, because AppKit keeps a supply of AppKitDefined events
coming.  lispnik/objc measured 100.9% CPU pumping against 0.4% modal.

The keyboard is handed back afterwards.  Showing a window makes this process
the frontmost application and it STAYS frontmost when the window closes, so
without RESTORE-FRONTMOST the terminal you started from sits at its prompt
while the window server delivers every keystroke here -- which reads exactly
like a hang and is not one."
  (setf *log* *error-output*)
  (let* ((listener (build-listener :title title))
         (window (listener-window listener)))
    (objc:retain window)
    (unwind-protect
         (objc:invoke (objc.runloop:shared-application) "runModalForWindow:" window)
      (ignore-errors (queue-set-eof (listener-input listener)))
      (ignore-errors (abort-evaluation listener))
      (ignore-errors
       (objc:invoke window "orderOut:" nil)
       (objc.runloop:pump-events :seconds 0.02d0 :max-seconds 0.3d0
                                 :until (constantly nil)))
      (objc:release window)
      (objc.runloop:restore-frontmost))
    t))
