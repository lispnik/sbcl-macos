;;;; src/screenshot.lisp -- driving the listener and photographing it.
;;;;
;;;; LISP_LISTENER_SCREENSHOT=<directory> makes the application run a scripted
;;;; session and write a PNG of the window at three points, then leave.  It is
;;;; how the images in the README are made, and it runs in CI on a GitHub macOS
;;;; runner, which has a window server -- lispnik/objc's suite reports Skip: 0
;;;; there with its windowed tests among them, and lem-cocoa photographs itself
;;;; the same way.
;;;;
;;;; Everything here runs on THREAD 1, inside an NSTimer callback, and pumps the
;;;; event loop between steps rather than sleeping.  That is not a style
;;;; preference: the listener thread's answers only reach the transcript through
;;;; a hop that thread 1 has to service, so a driver that slept would be waiting
;;;; for something it was itself preventing.
;;;;
;;;; Every wait is bounded and waits for a CONDITION.  A screenshot script that
;;;; sleeps two seconds and hopes goes red on a loaded runner and teaches nobody
;;;; anything.

(in-package #:lisp-listener)

;;; Reading the transcript ----------------------------------------------------

(defun transcript-text (listener)
  "The whole transcript as a Lisp string.  Main thread only."
  (let ((view (listener-view listener)))
    (transcript-substring view 0 (transcript-length view))))

(defun last-line (text)
  (subseq text (1+ (or (position #\Newline text :from-end t) -1))))

(defun waiting-at-top-level-p (listener)
  "True when the last line is a top-level prompt -- not a debugger one.

The debugger's prompt carries its level, `[1] CL-USER> ', so the bracket is
what tells the two apart."
  (let ((line (last-line (transcript-text listener))))
    (and (search "CL-USER>" line)
         (not (find #\[ line))
         t)))

(defun in-debugger-p (listener)
  (let ((line (last-line (transcript-text listener))))
    (and (search "CL-USER>" line) (find #\[ line) t)))

;;; Pumping and waiting -------------------------------------------------------

(defun pump (&optional (seconds 0.05d0))
  (objc.runloop:pump-events :seconds seconds
                            :max-seconds (* 4 seconds)
                            :until (constantly nil)))

(defun pump-for (seconds)
  "Service the event loop for SECONDS.  The window stays alive and drawing."
  (let ((deadline (+ (get-internal-real-time)
                     (round (* seconds internal-time-units-per-second)))))
    (loop while (< (get-internal-real-time) deadline) do (pump)))
  t)

(defun wait-for (predicate &key (timeout 10))
  "Pump until PREDICATE holds or TIMEOUT seconds pass.  Returns whether it held."
  (let ((deadline (+ (get-internal-real-time)
                     (round (* timeout internal-time-units-per-second)))))
    (loop
      (when (funcall predicate) (return t))
      (when (> (get-internal-real-time) deadline) (return nil))
      (pump))))

;;; Driving the listener ------------------------------------------------------

(defun type-and-submit (listener text)
  "Put TEXT in the input region and submit it, as a person would.

Through the view rather than straight onto the input queue, for two reasons:
the form then appears in the transcript, which is the entire point of a
screenshot; and the script exercises the real submit path -- the input marker,
the history, the hand-off -- instead of stepping around it."
  (let ((view (listener-view-object listener))
        (pointer (listener-view listener)))
    (replace-pending-input view pointer text)
    (submit-input view pointer))
  text)

(defun submit-and-wait (listener text marker &key (timeout 10))
  "Submit TEXT and pump until MARKER appears in what follows it."
  (let ((before (length (transcript-text listener))))
    (type-and-submit listener text)
    (or (wait-for (lambda ()
                    (let ((now (transcript-text listener)))
                      (and (> (length now) before)
                           (search marker now :start2 before))))
                  :timeout timeout)
        (progn (note "screenshots: ~s never produced ~s" text marker) nil))))

;;; Capturing -----------------------------------------------------------------

(defun window-capture-view (window)
  "The view that draws the whole window, title bar and all.

An NSWindow's content view has a SUPERVIEW -- the frame view, which draws the
title bar, the traffic lights and the border.  Asking it to draw gives the
window as it actually looks, and it does so through the ordinary offscreen
display path, which needs no permission at all.

That last part is the whole reason for going this way.  The obvious way to
photograph a window with its chrome is CGWindowListCreateImage, which
photographs the composited window -- and since Catalina that wants Screen
Recording permission.  On a CI runner there is nobody to grant it: the request
is a TCC prompt that no one can click, which is a hang rather than a failure,
and the picture would come back empty even if it did not hang.

Returns the content view instead if there is no superview, so a window built
some other way still photographs something rather than nothing."
  (let* ((content (objc:invoke window "contentView"))
         (frame-view (objc:invoke content "superview")))
    (if (and (cffi:pointerp frame-view) (not (cffi:null-pointer-p frame-view)))
        (values frame-view t)
        (values content nil))))

(defun write-window-png (window path)
  "Write WINDOW, chrome included, to PATH as a PNG.  Main thread only."
  (handler-case
      (multiple-value-bind (view chrome-p)
          (window-capture-view window)
        (let* ((bounds (objc:invoke view "bounds"))
               (representation
                 (objc:invoke view "bitmapImageRepForCachingDisplayInRect:" bounds)))
          (objc:invoke view "cacheDisplayInRect:toBitmapImageRep:" bounds representation)
          (objc:invoke (objc:invoke representation "representationUsingType:properties:"
                                    +png-file-type+
                                    (objc:invoke "NSDictionary" "dictionary"))
                       "writeToFile:atomically:" path t)
          ;; Said out loud, because "did the title bar come out" is exactly the
          ;; question a size in the log can answer and an exit code cannot.
          (note "captured ~a (~,0fx~,0f)"
                (if chrome-p "the whole window" "the content view only")
                (aref bounds 2) (aref bounds 3))))
    (error (condition) (note "snapshot: ~a" condition)))
  path)

(defun file-not-empty-p (path)
  (handler-case
      (with-open-file (stream path :element-type '(unsigned-byte 8)
                                   :if-does-not-exist nil)
        (and stream (plusp (file-length stream))))
    (error () nil)))

(defun capture (window directory name)
  "Let things settle, then write WINDOW to NAME in DIRECTORY.  Returns truth on
success -- an empty or missing file is a failure, because an exit code cannot
say the window was blank."
  (pump-for 0.6d0)
  (let ((path (merge-pathnames name (uiop:ensure-directory-pathname directory))))
    (write-window-png window (uiop:native-namestring path))
    (let ((ok (file-not-empty-p path)))
      (note "screenshot ~a: ~a" name (if ok "written" "MISSING OR EMPTY"))
      ok)))

;;; The three shots -----------------------------------------------------------

(defun shoot-session (listener directory)
  "An ordinary session: forms, printed output, returned values."
  (and (submit-and-wait listener "(+ 1 2)" "3")
       (submit-and-wait listener "(lisp-implementation-type)" "SBCL")
       (submit-and-wait listener
                        "(dotimes (i 3) (format t \"tick ~d~%\" i))"
                        "tick 2")
       (submit-and-wait listener "(loop for i from 1 to 6 collect (* i i))" "36")
       (capture (listener-window listener) directory "session.png")))

(defun shoot-debugger (listener directory)
  "An error, its restarts in the transcript, and the restarts panel.

Returns an alist of the two shots, because this scene makes two: the
transcript and the panel are separate windows, and there is no screen capture
available here to get both in one frame.

(CAR 7) rather than (ERROR \"...\"): a TYPE-ERROR from the system carries a
real report and a real restart list, which is what the pictures are for.

The restart is then taken BY CLICKING IT.  That is the point of doing it this
way round -- the panel gets exercised end to end, through the button's target
and tag and the number it queues, rather than merely photographed.  If the
click does not get us back to the top level, the panel is broken and this says
so instead of quietly aborting and looking fine."
  (let* ((entered (and (submit-and-wait listener "(car 7)" "Restarts:")
                       (wait-for (lambda () (in-debugger-p listener)) :timeout 5)))
         (transcript (and entered
                          (capture (listener-window listener) directory "debugger.png")))
         (panel-up (and entered
                        (wait-for (lambda () (restarts-panel-visible-p listener))
                                  :timeout 5)))
         (panel (and panel-up
                     (capture (listener-restarts-panel listener) directory
                              "restarts.png"))))
    (unless panel-up
      (note "screenshots: the restarts panel never appeared"))
    (let ((clicked (and panel-up
                        (click-restart 0 listener)
                        (wait-for (lambda () (waiting-at-top-level-p listener))
                                  :timeout 10))))
      (if clicked
          (note "restarts panel: clicking restart 0 returned to the top level")
          (progn
            (note "restarts panel: the click did NOT return to the top level")
            ;; Leave the listener usable for whatever runs next regardless.
            (abort-evaluation listener)
            (wait-for (lambda () (waiting-at-top-level-p listener)) :timeout 10)))
      (list (cons "debugger" transcript)
            (cons "restarts" (and panel clicked))))))

(defun shoot-interrupt (listener directory)
  "A form that never returns, and the prompt got back with Interrupt.

The window is pumped for two seconds while (LOOP) runs, which is the claim the
two-thread design makes: evaluation is on another thread, so AppKit is free.
The picture is taken after the interrupt, because that is the part a still
image can actually show -- the form, then a fresh prompt below it."
  ;; (LOOP (SLEEP ...)) rather than a bare (LOOP).  On a safepoint build an
  ;; interrupt is delivered at a safepoint poll instead of by a signal, and an
  ;; empty tight loop is the one shape that may carry none -- the interrupt
  ;; would never arrive and this would hang until the alarm killed it.  SLEEP
  ;; is interruptible for certain, and is a fairer picture of a long
  ;; computation than a spin.
  (type-and-submit listener "(loop (sleep 0.1))")
  (pump-for 2d0)
  (let ((responsive (not (waiting-at-top-level-p listener))))
    (unless responsive
      (note "screenshots: the loop did not hold the listener; the shot is wrong"))
    (abort-evaluation listener)
    (and (wait-for (lambda () (waiting-at-top-level-p listener)) :timeout 10)
         (capture (listener-window listener) directory "interrupt.png"))))

;;; The driver ----------------------------------------------------------------

(defun finish-and-exit (code)
  "Flush and leave with CODE.

:ABORT T because this runs inside an AppKit callback under -[NSApplication
run]: -terminate: has no way to carry an exit code, and unwinding out of a
timer callback is not something AppKit supports.  :ABORT T also skips flushing
streams, so the log is flushed by hand first -- in a bundle it is a buffered
file and everything said here would otherwise go nowhere."
  (ignore-errors (finish-output *log*))
  (ignore-errors (finish-output *error-output*))
  (sb-ext:exit :code code :abort t))

(defun run-screenshots ()
  "Drive the listener through three scenes, photograph each, and leave.
Exit code 0 only when every shot was written."
  (let ((listener *listener*)
        (directory (uiop:getenv "LISP_LISTENER_SCREENSHOT")))
    (ensure-directories-exist (uiop:ensure-directory-pathname directory))
    ;; The banner and the first prompt are written by the listener thread and
    ;; arrive through a hop; nothing can be typed until they have.
    (unless (wait-for (lambda () (waiting-at-top-level-p listener)) :timeout 20)
      (note "screenshots: no prompt after 20s; the listener never started.")
      (finish-and-exit 3))
    ;; One continuous session, so each picture carries the ones before it.  The
    ;; debugger therefore goes LAST: taken in the middle, its condition and
    ;; restarts would sit above the interrupt shot, which has nothing to do with
    ;; them and is the harder picture to read for it.
    (let ((results (append
                    (list (cons "session" (shoot-session listener directory))
                          (cons "interrupt" (shoot-interrupt listener directory)))
                    ;; Two shots, and a click: see SHOOT-DEBUGGER.
                    (shoot-debugger listener directory))))
      (let ((missing (mapcar #'car (remove-if #'cdr results))))
        (note "screenshots: ~d of ~d written~@[; missing: ~{~a~^, ~}~]"
              (count-if #'cdr results) (length results) missing)
        (finish-and-exit (if missing 1 0))))))
