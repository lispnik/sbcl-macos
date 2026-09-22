;;;; src/ios/app.lisp -- the iOS application: one listener, filling the screen.
;;;;
;;;; asdf-ios-app owns main() and UIApplicationMain.  When the scene connects it
;;;; boots ECL, makes a window with a plain root view controller, and calls
;;;; IOS-START on the main thread, which must RETURN so the run loop can go on.
;;;; So this is the Mac's BUILD-LISTENER without the application: there is no
;;;; menu, no second window and no run loop to enter.

(in-package #:lisp-listener)

(defun ios-start ()
  "The entry point.  Main thread; returns once the listener is running.

The order is the Mac's and for the same reason: the view first, then the
target the listener thread hops to, and only then the thread."
  ;; asdf-ios-app copies standard output into the app's Documents/console.log,
  ;; which is the one place a failure here can be read back from.
  (setf *log* *standard-output*)
  (objc:ensure-objc-initialized)
  (reset-transcript-attributes)
  (let ((listener (make-listener))
        (restarts (make-instance 'restarts-controller))
        (root (uikit:root-view)))
    (setf (controller-listener restarts) listener
          (getf (listener-retained listener) :restarts-controller) restarts)
    (multiple-value-bind (pointer object) (make-listener-view)
      (setf (listener-view listener) pointer
            (listener-view-object listener) object
            (listener-window listener) (uikit:key-window))
      (objc:invoke root "setBackgroundColor:"
                   (objc:invoke "UIColor" "systemBackgroundColor"))
      (objc:invoke root "addSubview:" pointer)
      (let ((safe (objc:invoke root "safeAreaLayoutGuide")))
        (uikit:pin pointer "topAnchor" safe "topAnchor")
        (uikit:pin pointer "leadingAnchor" safe "leadingAnchor" 4)
        (uikit:pin pointer "trailingAnchor" safe "trailingAnchor" -4))
      ;; Above the keyboard, not under it, and following it as it comes and
      ;; goes -- the key bar included.
      (uikit:pin pointer "bottomAnchor"
                 (objc:invoke root "keyboardLayoutGuide") "topAnchor")
      (register-listener listener)
      (setf *listener* listener
            *main-thread-target* pointer)
      (warm-selectors listener)
      (start-listener-thread listener)
      ;; The banner was written before there was anywhere to put it.
      (force-output (listener-output listener))
      (objc:invoke pointer "becomeFirstResponder")
      (let ((test (getenv "LISP_LISTENER_SELF_TEST")))
        (when test
          (start-self-test listener (or (ignore-errors (parse-integer test)) 0)))))
    listener))

(defun current-listener ()
  "The one listener there is."
  (or *listener* (first *listeners*)))

;;; The self-test -------------------------------------------------------------
;;;
;;; Started when LISP_LISTENER_SELF_TEST is set -- `xcrun simctl launch' passes
;;; it on as SIMCTL_CHILD_LISP_LISTENER_SELF_TEST.  It drives the listener the
;;; way a person would, through SUBMIT-INPUT, the Tab key and the restarts
;;; sheet, and writes one line per step to the log, which is console.log.  The
;;; value is how many seconds to hold on each screen worth photographing.
;;;
;;; A timer drives it, not a loop: this is the main thread, and every answer
;;; from the listener thread arrives through a hop the main thread has to be
;;; free to service.

(defvar *self-test* nil)

(defstruct (self-test (:constructor make-self-test (listener hold steps)))
  listener hold steps (started (get-internal-real-time)) timer (failures 0))

(defun self-test-text (listener)
  (let ((view (listener-view listener)))
    (transcript-substring view 0 (transcript-length view))))

(defun at-top-level-prompt-p (listener)
  (let* ((text (self-test-text listener))
         (line (subseq text (1+ (or (position #\Newline text :from-end t) -1)))))
    (and (search "CL-USER>" line) (not (find #\[ line)))))

(defun type-line (listener text)
  (let ((view (listener-view-object listener))
        (pointer (listener-view listener)))
    (replace-pending-input view pointer text)
    (submit-input view pointer)))

(defun build-self-test-steps (listener)
  "Each step: a label, a predicate that says it may run, and what it does.
A step whose predicate has not held within its time fails."
  (let ((view (listener-view-object listener))
        (pointer (listener-view listener)))
    (list
     (list "the listener prompts" (lambda () (at-top-level-prompt-p listener)) nil)
     (list "(+ 1 2) is typed" (constantly t) (lambda () (type-line listener "(+ 1 2)")))
     (list "it evaluates to 3"
           (lambda () (let ((text (self-test-text listener)))
                        (search (format nil "~%3~%") text)))
           nil)
     (list "Tab completes multiple-value-b"
           (lambda () (at-top-level-prompt-p listener))
           (lambda ()
             (replace-pending-input view pointer "(multiple-value-b")
             (complete-at-caret view pointer)
             (unless (string= (pending-input view pointer) "(multiple-value-bind")
               (error "completed to ~s" (pending-input view pointer)))
             (replace-pending-input view pointer "")))
     (list "an error is typed" (constantly t)
           (lambda () (type-line listener "(error \"boom\")")))
     (list "the restarts sheet appears" (lambda () (restarts-panel-visible-p listener))
           nil)
     ;; The table, not just the sheet: a data source that was never found
     ;; answers zero, and a sheet of blank rows looks identical in a picture.
     (list "its table has a row per restart" (constantly t)
           (lambda ()
             (let ((rows (restarts-table-row-count listener)))
               (unless (and rows (>= rows 2))
                 (error "the table has ~a rows" rows)))))
     (list :hold nil nil)
     (list "Cancel returns to the top level" (constantly t)
           (lambda ()
             (unless (cancel-to-top-level listener)
               (error "no top-level restart on offer"))))
     (list "the top-level prompt is back"
           (lambda () (and (at-top-level-prompt-p listener)
                           (not (restarts-panel-visible-p listener))))
           nil)
     (list :hold nil nil))))

(defun start-self-test (listener hold)
  (setf *self-test* (make-self-test listener hold (build-self-test-steps listener)))
  (setf (self-test-timer *self-test*)
        (uikit:after-every 0.2d0 (lambda (timer)
                                   (declare (ignore timer))
                                   (self-test-tick *self-test*))))
  (note "selftest: started"))

(defun self-test-tick (test)
  (let* ((step (first (self-test-steps test)))
         (elapsed (/ (- (get-internal-real-time) (self-test-started test))
                     internal-time-units-per-second)))
    (flet ((next ()
             (pop (self-test-steps test))
             (setf (self-test-started test) (get-internal-real-time))))
      (cond
        ((null step)
         (objc:invoke (self-test-timer test) "invalidate")
         (note "selftest: ~:[FAIL (~d)~;PASS~]"
               (zerop (self-test-failures test)) (self-test-failures test)))
        ((eq (first step) :hold)
         (when (>= elapsed (self-test-hold test)) (next)))
        (t
         (destructuring-bind (label ready action) step
           (cond
             ((ignore-errors (funcall ready))
              (handler-case (progn (when action (funcall action))
                                   (note "selftest: ok    ~a" label))
                (error (condition)
                  (incf (self-test-failures test))
                  (note "selftest: FAIL  ~a: ~a" label condition)))
              (next))
             ((> elapsed 10)
              (incf (self-test-failures test))
              (note "selftest: FAIL  ~a: timed out" label)
              (next)))))))))
