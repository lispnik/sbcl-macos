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
  (load-init-file)
  (let ((listener (make-listener))
        (restarts (make-instance 'restarts-controller))
        (history (make-instance 'history-controller))
        (root (uikit:root-view)))
    (setf (controller-listener restarts) listener
          (getf (listener-retained listener) :restarts-controller) restarts
          (history-controller-listener history) listener
          (getf (listener-retained listener) :history-controller) history)
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
      (report-init-file listener)
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

(defun type-into-view (pointer text)
  "Type TEXT one character at a time the way UIKit does it.

UIKit\'s contract is: ask the delegate whether the change may go ahead, and
apply it only if the delegate says yes -- a NO means the delegate did whatever
was wanted itself, which is how the paredit hook works.  So each character is
offered to that method and inserted only when it answers true.

-insertText: is NOT the way in: called programmatically it does not consult the
delegate at all, so it walked straight past the hook this is here to test."
  (loop for character across text
        for caret = (caret-index pointer)
        do (when (objc:invoke-bool pointer
                                   "textView:shouldChangeTextInRange:replacementText:"
                                   pointer (cons caret 0) (string character))
             (objc:invoke pointer "insertText:" (string character))))
  text)

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
     ;; What the last launch left behind.  Zero on a first run, which is not a
     ;; failure -- the number is the interesting part, so it is logged.
     (list "init.lisp" (constantly t)
           (lambda ()
             (note "selftest: init file ~s, C-M-t bound to ~a"
                   *init-file-loaded* (paredit-key "C-M-t"))))
     (list "the saved history is loaded" (constantly t)
           (lambda ()
             (note "selftest: history has ~d line~:p from earlier launches"
                   (length (view-history (listener-view-object listener))))))
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
     ;; The history list: opened, narrowed, and a row chosen, which puts the
     ;; line in the input region without submitting it.
     (list "the history list opens and narrows"
           (lambda () (at-top-level-prompt-p listener))
           (lambda ()
             (let ((view (listener-view-object listener))
                   (pointer (listener-view listener)))
               ;; Something to find, whatever earlier launches left behind.
               (setf (view-history view)
                     (list "(list :from-the-history)" "(+ 40 2)"))
               (unless (open-history-popup listener)
                 (error "the list would not open"))
               (unless (history-popup-visible-p listener)
                 (error "the list is not on screen"))
               (unless (eql 2 (history-row-count listener))
                 (error "~a rows, wanted 2" (history-row-count listener)))
               (type-history-query "from" listener)
               (unless (eql 1 (history-row-count listener))
                 (error "~a rows after narrowing, wanted 1"
                        (history-row-count listener))))))
     ;; Left on screen, narrowed, for the hold to be photographed.
     (list :hold nil nil)
     (list "and a chosen row lands in the input region, unsubmitted"
           (constantly t)
           (lambda ()
             (let ((view (listener-view-object listener))
                   (pointer (listener-view listener)))
               (choose-history-row listener 0)
               (unless (string= (pending-input view pointer)
                                "(list :from-the-history)")
                 (error "chose ~s" (pending-input view pointer)))
               (when (history-popup-visible-p listener)
                 (error "the list stayed on screen"))
               (replace-pending-input view pointer ""))))
     ;; Paredit, through the same delegate a keyboard goes through: each
     ;; character is offered to -textView:shouldChangeTextInRange:replacementText:
     ;; exactly as UIKit offers it.
     (list "paredit balances what is typed" (lambda () (at-top-level-prompt-p listener))
           (lambda ()
             (let ((view (listener-view-object listener))
                   (pointer (listener-view listener)))
               (replace-pending-input view pointer "")
               (type-into-view pointer "(list 1 2")
               (unless (string= (pending-input view pointer) "(list 1 2)")
                 (error "( did not auto-close: ~s" (pending-input view pointer)))
               (type-into-view pointer ")")
               (unless (string= (pending-input view pointer) "(list 1 2)")
                 (error ") doubled the paren: ~s" (pending-input view pointer)))
               ;; The highlight: the caret is just past the close paren.
               (refresh-paren-highlight view pointer)
               (unless (= 2 (length (view-paren-marks view)))
                 (error "~d paren~:p tinted, wanted 2"
                        (length (view-paren-marks view))))
               ;; And a structural command, as its key would run it.
               (replace-pending-input view pointer "(list (a) b)")
               (objc:invoke pointer "setSelectedRange:"
                            (cons (+ (view-input-start view) 8) 0))
               (unless (run-paredit-at-caret view pointer 'slurp-forward)
                 (error "slurp declined"))
               (unless (string= (pending-input view pointer) "(list (a b))")
                 (error "slurp gave ~s" (pending-input view pointer)))
               ;; Left on screen, tinted, for the hold below to be photographed.
               (replace-pending-input view pointer "(defun f (x) (list x))")
               (objc:invoke pointer "setSelectedRange:"
                            (cons (transcript-length pointer) 0))
               (refresh-paren-highlight view pointer))))
     (list :hold nil nil)
     (list "the tinted pair is cleared away" (constantly t)
           (lambda ()
             (let ((view (listener-view-object listener))
                   (pointer (listener-view listener)))
               (replace-pending-input view pointer ""))))
     ;; Stop, on a form half read -- which is all it can do here.  A running
     ;; computation cannot be stopped on iOS at all: this ECL delivers no
     ;; interrupt to a thread in the app, and does not kill one either
     ;; (measured; see CLAUDE.md).  So nothing below types a form that loops,
     ;; because nothing could get the listener back.
     (list "Stop is asked, mid-form" (lambda () (at-top-level-prompt-p listener))
           (lambda ()
             (let ((view (listener-view-object listener))
                   (pointer (listener-view listener)))
               (replace-pending-input view pointer "(list 1")
               (submit-input view pointer))
             (abort-evaluation listener)))
     (list "Stop abandons a half-read form"
           (lambda () (let ((text (self-test-text listener)))
                        (search "; Aborted." text)))
           nil)
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
