;;;; tools/headless-test.lisp -- drive a real listener, with no Mac.
;;;;
;;;;     sbcl --script tools/headless-test.lisp       (or: make test)
;;;;
;;;; The third and last of the off-macOS checks, and the only one that runs the
;;;; program.  syntax-check asks whether src/ parses; compile-check asks whether
;;;; it compiles; this asks whether the listener WORKS -- a real thread, real
;;;; gray streams, the real reader, evaluator, printer and debugger, with the
;;;; interactive restarts and Y-OR-N-P actually conversing.  What makes that
;;;; possible is that only ONE of the listener's two halves is Cocoa.
;;;;
;;;; The seam is SCHEDULE-FLUSH.  It declines to do anything while
;;;; *MAIN-THREAD-TARGET* is NIL -- written for the interval before the window
;;;; exists, so the banner is not lost -- and with no target ever set, output
;;;; simply accumulates in the stream's own segments.  TRANSCRIPT-SO-FAR below
;;;; reads them.  Nothing here touches AppKit, and tools/stubs/stubs.lisp
;;;; answers for OBJC:INVOKE on the few paths that reach it.
;;;;
;;;; What this does NOT check, and must not be read as checking: the window, the
;;;; text view, the restarts panel, the table, its buttons, the screenshots --
;;;; every one of those is a stub here and is exactly as unverified as before.
;;;; That is still the macOS workflow's job.
;;;;
;;;; This exists because it found a bug three rounds of CI screenshots had not:
;;;; LISTENER-INPUT-STREAM had no STREAM-LINE-COLUMN method, FRESH-LINE on a
;;;; two-way stream asks the INPUT half for its column, and so every restart
;;;; that prompts -- and Y-OR-N-P with it -- was broken.  Cases 3, 4 and 5 below
;;;; are that bug, and they fail without the method.
;;;;
;;;; Exits non-zero if any case fails.

(in-package #:cl-user)

(handler-bind ((warning #'muffle-warning))
  (require :asdf))

(defparameter *here*
  (make-pathname :name nil :type nil :version nil
                 :defaults (or *load-truename* *default-pathname-defaults*)))
(defparameter *root*
  (make-pathname :directory (butlast (pathname-directory *here*)) :defaults *here*))

(handler-bind ((warning #'muffle-warning))
  (load (merge-pathnames "tools/stubs/stubs.lisp" *root*) :external-format :utf-8)
  ;; LOAD rather than COMPILE-FILE: the compiler's report is compile-check's
  ;; business and duplicating it here would only be noise.  The order is
  ;; lisp-listener.asd's, which is :SERIAL.
  (dolist (name '("package" "main-thread" "queue" "listener" "view" "streams"
                  "restarts" "repl" "window" "screenshot" "app"))
    (load (merge-pathnames (format nil "src/~a.lisp" name) *root*)
          :external-format :utf-8)))

(in-package #:lisp-listener)

;;; Reading and writing a running listener -------------------------------------

(defparameter *timeout* 10
  "Seconds WAIT-FOR-TEXT will wait.  Generous: these all finish in well under a
second, and the only thing a tight bound buys is a flaky test on a loaded CI
runner.  A case that is going to fail fails by waiting the whole ten.")

(defun count-substring (needle haystack)
  (loop with count = 0 with start = 0
        for position = (search needle haystack :start2 start)
        while position
        do (incf count) (setf start (1+ position))
        finally (return count)))

(defun transcript-so-far (listener)
  "Everything the listener has printed.

With no main thread to flush to, this is where the transcript lives -- the
output stream's own segment list, newest first, each a (KIND . TEXT)."
  (let ((stream (listener-output listener)))
    (bt:with-lock-held ((stream-lock stream))
      (apply #'concatenate 'string
             (mapcar #'cdr (reverse (stream-segments stream)))))))

(defun wait-for-text (listener text &key (timeout *timeout*))
  "Wait for TEXT to appear in the transcript.  True if it did."
  (let ((deadline (+ (get-internal-real-time)
                     (* timeout internal-time-units-per-second))))
    (loop (when (search text (transcript-so-far listener)) (return t))
          (when (> (get-internal-real-time) deadline) (return nil))
          (sleep 0.02))))

(defun say (listener line)
  "Type LINE and press return, exactly as the view does."
  ;; A short settle first.  The listener is a separate thread and the queue has
  ;; no notion of a prompt; typing the answer to a question before the question
  ;; has been asked would have it read as the form BEFORE it, which is a race
  ;; this test would lose intermittently rather than a defect it should report.
  (sleep 0.05)
  (queue-push-string (listener-input listener) (format nil "~a~%" line))
  line)

;;; Reporting ------------------------------------------------------------------

(defvar *failures* 0)
(defvar *checks* 0)

(defun check (ok control &rest arguments)
  (incf *checks*)
  (unless ok (incf *failures*))
  (format t "~&  ~:[FAIL~;ok  ~]  ~?~%" ok control arguments)
  (finish-output)
  ok)

(defun check-text (listener text label)
  (check (wait-for-text listener text) "~a" label))

(defmacro defcase (name docstring &body body)
  "One case: a fresh listener, a heading, and BODY with LISTENER bound.

Fresh each time because the debugger cases end a level deep and a case that
inherited that would be testing the case before it."
  `(defun ,name ()
     (format t "~&~%~a~%" ,docstring)
     (finish-output)
     (let ((listener (make-listener)))
       (setf *listener* listener)
       (start-listener-thread listener)
       (check-text listener "CL-USER>" "the listener starts and prompts")
       (unwind-protect (progn ,@body)
         (queue-set-eof (listener-input listener))
         (sleep 0.1)))))

;;; The cases ------------------------------------------------------------------

(defcase case-session "A session: the banner, a form, its value, another prompt."
  (check-text listener (lisp-implementation-type) "the banner names the implementation")
  (say listener "(+ 1 2)")
  (check-text listener "3" "(+ 1 2) evaluates to 3")
  (say listener "(list :a :b)")
  (check-text listener "(:A :B)" "a second form evaluates after the first")
  (check (let ((text (transcript-so-far listener)))
           (> (count-substring "CL-USER>" text) 2))
         "each form is followed by a fresh prompt"))

(defcase case-debugger "The debugger: the report, the restarts, the backtrace."
  (say listener "(error \"a deliberate error\")")
  (check-text listener "a deliberate error" "the condition's report is printed")
  (check-text listener "Restarts:" "the restarts are listed")
  (check-text listener "[ABORT]" "the listener's own ABORT is among them")
  (check-text listener "Backtrace:" "the backtrace is printed")
  (check-text listener "[1] CL-USER>" "the prompt shows the debugger level"))

(defcase case-use-value
    "USE-VALUE: an interactive restart prompts, and its value is used."
  (say listener "(symbol-value '*no-such-variable-at-all*)")
  (check-text listener "Restarts:" "an unbound variable enters the debugger")
  ;; 1 is USE-VALUE, whose interactive function prompts for a form.
  (say listener "1")
  (check-text listener "Enter a form to be evaluated" "USE-VALUE prompts")
  (say listener "(* 6 7)")
  (check-text listener "42" "the value it read is the value of the form"))

(defcase case-store-value
    "STORE-VALUE: the other interactive restart on an unbound variable."
  (say listener "(symbol-value '*another-missing-variable*)")
  (check-text listener "Restarts:" "an unbound variable enters the debugger")
  (say listener "2")
  (check-text listener "Enter a form to be evaluated" "STORE-VALUE prompts")
  (say listener "(list :stored)")
  (check-text listener "(:STORED)" "the value it read is stored and returned"))

(defcase case-y-or-n-p "Y-OR-N-P: the question is asked AND the answer arrives."
  ;; Not a restart at all, and that is the point: the bug these three cases
  ;; share was in *QUERY-IO*, so it took out every question the listener asks.
  (say listener "(if (y-or-n-p \"shall I? \") :yes :no)")
  (check-text listener "shall I?" "the question is asked")
  (say listener "y")
  (check-text listener ":YES" "the answer reaches Y-OR-N-P"))

(defcase case-abort "ABORT: back to the top level, and it says so."
  (say listener "(error \"abort me\")")
  (check-text listener "Restarts:" "the debugger is up")
  (say listener "0")
  (check-text listener "; Aborted." "aborting says so")
  (say listener "(+ 40 2)")
  (check-text listener "42" "the listener evaluates again afterwards"))

(defcase case-toplevel-restart-index
    "The toplevel restart is NOT index 0 -- Cancel must find it by object."
  ;; A regression test for a decision, not for a line.  On an unbound variable
  ;; SBCL establishes CONTINUE, USE-VALUE and STORE-VALUE in front of the
  ;; listener's own ABORT, so the panel's Cancel button sits at index 3.  Had
  ;; Cancel assumed index 0 -- which it does not, it looks the restart up by
  ;; object -- Escape would have invoked `[CONTINUE] Retry using
  ;; *NO-SUCH-VARIABLE*' and spun, rather than returning to the top level.
  (say listener "(symbol-value '*yet-another-missing*)")
  (check-text listener "Restarts:" "the debugger is up")
  (let* ((text (transcript-so-far listener))
         (start (search "Restarts:" text))
         (abort-line (search "[ABORT]" text :start2 (or start 0))))
    (check (and abort-line
                (let ((line-start (1+ (or (position #\Newline text :end abort-line
                                                         :from-end t)
                                          0))))
                  (not (eql (char text line-start) #\0))))
           "the listener's ABORT is not restart 0 here"))
  (check-text listener "[CONTINUE]" "CONTINUE is in front of it")
  ;; And the thing Cancel does, from the listener's side: taking that restart
  ;; returns to the top level however deep it sits.
  (say listener "3")
  (check-text listener "; Aborted." "taking it by its real index aborts")
  ;; CL-USER> and not [1] CL-USER>: the level it landed at is the whole point.
  (check-text listener (format nil "; Aborted.~%CL-USER>")
              "and lands at the TOP level, not one below it"))

(defun case-interactive-restarts-are-marked ()
  "The panel's labels mark the restarts that will ask.

No listener here: RESTART-TITLES is a pure function of the restarts in scope,
so establishing one of each kind and reading the labels is both smaller and
more exact than fishing them out of a debugger.  USE-VALUE carries an
interactive function and PLAIN-RESTART does not; the labels must differ in
exactly that."
  (format t "~&~%Labels: a restart that asks is marked, one that does not is not.~%")
  (finish-output)
  (let ((asking nil) (plain nil))
    ;; A real unbound variable, not a synthetic RESTART-CASE.  SBCL's USE-VALUE
    ;; for this error is the one that carries :INTERACTIVE READ-EVALUATED-FORM;
    ;; a USE-VALUE written here by hand would carry no interactive function and
    ;; the test would pass or fail for a reason having nothing to do with the
    ;; panel.  PLAIN-RESTART, established around it, is the control.
    (handler-bind
        ((unbound-variable
           (lambda (condition)
             (declare (ignore condition))
             (let ((titles (restart-titles (compute-restarts))))
               (setf asking (find "[USE-VALUE]" titles :test #'search)
                     plain (find "[PLAIN-RESTART]" titles :test #'search)))
             (invoke-restart 'plain-restart))))
      (with-simple-restart (plain-restart "A restart that does not ask.")
        (eval '*a-variable-that-is-not-bound*)))
    (check (and asking (search "…" asking))
           "USE-VALUE's label ends in an ellipsis")
    (check (and plain (not (search "…" plain)))
           "a restart that does not ask carries none")))

;;; ----------------------------------------------------------------------------

(dolist (case '(case-session case-debugger case-use-value case-store-value
                case-y-or-n-p case-abort case-toplevel-restart-index
                case-interactive-restarts-are-marked))
  (funcall case))

(format t "~&~%headless-test: ~d check~:p, ~d failure~:p~%" *checks* *failures*)
(finish-output)
(sb-ext:exit :code (if (zerop *failures*) 0 1) :abort t)
