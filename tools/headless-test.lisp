;;;; tools/headless-test.lisp -- drive a real listener, with no Mac.
;;;;
;;;;     sbcl --script tools/headless-test.lisp       (or: make test)
;;;;     ecl --norc --shell tools/headless-test.lisp  (or: make test-ecl)
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
  ;;
  ;; The core, and the front end that runs on this Lisp: the Mac's on SBCL,
  ;; iOS's on ECL.  Nothing here reaches either -- they are stubs all the way
  ;; down -- but loading the one that ships on this Lisp keeps it honest.
  (dolist (name (append '("package" "impl" "main-thread" "queue" "listener"
                          "history" "sexp" "paredit" "keymap" "transcript"
                          "completion" "paren-highlight" "paredit-view" "streams"
                          "config"
                          "restarts" "repl")
                        #+sbcl '("macos/view" "macos/window" "macos/restarts-panel"
                                 "macos/screenshot" "macos/app")
                        #+ecl '("ios/view" "ios/restarts-sheet" "ios/app")))
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

;;; An unbound variable, on either Lisp ------------------------------------------
;;;
;;; SBCL establishes CONTINUE, USE-VALUE and STORE-VALUE around an unbound
;;; variable, in that order, in front of the listener's own ABORT -- which is
;;; what several cases below are about.  ECL establishes none of them.  So on
;;; ECL the variable is reached through MISSING-VALUE, which establishes the
;;; same three, in the same order, with interactive functions that converse on
;;; *QUERY-IO* the way SBCL's do -- ~& included, which is the point of the
;;; cases that use them -- and then signals a real UNBOUND-VARIABLE.

(defun missing-variable-source (name)
  "Source text that reads the unbound variable NAME, a string."
  #+sbcl (format nil "(symbol-value '~a)" name)
  #-sbcl (format nil "(cl-user::missing-value '~a)" name))

(defun missing-variable-form (symbol)
  #+sbcl symbol
  #-sbcl `(cl-user::missing-value ',symbol))

#-sbcl
(defun cl-user::read-evaluated-form ()
  (format *query-io* "~&Enter a form to be evaluated: ")
  (finish-output *query-io*)
  (list (eval (read *query-io*))))

#-sbcl
(defun cl-user::missing-value (name)
  (restart-case (error 'unbound-variable :name name)
    (continue ()
      :report (lambda (stream) (format stream "Retry using ~s." name))
      (symbol-value name))
    (use-value (value)
      :report "Use specified value."
      :interactive cl-user::read-evaluated-form
      value)
    (store-value (value)
      :report "Set specified value and use it."
      :interactive cl-user::read-evaluated-form
      (setf (symbol-value name) value))))

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
  (say listener (missing-variable-source "*no-such-variable-at-all*"))
  (check-text listener "Restarts:" "an unbound variable enters the debugger")
  ;; The transcript's list marks the rows that will ask, as the panel's does.
  (check-text listener (format nil "Use specified value. …")
              "USE-VALUE is marked with an ellipsis in the transcript")
  (check (not (search (format nil "top level. …") (transcript-so-far listener)))
         "ABORT, which does not ask, is not marked")
  ;; 1 is USE-VALUE, whose interactive function prompts for a form.
  (say listener "1")
  (check-text listener "Enter a form to be evaluated" "USE-VALUE prompts")
  (say listener "(* 6 7)")
  (check-text listener "42" "the value it read is the value of the form"))

(defcase case-store-value
    "STORE-VALUE: the other interactive restart on an unbound variable."
  (say listener (missing-variable-source "*another-missing-variable*"))
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

(defcase case-interrupt
    "Interrupt: out of a computation, and out of a half-read form."
  ;; The second half is a regression test.  Interrupt cleared the queue and
  ;; interrupted the thread, and for a thread parked in READ neither had any
  ;; effect: clearing leaves nothing to return, and an abort delivered by an
  ;; interrupt does not unwind a thread out of a condition wait -- on ECL not at
  ;; all.  So Interrupt did nothing at all to a form half read.
  (say listener "(loop (sleep 0.01))")
  (sleep 0.3)
  (check (abort-evaluation listener) "Interrupt reports it did something")
  (check-text listener "; Aborted." "a running computation is abandoned")
  (say listener "(+ 1 2)")
  (check-text listener "3" "and the listener evaluates again")
  ;; Now the half-read form: READ has taken "(list 1" and waits for the rest.
  (say listener "(list 1")
  (sleep 0.3)
  (let ((before (count-substring "; Aborted." (transcript-so-far listener))))
    (abort-evaluation listener)
    (check (let ((deadline (+ (get-internal-real-time)
                              (* 5 internal-time-units-per-second))))
             (loop (when (> (count-substring "; Aborted." (transcript-so-far listener))
                            before)
                     (return t))
                   (when (> (get-internal-real-time) deadline) (return nil))
                   (sleep 0.02)))
           "a half-read form is abandoned too"))
  ;; And what is typed next is read as a FRESH form, not as the rest of the
  ;; abandoned one -- which would have made the check above pass by accident.
  (say listener "(+ 40 2)")
  (check-text listener "42" "the next form is read on its own")
  (check (not (search "(1 42)" (transcript-so-far listener)))
         "and not as the tail of the form that was abandoned"))

(defcase case-nested-debugger
    "An error AT a debugger prompt opens the next level, not the top level."
  ;; A regression test.  Every evaluation at a debugger prompt ran inside the
  ;; hook's HANDLER-CASE, which handled the error, so a mistake typed at [1]
  ;; quietly dropped the listener back to CL-USER> and said so only in the log.
  (let ((log (make-string-output-stream)))
    (setf *log* log)
    (unwind-protect
         (progn
           (say listener "(error \"first\")")
           (check-text listener "[1] CL-USER>" "the first error opens level 1")
           (say listener "(error \"second\")")
           (check-text listener "[2] CL-USER>" "an error at [1] opens level 2")
           (say listener "(car 7)")
           (check-text listener "[3] CL-USER>" "and one at [2] opens level 3")
           (say listener "(+ 20 22)")
           (check-text listener "42" "the deepest level still evaluates")
           (check (not (search "; Aborted." (transcript-so-far listener)))
                  "nothing was aborted on the way down")
           ;; Restart 0 at every level here is the listener's own ABORT.
           (say listener "0")
           (check-text listener (format nil "; Aborted.~%CL-USER>")
                       "its ABORT goes all the way back to the top level")
           (check (not (search "the debugger itself failed"
                               (get-output-stream-string log)))
                  "and the debugger never reported failing"))
      (setf *log* nil))))

(defcase case-toplevel-restart-index
    "The toplevel restart is NOT index 0 -- Cancel must find it by object."
  ;; A regression test for a decision, not for a line.  On an unbound variable
  ;; SBCL establishes CONTINUE, USE-VALUE and STORE-VALUE in front of the
  ;; listener's own ABORT, so the panel's Cancel button sits at index 3.  Had
  ;; Cancel assumed index 0 -- which it does not, it looks the restart up by
  ;; object -- Escape would have invoked `[CONTINUE] Retry using
  ;; *NO-SUCH-VARIABLE*' and spun, rather than returning to the top level.
  (say listener (missing-variable-source "*yet-another-missing*"))
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
    ;; panel.  (On ECL, which has none, MISSING-VALUE's does carry one.)
    ;; PLAIN-RESTART, established around it, is the control.
    (handler-bind
        ((unbound-variable
           (lambda (condition)
             (declare (ignore condition))
             (let ((titles (restart-titles (compute-restarts))))
               (setf asking (find "[USE-VALUE]" titles :test #'search)
                     plain (find "[PLAIN-RESTART]" titles :test #'search)))
             (invoke-restart 'plain-restart))))
      (with-simple-restart (plain-restart "A restart that does not ask.")
        (eval (missing-variable-form '*a-variable-that-is-not-bound*))))
    (check (and asking (search "…" asking))
           "USE-VALUE's label ends in an ellipsis")
    (check (and plain (not (search "…" plain)))
           "a restart that does not ask carries none")))

(defun case-two-listeners ()
  "Two at once: the registry, and one transcript per listener.

What New Listener opens.  Cocoa is hollow here, so this cannot say the second
WINDOW works -- that is the macOS workflow's job -- but the half that the
window is only a face for is all here: two threads, two queues, two
transcripts, and the bookkeeping that decides which is which."
  (format t "~&~%Two listeners: each its own thread, queue and transcript.~%")
  (finish-output)
  (let ((one (make-listener))
        (two (make-listener)))
    (unwind-protect
         (progn
           (register-listener one)
           (register-listener two)
           (setf *listener* two)
           (start-listener-thread one)
           (start-listener-thread two)
           (check (= 2 (length *listeners*)) "both are registered")
           (check-text one "CL-USER>" "the first prompts")
           (check-text two "CL-USER>" "the second prompts")

           ;; The reason the whole thing works: each thread BINDS *LISTENER* to
           ;; its own, so everything it reaches answers for the right one.  The
           ;; form is true only if that binding is in place.
           (say one "(eq (lisp-listener::listener-thread lisp-listener::*listener*) (bt:current-thread))")
           (check-text one "T" "each thread speaks for its own listener")

           ;; Independence, which is the property a second window is for.
           (say one "(list :first 111)")
           (check-text one "(:FIRST 111)" "the first evaluates its own form")
           (say two "(list :second 222)")
           (check-text two "(:SECOND 222)" "the second evaluates its own form")
           (check (not (search "111" (transcript-so-far two)))
                  "the first's value is NOT in the second's transcript")
           (check (not (search "222" (transcript-so-far one)))
                  "nor the second's in the first's")

           ;; An error in one leaves the other at its own top level.
           (say two "(error \"only in the second\")")
           (check-text two "Restarts:" "an error puts the second in the debugger")
           (check (not (search "only in the second" (transcript-so-far one)))
                  "the first knows nothing of it")
           (say one "(list :first :unaffected)")
           (check-text one "(:FIRST :UNAFFECTED)" "and goes on evaluating")

           ;; Closing one: only that one goes, and *LISTENER* falls back.
           (unregister-listener two)
           (check (equal (list one) *listeners*) "unregistering takes out just it")
           (check (eq one *listener*) "*LISTENER* falls back to one that is left")
           (unregister-listener one)
           (check (null *listeners*) "and the last one leaves none"))
      (queue-set-eof (listener-input one))
      (queue-set-eof (listener-input two))
      (setf *listeners* '() *listener* nil)
      (sleep 0.1))))

(defun case-nil-is-nobody ()
  "SAME-OBJC-OBJECT-P: NIL matches nothing, which is what makes this safe here.

Off macOS every INVOKE answers NIL, so every listener's window is NIL.  A rule
under which NIL matched NIL would have LISTENER-FOR-WINDOW hand back the first
listener in the list for any window at all -- including, on a real Mac, for a
window belonging to a listener that had already gone."
  (format t "~&~%NIL is not an object: the lookup must not match on it.~%")
  (finish-output)
  (check (not (same-objc-object-p nil nil)) "NIL does not match NIL")
  (check (same-objc-object-p :a :a) "a real object matches itself")
  (check (not (same-objc-object-p :a :b)) "and does not match another")
  (let ((one (make-listener)))
    (unwind-protect
         (progn
           (register-listener one)
           (check (null (listener-for-window nil)) "no listener answers for NIL")
           (check (null (listener-for-view-object nil)) "nor for a NIL view"))
      (setf *listeners* '() *listener* nil))))

(defun check-edit (command before after label)
  "Run COMMAND on BEFORE, where | marks the caret, and compare with AFTER.

The caret is written into the strings because a command is judged on where it
leaves the caret as much as on the text -- and a test that says
\"(list |)\" reads like the thing it is testing."
  (let* ((offset (position #\| before))
         (text (remove #\| before)))
    (multiple-value-bind (new-text new-offset)
        (run-paredit-command command text offset)
      (let ((got (if new-text
                     (concatenate 'string (subseq new-text 0 new-offset) "|"
                                  (subseq new-text new-offset))
                     :declined)))
        (check (equal got after) "~a: ~s => ~s" label before got)))))

(defun case-sexp ()
  "The scanner: what is code, what matches what."
  (format t "~&~%Sexp scanning: parens, and the ones that only look like parens.~%")
  (finish-output)
  ;; 0123456789
  ;; (list (a))
  (check (= 9 (paren-match-offset "(list (a))" 0)) "the outer open matches the outer close")
  (check (= 0 (paren-match-offset "(list (a))" 9)) "and the outer close matches back")
  (check (= 8 (paren-match-offset "(list (a))" 6)) "the inner pair matches too")
  ;; A paren inside a string, a comment or a character literal is not a paren.
  (check (null (paren-match-offset "(a \"(\"" 0))
         "an open paren inside a string does not close the form")
  (check (= 7 (paren-match-offset "(a \"(\" )" 0))
         "and the real close is still found past it")
  ;; (a #\( ) ; )   -- the ( at 5 is a character literal, and the ) at 11 is
  ;; inside a comment, so the form closes at 7 and nowhere else.
  (check (= 7 (paren-match-offset "(a #\\( ) ; )" 0))
         "a character literal is not a paren, and neither is one in a comment")
  (check (not (code-position-p "(a \"x(\"" 5)) "inside a string is not code")
  (check (not (code-position-p "(a ; x(" 6)) "inside a comment is not code")
  (check (not (code-position-p "(a #\\(" 5)) "a character literal is not code")
  (check (code-position-p "(a b)" 3) "ordinary code is")
  ;; Spans and bounds.
  (multiple-value-bind (start end) (sexp-bounds "(list (a) b)" 7)
    (check (and (= start 6) (= end 9)) "the innermost form containing an offset"))
  (multiple-value-bind (start end) (sexp-span-at "  (a b) c" 0)
    (check (and (= start 2) (= end 7)) "a span skips leading whitespace"))
  (multiple-value-bind (start end) (sexp-span-at "'(a) b" 0)
    (check (and (= start 0) (= end 4)) "a reader prefix belongs to the span"))
  (check (equal '((1 . 2) (3 . 4)) (sexp-spans "(a b)" 1 4))
         "the children of a list are its spans"))

(defun case-paredit ()
  "The commands, each judged on the text and the caret it leaves."
  (format t "~&~%Paredit: balanced insertion, motion, and structure.~%")
  (finish-output)
  ;; Balanced insertion.
  (check-edit 'insert-pair "(list |" "(list (|)" "( inserts a pair")
  (check-edit 'insert-pair "(list \"a|\"" :declined "( inside a string is just a paren")
  (check-edit 'insert-quote "(list |" "(list \"|\"" "\" inserts a pair")
  (check-edit 'close-or-skip "(list (a|)" "(list (a)|" ") steps over the close paren")
  (check-edit 'close-or-skip "(list (a|" :declined
              ") with nothing to step over declines, so the paren is typed")
  (check-edit 'delete-pair-backward "(list (|)" "(list |" "Backspace takes an empty pair whole")
  (check-edit 'delete-pair-backward "(list \"|\"" "(list |" "and an empty string whole")
  (check-edit 'delete-pair-backward "(list (a)|" "(list (a)|"
              "Backspace over a MATCHED paren refuses")
  ;; The unmatched one is the whole point: the line is already unbalanced, and
  ;; deleting that paren is what fixes it.  Declining hands the key back to the
  ;; toolkit, which deletes the character in the ordinary way.
  (check-edit 'delete-pair-backward "(list (a|" :declined
              "Backspace over an UNMATCHED open paren is allowed")
  (check-edit 'delete-pair-backward "list a)|" :declined
              "and over an unmatched close paren too")
  (check-edit 'delete-pair-backward "(list a|" :declined
              "and over an ordinary character it declines")
  ;; Forward delete, by the same rules.
  (check-edit 'delete-pair-forward "(list |()" "(list |" "Delete takes an empty pair whole")
  (check-edit 'delete-pair-forward "(list |(a))" "(list |(a))"
              "Delete over a matched paren refuses")
  ;; "(list (a" -- the inner open paren has no partner, so it may go.
  (check-edit 'delete-pair-forward "(list |(a" :declined
              "Delete over an unmatched paren is allowed")
  (check-edit 'delete-pair-forward "(list |a)" :declined
              "and over an ordinary character it declines")
  ;; Motion leaves the text alone.
  (check-edit 'forward-sexp "|(a b) c" "(a b)| c" "forward-sexp steps over a form")
  (check-edit 'backward-sexp "(a b) c|" "(a b) |c" "backward-sexp steps back over one")
  ;; Structure.
  (check-edit 'kill-sexp "(list |(a b) c)" "(list |c)" "kill-sexp takes the form at the caret")
  (check-edit 'wrap-round "(list| a)" "(|(list a))"
              "wrap-round adds a pair around the form, caret inside the new one")
  (check-edit 'splice "(list (a| b))" "(list a| b)" "splice removes the parens around it")
  (check-edit 'slurp-forward "(list (a|) b)" "(list (a| b))" "slurp pulls the next form in")
  (check-edit 'barf-forward "(list (a b|))" "(list (a) |b)" "barf pushes the last form out")
  ;; And the ones that are reachable but unbound by default.
  (check-edit 'raise-sexp "(list (a|))" "|(a)" "raise-sexp replaces the enclosing form")
  (check-edit 'transpose-sexps "(list |a b)" "(list b |a)" "transpose swaps two siblings"))

(defun case-keymap ()
  "The keymap: parsing a spec, and rebinding one."
  (format t "~&~%Keymap: specs, lookup, and rebinding.~%")
  (finish-output)
  (multiple-value-bind (modifiers character) (parse-key-spec "C-)")
  (check (and (equal modifiers '(:control)) (char= character #\)))
         "C-) parses as Control and the character"))
  (multiple-value-bind (modifiers character) (parse-key-spec "C-M-f")
    (check (and (= 2 (length modifiers)) (member :meta modifiers) (char= character #\f))
           "C-M-f parses as both modifiers"))
  (multiple-value-bind (modifiers character) (parse-key-spec "Backspace")
    (check (and (null modifiers) (char= character #\Backspace))
           "Backspace parses as its character"))
  (check (null (nth-value 1 (parse-key-spec "nonsense"))) "and nonsense parses as nothing")
  (check (paredit-self-insert-key-p "(") "( is a self-inserting key")
  (check (not (paredit-self-insert-key-p "C-k")) "and C-k is not")
  (check (eq 'insert-pair (paredit-command-for #\( '())) "( is bound to INSERT-PAIR")
  (check (and (eq 'delete-pair-backward (paredit-command-for #\Backspace '()))
              (eq 'delete-pair-forward (paredit-command-for #\Rubout '())))
         "Backspace and Delete are told apart")
  (check (eq 'slurp-forward (paredit-command-for #\) '(:control)))
         "and C-) to SLURP-FORWARD")
  (check (null (paredit-command-for #\) '(:meta))) "M-) is bound to nothing")
  ;; Rebinding, and putting it back.
  (let ((original (copy-tree *paredit-keys*)))
    (unwind-protect
         (progn
           (setf (paredit-key "C-M-t") 'transpose-sexps)
           (check (eq 'transpose-sexps (paredit-command-for #\t '(:control :meta)))
                  "a new binding takes effect at once")
           (setf (paredit-key "C-M-t") nil)
           (check (null (paredit-command-for #\t '(:control :meta)))
                  "and NIL unbinds it")
           (check (null (ignore-errors (setf (paredit-key "C-M-z") 'no-such-command)))
                  "a command that does not exist is refused"))
      (setf *paredit-keys* original)))
  ;; The switch covers every key.
  (let ((*paredit-enabled* nil))
    (check (null (paredit-command-for #\( '()))
           "with paredit off, nothing is bound at all")))

(defun case-init-file ()
  "init.lisp: loaded at startup, and never fatal."
  (format t "~&~%Init file: read at startup, and a broken one does not stop it.~%")
  (finish-output)
  (let* ((directory (merge-pathnames
                     (format nil "lisp-listener-init-~d/" (get-universal-time))
                     (uiop:temporary-directory)))
         (*history-directory* directory)
         (*paren-highlight-enabled* t))
    (unwind-protect
         (progn
           (check (eq :none (load-init-file)) "no init file is not a failure")
           (ensure-directories-exist directory)
           (with-open-file (stream (init-file) :direction :output
                                               :if-exists :supersede)
             (write-string "(setf *paren-highlight-enabled* nil)" stream))
           (check (eq :loaded (load-init-file)) "an init file is loaded")
           (check (null *paren-highlight-enabled*) "and what it sets is set")
           (with-open-file (stream (init-file) :direction :output
                                               :if-exists :supersede)
             (write-string "(error \"deliberate\")" stream))
           (let ((outcome (load-init-file)))
             (check (and (consp outcome) (eq :failed (car outcome)))
                    "a broken one reports rather than signals")
             (check (search "deliberate" (cdr outcome))
                    "and says what went wrong")))
      (ignore-errors (uiop:delete-directory-tree directory :validate t)))))

(defun case-history ()
  "The history is saved as each line is submitted, and read back at the next
launch.

No listener here: this is a file, a limit and two functions.  The directory is
bound away from the front end's own -- a test has no business writing into
~/Library."
  (format t "~&~%History: saved between launches, trimmed, and never fatal.~%")
  (finish-output)
  (let* ((directory (merge-pathnames
                     (format nil "lisp-listener-test-~d/" (get-universal-time))
                     (uiop:temporary-directory)))
         (*history-directory* directory))
    (unwind-protect
         (progn
           (check (null (load-history)) "an empty history reads as no lines")
           (record-history-line "(+ 1 2)")
           (record-history-line "(list :a)")
           (check (equal (load-history) '("(list :a)" "(+ 1 2)"))
                  "what was recorded comes back, newest first")
           ;; A submitted form may be several lines long, which is why the
           ;; records are printed strings rather than the file's own lines.
           (record-history-line (format nil "(defun f ()~%  :multi)"))
           (check (equal (first (load-history))
                         (format nil "(defun f ()~%  :multi)"))
                  "a form of several lines survives the round trip")
           ;; The trim, and that it is written back rather than only applied.
           (let ((*history-limit* 2))
             (check (equal (load-history) '("(defun f ()
  :multi)" "(list :a)"))
                    "only the newest LIMIT lines are kept"))
           (let ((*history-limit* 2))
             (load-history)
             (check (= 2 (length (read-history-file (history-file))))
                    "and the file itself is trimmed, once"))
           (check (equal (load-history)
                         (list (format nil "(defun f ()~%  :multi)") "(list :a)"))
                  "so a later launch sees the trimmed list")
           ;; A view gets the saved history when it is made.
           (let ((view (make-instance (quote listener-text-view))))
             (initialize-view-history view)
             (check (equal (view-history view) (load-history))
                    "a new view starts with the saved history")
             (check (null (view-history-index view))
                    "and not part way through it")))
      (ignore-errors (uiop:delete-directory-tree directory :validate t))))
  ;; Nowhere to write: the listener must not care.  This is the iOS-style
  ;; failure -- a path that cannot be created -- and it must cost the history
  ;; and nothing else.
  (let ((*history-directory* #p"/lisp-listener-cannot-write-here/"))
    (check (string= "(+ 1 2)" (record-history-line "(+ 1 2)"))
           "recording into an unwritable place does not signal")
    (check (null (load-history))
           "and reading from one answers no lines")))

(defcase case-completion "Tab completion: the token, the candidates, the package."
  (let ((user (find-package "COMMON-LISP-USER")))
    (check (equal (symbol-completions "multiple-value-b" user) '("multiple-value-bind"))
           "one candidate completes, in the case it was typed in")
    (let ((several (symbol-completions "multiple-value-" user)))
      (check (and (> (length several) 3)
                  (every (lambda (c) (and (eql 0 (search "multiple-value-" c))
                                          (string= c (string-downcase c))))
                         several))
             "several candidates, all extending the token"))
    (check (member ":test" (symbol-completions ":tes" user) :test #'string=)
           "a leading colon completes keywords")
    (check (member "cl:car" (symbol-completions "cl:ca" user) :test #'string=)
           "pkg: completes external symbols and keeps the qualifier")
    (check (member "MAPCAR" (symbol-completions "MAPC" user) :test #'string=)
           "typing upper case keeps upper case")
    (check (null (symbol-completions "no-such-package:x" user))
           "an unknown package completes nothing")
    (check (= (symbol-token-start "(mapc #'fir") 8)
           "the token stops at #' and parentheses")
    (check (and (string= (common-prefix '("mapcan" "mapcar")) "mapca")
                (string= (common-prefix '("car")) "car")
                (string= (common-prefix '()) ""))
           "without a popup, several candidates extend to what they share"))
  (say listener "(in-package :keyword)")
  (check-text listener "KEYWORD>" "the prompt follows IN-PACKAGE")
  (check (eq (listener-completion-package listener) (find-package "KEYWORD"))
         "and so does the package completion reads"))

(defcase case-prompt-is-recorded "Clear Transcript's prompt: recorded while waiting, NIL while not."
  (flet ((prompt-becomes (text)
           (loop repeat 200
                 until (equal (listener-prompt listener) text)
                 do (sleep 0.02))
           (equal (listener-prompt listener) text)))
    (check (prompt-becomes "CL-USER> ") "waiting at the top level records its prompt")
    (say listener "(format t \"during: ~s~%\" (lisp-listener::listener-prompt lisp-listener::*listener*))")
    (check-text listener "during: NIL" "evaluating records none")
    (say listener "(error \"boom\")")
    (check (prompt-becomes "[1] CL-USER> ") "a debugger level records its own")
    (say listener "(in-package :keyword)")
    (check (prompt-becomes "[1] KEYWORD> ") "and follows IN-PACKAGE")))

;;; ----------------------------------------------------------------------------

(dolist (case '(case-session case-debugger case-use-value case-store-value
                case-y-or-n-p case-abort case-interrupt case-nested-debugger
                case-toplevel-restart-index
                case-interactive-restarts-are-marked
                case-two-listeners case-nil-is-nobody case-history case-completion
                case-sexp case-paredit case-keymap case-init-file
                case-prompt-is-recorded))
  (funcall case))

(format t "~&~%headless-test: ~d check~:p, ~d failure~:p~%" *checks* *failures*)
(finish-output)
(exit-process (if (zerop *failures*) 0 1))
