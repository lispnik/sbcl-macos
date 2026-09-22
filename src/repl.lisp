;;;; src/repl.lisp -- the listener thread: read, eval, print, and the debugger.
;;;;
;;;; This runs on an ordinary SBCL thread.  It never touches AppKit; everything
;;;; it shows goes through the output stream, which hops to thread 1 for it.
;;;;
;;;; The streams are bound HERE rather than set globally, and that is not
;;;; tidiness.  asdf-macos-app's %app-toplevel LET-binds *STANDARD-OUTPUT* to
;;;; the bundle's log file around the entry point, and a thread created later
;;;; starts from the GLOBAL value -- so a listener that relied on inheriting
;;;; the binding would quietly write the whole session into the log file.

(in-package #:lisp-listener)

(defvar +eof+ (list :eof)
  "READ's end-of-file value.  A fresh object, so that no form a user types can
be mistaken for it -- and ONE object: a quoted #:EOF written in two places is
two different uninterned symbols and never EQ to itself.")

(defun package-short-name (package)
  "The shortest of PACKAGE's names, which is what a prompt wants."
  (first (sort (cons (package-name package) (copy-list (package-nicknames package)))
               #'< :key #'length)))

(defun emit-prompt (listener)
  "Write the prompt and leave the caret after it.

FRESH-LINE rather than an unconditional newline: after a form that printed a
trailing newline of its own, an extra blank line before every prompt is the
sort of thing that makes a listener feel unfinished."
  (let ((stream (listener-output listener))
        (level (listener-debug-level listener)))
    ;; Published for completion, which runs on thread 1 and so cannot see this
    ;; thread's binding.  At the prompt because that is where IN-PACKAGE shows.
    (setf (listener-package listener) *package*)
    (let ((prompt (format nil "~:[~*~;[~d] ~]~a> " (plusp level) level
                          (package-short-name *package*))))
      (with-output-kind (stream :prompt)
        (fresh-line stream)
        (write-string prompt stream))
      (setf (listener-prompt listener) prompt)))
  ;; The prompt has been handed to thread 1; the typing attributes have to
  ;; follow it there, or what is typed next comes out prompt-coloured.
  (let ((pointer (listener-view listener)))
    (when pointer
      (on-main-thread () (apply-typing-attributes pointer))))
  (values))

;;; Values --------------------------------------------------------------------

(defun shift-values (form values)
  "Rotate the standard history variables, in the order CL specifies.
SETF is sequential, so oldest first is not a stylistic choice."
  (setf /// //
        // /
        / values
        *** **
        ** *
        * (first values)
        +++ ++
        ++ +
        + form)
  values)

(defun print-values (listener values)
  (let ((stream (listener-output listener)))
    (with-output-kind (stream :value)
      (if (null values)
          (format stream "~&; No values~%")
          (dolist (value values)
            (fresh-line stream)
            (handler-case (prin1 value stream)
              (error (condition)
                (format stream "#<unprintable ~a: ~a>" (type-of value) condition)))
            (terpri stream)))))
  values)

;;; The debugger --------------------------------------------------------------

(defvar *toplevel-restart* nil
  "The ABORT restart that returns to the listener's top level.

Captured as the restart OBJECT, not as a number.  The panel's Cancel has to
take this exact restart, and its position in COMPUTE-RESTARTS' list is whatever
the handlers in between make it -- assuming index 0 would be right today and
wrong the first time anyone's own handler established one nearer.")

(defparameter *backtrace-enabled* t
  "Whether entering the debugger also shows a backtrace.")

(defparameter *backtrace-frames* 12
  "How many frames to show.  SBCL's own REPL shows a similar handful; the rest
is almost always this program rather than yours.")

(defun frame-line (frame)
  "One frame, on one line.

SB-DEBUG hands back the call as a form, and left to itself the pretty printer
spreads a wide one over five lines, which turns a twelve-frame backtrace into a
page.  Depth and length are capped instead, the way a debugger caps them."
  (let ((*print-pretty* nil)
        (*print-level* 3)
        (*print-length* 6)
        (*print-circle* t)
        (*print-readably* nil))
    (handler-case (princ-to-string frame)
      (error () "(unprintable frame)"))))

(defun listener-frame-p (frame)
  "Whether FRAME is the listener evaluating a form, rather than the form.

The frame's OPERATOR, never its printed text.  Matching the printed line for
the name was the first attempt and it is wrong: a frame whose ARGUMENTS happen
to print that name truncates the backtrace at itself, so evaluating something
as innocent as (list \"LISTENER-REP\") would give a mysteriously short
backtrace.  Found by a test whose own label string contained the name, which
made the trim appear to work for entirely the wrong reason."
  (and (consp frame)
       (symbolp (first frame))
       (eq (first frame) 'listener-rep)))

(defun trim-listener-frames (frames count)
  "Cut FRAMES where the listener's own machinery begins, and cap the rest.

Everything below LISTENER-REP is this program evaluating your form -- EVAL, the
read loop, the thread function -- and it is the same frames every time, on
every error."
  (let ((end (or (position-if #'listener-frame-p frames) (length frames))))
    (subseq frames 0 (min end count))))

(defun capture-backtrace (&optional (count *backtrace-frames*))
  "The frames beneath the error, as numbered strings.

CAPTURED HERE, inside the hook, because here is the only place the stack still
exists: by the time a restart has been chosen it has been unwound.

:FROM :DEBUGGER-FRAME is what SBCL's own debugger uses and is what makes the
result readable -- it starts at the frame that signalled and skips
INVOKE-DEBUGGER, RUN-HOOK and this hook itself, which are ours and are never
what anyone wants to see first.  A few extra frames are asked for because
TRIM-LISTENER-FRAMES may drop some."
  (handler-case
      ;; Trimmed as FRAMES and printed afterwards: the decision is about which
      ;; function a frame is, which only the frame itself can answer.
      (let* ((raw (sb-debug:list-backtrace :count (+ count 8) :from :debugger-frame))
             (frames (trim-listener-frames raw count)))
        (loop for frame in frames
              for index from 0
              collect (format nil "~2d: ~a" index (frame-line frame))))
    (error (condition)
      (list (format nil "(the backtrace could not be taken: ~a)" condition)))))

(defun print-backtrace-lines (listener lines)
  (when lines
    (let ((stream (listener-output listener)))
      (with-output-kind (stream :note)
        (format stream "~&Backtrace:~%")
        (dolist (line lines)
          (format stream "  ~a~%" line))))))

(defun print-restarts (listener restarts)
  "The numbered list in the transcript.

The ellipsis marks the restarts that will stop and ask for a value, the same
as the panel's rows do -- see RESTART-ASKS-P.  Both doors open on the same
list, so both have to say the same thing about it; marking only the one with
buttons would make typing 3 and clicking row 3 look like different acts."
  (let ((stream (listener-output listener)))
    (with-output-kind (stream :error)
      (format stream "~&Restarts:~%")
      (loop for restart in restarts
            for index from 0
            do (format stream "  ~2d: [~a] ~a~@[~a~]~%"
                       index
                       (or (restart-name restart) "ANONYMOUS")
                       (handler-case (princ-to-string restart)
                         (error () "(unprintable restart)"))
                       (and (restart-asks-p restart) " …"))))))

(defun drain-pending-whitespace (stream)
  "Consume whitespace already buffered on STREAM, without ever blocking.

READ leaves the newline that followed a form sitting in the queue.  The
debugger reads LINES rather than forms, so that stray newline would come back
as an empty first line -- and the user would see a second debugger prompt
appear immediately, for no reason they could have caused."
  (loop while (and (listen stream)
                   (member (peek-char nil stream nil nil)
                           '(#\Space #\Tab #\Newline #\Return)))
        do (read-char stream nil nil))
  stream)

(defun listener-debugger (listener condition)
  "Show CONDITION and its restarts, then read at a nested level.

A number picks a restart, anything else is evaluated here, so a debugger level
is a working listener too.

THIS MUST NOT RETURN.  If *DEBUGGER-HOOK* returns, INVOKE-DEBUGGER falls
through to SBCL's own debugger, which would then try to converse on *DEBUG-IO*
-- this same window -- from underneath us.  Every path out of here either
transfers control through a restart or aborts to the top level."
  (let ((stream (listener-output listener))
        (restarts (compute-restarts condition))
        (saved (listener-debug-level listener)))
    (with-output-kind (stream :error)
      (format stream "~&~%~a~%  [Condition of type ~a]~%"
              (report-condition condition) (type-of condition)))
    (print-restarts listener restarts)
    (let ((backtrace (and *backtrace-enabled* (capture-backtrace))))
      (print-backtrace-lines listener backtrace)
      ;; The panel is an ADDITION: the numbered list above is still printed and
      ;; the prompt below still takes a number.  Clicking a button types that
      ;; number, so both doors lead to the same READ-LINE.
      (offer-restarts listener condition restarts backtrace
                      (position *toplevel-restart* restarts)))
    (drain-pending-whitespace *standard-input*)
    (setf (listener-debug-level listener) (1+ saved))
    ;; No ABORT restart is established here on purpose.  Aborting means "back to
    ;; the top level", from however deep, so CL:ABORT should find the toplevel
    ;; one and not a nearer one of ours -- and a nearer one is also how the EOF
    ;; case below used to spin: (abort) invoked the restart established by this
    ;; very iteration, the loop went round, and READ-LINE returned NIL again.
    (unwind-protect
         (loop
           (emit-prompt listener)
           (let ((line (read-line *standard-input* nil nil)))
             (setf (listener-prompt listener) nil)
             ;; End of input: the window has gone and nothing can ever be read
             ;; again.  Leave, and let the abort below unwind to the top level.
             (when (null line) (return))
             (let ((selection (restart-selection line (length restarts))))
               (cond
                 (selection (take-restart (nth selection restarts)))
                 (t
                  ;; READ-FROM-STRING on a blank line signals END-OF-FILE, which
                  ;; would reach the hook and open a further debugger level --
                  ;; so pressing Return at a debugger prompt would descend a
                  ;; level each time.  Nothing typed, nothing to do.
                  (multiple-value-bind (form position)
                      (read-from-string line nil +eof+)
                    (declare (ignore position))
                    (unless (eq form +eof+)
                      (print-values listener
                                    (multiple-value-list (eval form))))))))))
      (setf (listener-debug-level listener) saved)
      (withdraw-restarts listener))
    ;; THIS MUST NOT RETURN; see the docstring.  Reached only on end of input.
    (ignore-errors (abort))
    (note "the listener's input ended inside the debugger.")))

(defun restart-selection (line count)
  "The restart LINE names, or NIL when it is a form to evaluate instead.

READ-LINE rather than READ, so that a restart which goes on to read a value of
its own starts from a clean line rather than from the rest of ours."
  (let ((trimmed (string-trim '(#\Space #\Tab #\Return) line)))
    (when (and (plusp (length trimmed))
               (every #'digit-char-p trimmed))
      (let ((index (parse-integer trimmed :junk-allowed t)))
        (when (and index (< -1 index count))
          index)))))

(defun take-restart (restart)
  "Invoke RESTART, asking for whatever it needs.

INVOKE-RESTART-INTERACTIVELY is right for every restart, not only the ones
with an interactive function: a restart that has none is simply called with no
arguments.  It converses on *QUERY-IO*, which this thread has bound to the
window, so STORE-VALUE and USE-VALUE ask there rather than nowhere."
  (invoke-restart-interactively restart))

;;; The loop ------------------------------------------------------------------

(defun print-banner (listener)
  (let ((stream (listener-output listener)))
    (with-output-kind (stream :note)
      (format stream "~a ~a~%" (lisp-implementation-type) (lisp-implementation-version))
      (if (safepoint-build-p)
          (format stream "Safepoint build: callbacks on libdispatch threads are safe.~%")
          (format stream "WARNING: this SBCL is not a --with-sb-safepoint build.~@
                          A garbage collection while two libdispatch worker threads are~@
                          inside Lisp takes the process down with no condition and no~@
                          backtrace.  AppKit reaches libdispatch on its own.  See~@
                          lispnik/objc doc/sbcl-libdispatch-safepoint.md.~%"))))
  (values))

(defun listener-rep (listener)
  "One read, eval, print.  Returns NIL when the input has ended for good.
Errors go to the debugger hook, not to here."
  (emit-prompt listener)
  (let ((form (read *standard-input* nil +eof+)))
    ;; No longer waiting at it: a Clear Transcript from here until the next
    ;; prompt must not put one back.
    (setf (listener-prompt listener) nil)
    (cond
      ((eq form +eof+) nil)
      (t
       ;; The view appended the newline the user pressed, so the transcript is
       ;; already at the start of a line -- but the output stream last wrote the
       ;; prompt and still believes it is nine columns in.  Without this,
       ;; FRESH-LINE below emits a newline that is already on screen and every
       ;; single value gets a blank line above it.
       (setf (stream-column (listener-output listener)) 0)
       (setf - form)
       (let ((values (multiple-value-list (eval form))))
         (shift-values form values)
         (print-values listener values))
       t))))

(defun listener-loop (listener)
  (let* ((input (listener-input-stream-for listener))
         (output (listener-output listener))
         (io (make-two-way-stream input output))
         (debugger (lambda (condition hook)
                     (declare (ignore hook))
                     (handler-case (listener-debugger listener condition)
                       (error (inner)
                         (note "the debugger itself failed: ~a" inner)
                         (abort))))))
    (let ((*standard-input* input)
          (*standard-output* output)
          (*error-output* output)
          (*trace-output* output)
          (*query-io* io)
          (*debug-io* io)
          (*terminal-io* io)
          (*package* (find-package "COMMON-LISP-USER"))
          ;; BOTH hooks, and that is not belt and braces.  SBCL's
          ;; INVOKE-DEBUGGER sets *DEBUGGER-HOOK* to NIL before calling it, so
          ;; that a hook which itself errors cannot loop -- which means a
          ;; nested error, raised while the debugger is already up, finds
          ;; *DEBUGGER-HOOK* empty.  SB-EXT:*INVOKE-DEBUGGER-HOOK* is not
          ;; nulled, so it is what catches the nested one.  Bind only the
          ;; standard hook and debugger levels below the first fall through to
          ;; SBCL's own debugger, on *DEBUG-IO*, which is this window.
          ;;
          ;; Measured rather than assumed: with both bound, an error raised
          ;; from inside the hook came back reporting `*debugger-hook* is now
          ;; NIL' and was handled anyway.
          (*debugger-hook* debugger)
          (sb-ext:*invoke-debugger-hook* debugger))
      (print-banner listener)
      (loop
        ;; The only ABORT restart in the whole listener, so that aborting from
        ;; any depth -- a nested debugger level, a form interrupted by the
        ;; Interrupt menu item -- lands here and nowhere in between.
        ;;
        ;; LIVE starts true, so an abort that unwinds past the SETF leaves it
        ;; true and the loop goes round again; only a clean NIL from
        ;; LISTENER-REP, which means end of input, stops it.
        (let ((live t))
          ;; WITH-SIMPLE-RESTART answers (VALUES NIL T) when its restart was
          ;; taken, which is how the loop can tell "the form finished" from
          ;; "the form was abandoned" -- and it is worth telling, because
          ;; otherwise an interrupt leaves no trace at all.
          (multiple-value-bind (ignored aborted)
              (with-simple-restart (abort "Return to the listener's top level.")
                ;; FIND-RESTART inside the form finds the innermost ABORT,
                ;; which is the one just established.
                (let ((*toplevel-restart* (find-restart 'abort)))
                  (setf live (listener-rep listener))))
            (declare (ignore ignored))
            (when aborted (note-abort listener)))
          (unless live (return)))))))

(defun note-abort (listener)
  "Say that the last form was abandoned.

Without this, interrupting a computation shows the form and then a fresh
prompt, with nothing at all to distinguish it from a form that simply returned
no values."
  (let ((stream (listener-output listener)))
    (with-output-kind (stream :note)
      (format stream "~&; Aborted.~%"))))

(defun start-listener-thread (listener)
  "Start the listener.  Called LAST, once there is a view to talk to.

UNWIND-PROTECT, and never HANDLER-CASE.  A handler for ERROR established out
here HANDLES the condition, and a handled condition never reaches
INVOKE-DEBUGGER -- so *DEBUGGER-HOOK* would not run, and an error in an
evaluated form would quietly restart the listener instead of showing its
restarts.  The whole debugger would be dead code and nothing would say so.

That is not hypothetical.  This function had exactly that shape, the debugger
had never once run, and the first CI run that reached it reported
`the listener restarted after: The value 7 is not of type LIST' where a restart
list should have been.  It is the reason the screenshot of the debugger is
worth taking: it is the only check here that exercises the path at all.

Nothing should escape LISTENER-LOOP in any case -- the hook it binds does not
return.  If something does, the thread ends and says so on the way out."
  (setf (listener-thread listener)
        (bt:make-thread
         (lambda ()
           ;; Bound here so that everything the thread reaches -- the debugger,
           ;; the restarts panel, ABORT-EVALUATION's default -- answers for
           ;; THIS listener.  A thread started while another window was in
           ;; front would otherwise inherit that window's listener as the
           ;; global value and quietly act on it.
           (let ((*listener* listener))
             (unwind-protect (listener-loop listener)
               (note "the listener thread has ended."))))
         :name "lisp listener"))
  listener)

(defun abort-evaluation (&optional (listener *listener*))
  "Return the listener to its top level, whatever it is doing.

Both halves are needed.  Clearing the queue wakes a thread parked in READ;
the interrupt reaches one that is off in a computation of its own.  ABORT is
wrapped because a thread caught between the two restarts has none, and
CL:ABORT with nothing to abort to signals CONTROL-ERROR."
  (let ((thread (and listener (listener-thread listener))))
    (when (and thread (bt:thread-alive-p thread))
      (queue-clear (listener-input listener))
      (bt:interrupt-thread thread (lambda () (ignore-errors (abort))))
      t)))
