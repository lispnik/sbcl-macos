;;;; src/impl.lisp -- SBCL or ECL, and the front end's half of the contract.
;;;;
;;;; The listener runs on SBCL on the Mac and on ECL on iOS, and this is the
;;;; only file in src/ that says which.  Everything here is either an SBCL
;;;; internal with an ECL counterpart, or a name the front end -- src/macos/
;;;; or src/ios/ -- promises to define.
;;;;
;;;; (The Gray stream package is the other difference, and it is settled in
;;;; package.lisp by a local nickname, because a nickname has to exist before
;;;; the reader meets the first qualified symbol.)

(in-package #:lisp-listener)

;;; SBCL and ECL ---------------------------------------------------------------

(defmacro with-invoke-debugger-hook ((hook) &body body)
  "Run BODY with the implementation's own debugger hook bound to HOOK.

Both SBCL and ECL null CL:*DEBUGGER-HOOK* before calling it, so that a hook
which itself errors cannot loop; a nested error raised while the debugger is
already up then finds it empty.  Each has a second hook that is not nulled, and
that is what catches the nested one.  See LISTENER-LOOP."
  `(let ((#+sbcl sb-ext:*invoke-debugger-hook*
          #+ecl ext:*invoke-debugger-hook*
          ,hook))
     ,@body))

(defun backtrace-frames (count)
  "Up to COUNT frames beneath the debugger, innermost first, each a list whose
first element names the function.

On SBCL, :FROM :DEBUGGER-FRAME is what SBCL's own debugger uses: it starts at
the frame that signalled and skips INVOKE-DEBUGGER and the hooks.  ECL has no
such option, so its frames are cut after INVOKE-DEBUGGER by hand, and only the
function is known -- ECL's frame stack does not keep the arguments."
  #+sbcl (sb-debug:list-backtrace :count count :from :debugger-frame)
  #+ecl
  (let* ((frames (loop for index from (si::ihs-top) downto 1
                       collect (list (ecl-function-name (si::ihs-fun index)))))
         (debugger (position 'invoke-debugger frames :key #'first)))
    (subseq frames (if debugger (1+ debugger) 0)
            (min (length frames) (+ (if debugger (1+ debugger) 0) count)))))

#+ecl
(defun ecl-function-name (function)
  (or (ignore-errors
       (if (functionp function)
           (nth-value 2 (function-lambda-expression function))
           function))
      function))

(defun restart-interactive-function (restart)
  "RESTART's interactive function, or NIL.  An internal on both, guarded:
losing it costs an ellipsis in the restart list and nothing else."
  (ignore-errors
   #+sbcl (sb-kernel::restart-interactive-function restart)
   #+ecl (si::restart-interactive-function restart)))

(defun exit-process (code)
  "Leave now, without unwinding: the caller has nothing left to clean up and a
thread still blocked in READ would otherwise hold the process open."
  #+sbcl (sb-ext:exit :code code :abort t)
  #+ecl (ext:quit code))

(defun getenv (name)
  "The environment variable NAME, or NIL.  Not UIOP's: an iOS app is linked
without ASDF, and so without UIOP."
  #+sbcl (sb-ext:posix-getenv name)
  #+ecl (ext:getenv name))

(defun safepoint-build-p ()
  "True where stopping the world cannot kill the process from a libdispatch
thread.

On SBCL that means a build --with-sb-safepoint: such a build stops the world by
polling rather than by signalling, which is what makes it safe for Lisp to run
on a thread Darwin will not let anyone signal.  AppKit reaches libdispatch on
its own, so a Cocoa application wants one whether or not it uses GCD itself.
See lispnik/objc's doc/sbcl-libdispatch-safepoint.md.

ECL never signals a thread to collect garbage, so the question does not arise."
  #+sbcl (and (member :sb-safepoint *features*) t)
  #-sbcl t)

;;; The front end ----------------------------------------------------------------
;;;
;;; Defined in src/macos/ or src/ios/, which load after everything here.  The
;;; core calls them; neither front end is loaded at the same time as the other.

(declaim (ftype function
                ;; An NSColor or UIColor for a kind of transcript text, and the
                ;; monospaced font it is set in.
                transcript-color transcript-font
                ;; The run loop modes a hop to the main thread is queued in.
                main-thread-run-loop-modes
                ;; Where the history file lives, or NIL for nowhere.
                history-directory
                ;; The tint for a matched (:MATCH) or unmatched (:MISMATCH)
                ;; parenthesis, and the chance to rebuild cached key commands
                ;; after *PAREDIT-KEYS* changes.
                paren-background-color invalidate-key-commands
                ;; The restarts, on screen: build and show, take down, ask.
                show-restarts-panel hide-restarts-panel restarts-panel-visible-p
                ;; LISTENER-TEXT-VIEW's slot accessors.  The class is the front
                ;; end's -- its superclass is NSTextView or UITextView -- and
                ;; the transcript in the core reads and writes its slots.
                view-input-start (setf view-input-start)
                view-history (setf view-history)
                view-history-index (setf view-history-index)
                view-paren-marks (setf view-paren-marks)))

;;; Defined later in the core than the file that first calls them.  A :SERIAL
;;; system tolerates a forward reference; the compile check, which compiles each
;;; file on its own, reports one as a style warning without these.
(declaim (ftype function refresh-paren-highlight clear-paren-highlight))
