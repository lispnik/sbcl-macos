;;;; src/keymap.lisp -- which key runs which command, and the switches.
;;;;
;;;; ONE table, read by both front ends.  A key spec is a STRING, in the
;;;; notation an Emacs user already knows: "(" ")" "\"" "Backspace", and
;;;; "C-k" "M-(" "C-)" "C-M-f" for the chords.  A binding's value is the name of
;;;; a function in src/paredit.lisp.
;;;;
;;;; Strings rather than a structure, because the table is meant to be edited by
;;;; hand -- at the listener's own prompt, or in init.lisp -- and
;;;; (setf (paredit-key "C-)") 'slurp-forward) is the whole interface.  The
;;;; front ends parse the specs; nothing else needs to.
;;;;
;;;; A bare character spec is a key that would otherwise type that character:
;;;; those go through the toolkit's text-insertion path, where the character is
;;;; visible.  Chords go through the key-command path.  PAREDIT-SELF-INSERT-KEY-P
;;;; is which is which, and the front ends divide their work on it.

(in-package #:lisp-listener)

(defparameter *paredit-enabled* t
  "Whether the structural commands and balanced insertion are in effect.
NIL leaves every key doing what the toolkit does with it, which is what the
input region was before src/paredit.lisp existed.")

(defparameter *paren-highlight-enabled* t
  "Whether the parenthesis under the caret and its partner are tinted.")

(defparameter *auto-indent-enabled* t
  "Whether Option-Return indents the new line.  NIL still breaks the line, at
column 0: the key is how a form is written over several lines at all, so it
does not stop working when the indentation is off.")

(defparameter *paredit-keys*
  '(("("         . insert-pair)
    (")"         . close-or-skip)
    ("\""        . insert-quote)
    ("Backspace" . delete-pair-backward)
    ("Delete"    . delete-pair-forward)
    ("C-M-f"     . forward-sexp)
    ("C-M-b"     . backward-sexp)
    ("C-k"       . kill-sexp)
    ("M-("       . wrap-round)
    ("M-s"       . splice)
    ("C-)"       . slurp-forward)
    ("C-}"       . barf-forward))
  "The default bindings: paredit's own, as far as they fit a one-line listener.

The commands not bound here -- SLURP-BACKWARD, BARF-BACKWARD, RAISE-SEXP,
TRANSPOSE-SEXPS -- are bound by naming them, e.g.
  (setf (paredit-key \"C-(\") 'slurp-backward)")

(defun paredit-key (spec)
  "The command bound to SPEC, or NIL."
  (cdr (assoc spec *paredit-keys* :test #'string=)))

(defun (setf paredit-key) (command spec)
  "Bind SPEC to COMMAND, or unbind it with NIL.

Refuses a command that is not one of *PAREDIT-COMMANDS*: a misspelling would
otherwise look exactly like a key that does nothing."
  (unless (or (null command) (member command *paredit-commands*))
    (error "lisp-listener: ~s is not a paredit command; see *PAREDIT-COMMANDS*."
           command))
  (let ((entry (assoc spec *paredit-keys* :test #'string=)))
    (cond ((and entry command) (setf (cdr entry) command))
          (entry (setf *paredit-keys* (remove entry *paredit-keys*)))
          (command (push (cons spec command) *paredit-keys*))))
  ;; The iOS front end builds UIKeyCommands once and keeps them; a rebinding
  ;; has to make it build them again.  NIL here on the Mac, where nothing
  ;; caches anything.
  (invalidate-key-commands)
  command)

(defun parse-key-spec (spec)
  "SPEC -> (values MODIFIERS CHARACTER), where MODIFIERS is a list of :CONTROL
and :META and CHARACTER is the key itself, or (values NIL NIL) for nonsense.

\"Backspace\" answers #\\Backspace, so the front ends can look it up like any
other key."
  (let ((modifiers '())
        (rest spec))
    (loop
      (cond ((and (> (length rest) 2) (string= "C-" (subseq rest 0 2)))
             (push :control modifiers)
             (setf rest (subseq rest 2)))
            ((and (> (length rest) 2) (string= "M-" (subseq rest 0 2)))
             (push :meta modifiers)
             (setf rest (subseq rest 2)))
            (t (return))))
    (cond ((string-equal rest "Backspace") (values modifiers #\Backspace))
          ;; Forward delete.  #\Rubout, which is DEL and not #\Backspace: the
          ;; two keys have to be told apart, since one deletes the character
          ;; behind the caret and the other the one in front.
          ((string-equal rest "Delete") (values modifiers #\Rubout))
          ((string-equal rest "Tab") (values modifiers #\Tab))
          ((= (length rest) 1) (values modifiers (char rest 0)))
          (t (values nil nil)))))

(defun paredit-self-insert-key-p (spec)
  "True when SPEC is a bare character -- a key the toolkit would otherwise turn
into text, and which the front ends therefore handle on the insertion path."
  (multiple-value-bind (modifiers character) (parse-key-spec spec)
    (and character (null modifiers))))

(defun paredit-command-for (character modifiers)
  "The command bound to CHARACTER with MODIFIERS held, or NIL.

What a front end asks once it has turned a key event into a character and a set
of modifiers.  NIL whenever paredit is switched off, so the one test covers
every key."
  (when *paredit-enabled*
    (loop for (spec . command) in *paredit-keys*
          do (multiple-value-bind (spec-modifiers spec-character)
                 (parse-key-spec spec)
               (when (and spec-character
                          (char= spec-character character)
                          (null (set-exclusive-or spec-modifiers modifiers)))
                 (return command))))))
