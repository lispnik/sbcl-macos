;;;; src/paredit.lisp -- structural editing, as pure functions.
;;;;
;;;; Every command here has the same shape: it takes the input region's TEXT and
;;;; the caret's CHARACTER offset into it, and answers (values NEW-TEXT
;;;; NEW-OFFSET), or NIL when it declines.  Declining is ordinary -- an
;;;; unbalanced line, nothing of that shape under the caret -- and the front end
;;;; then does whatever the key would have done without paredit, which is how
;;;; `(' still types a paren when this cannot help.
;;;;
;;;; NIL rather than an error, and no view anywhere: that is what lets the whole
;;;; of this be tested on a machine with no window (case-paredit), and what
;;;; keeps the two front ends down to converting offsets and writing text back.
;;;;
;;;; A command may not signal.  The front end calls it from inside an IMP, where
;;;; DEFINE-LISTENER-METHOD's handler would swallow the condition and answer as
;;;; though the key did nothing.

(in-package #:lisp-listener)

;;; Balanced insertion ----------------------------------------------------------
;;;
;;; The part paredit is famous for, and the part revl had no need of: it edits
;;; whole files, where the parens are already balanced.  A listener's input
;;; region is a line being typed, so these are written for that.

(defun insert-pair (text offset)
  "( inserts () and leaves the caret between them.

Inside a string or a comment it declines, so a paren typed in prose stays one
paren."
  (if (code-position-p text offset)
      (values (concatenate 'string (subseq text 0 offset) "()" (subseq text offset))
              (1+ offset))
      nil))

(defun insert-quote (text offset)
  "\" inserts a pair of them, unless we are already inside a string -- where it
closes it -- or in a comment, where it is just a character."
  (cond ((not (code-position-p text offset)) nil)
        (t (values (concatenate 'string (subseq text 0 offset) "\"\""
                                (subseq text offset))
                   (1+ offset)))))

(defun close-or-skip (text offset)
  ") steps over the close paren that is already there, rather than typing a
second one.  With none to step over it declines, and the toolkit types the
paren -- which is what an unclosed form wants."
  (when (and (code-position-p text offset)
             (< offset (length text))
             (char= (char text offset) #\)))
    (values text (1+ offset))))

(defun delete-pair-backward (text offset)
  "Backspace over the open half of an empty pair takes both halves.

`()' and `\"\"' go whole.  A lone paren with content beside it is REFUSED --
declining leaves the key to the toolkit, which would unbalance the line, so
this answers the text unchanged instead and the front end stops there."
  (when (plusp offset)
    (let ((before (char text (1- offset)))
          (after (and (< offset (length text)) (char text offset))))
      (cond
        ;; An empty pair, caret in the middle: both go.
        ((and after (or (and (char= before #\() (char= after #\)))
                        (and (char= before #\") (char= after #\"))))
         (values (concatenate 'string (subseq text 0 (1- offset))
                              (subseq text (1+ offset)))
                 (1- offset)))
        ;; A paren that is holding something up: refuse, without unbalancing.
        ((and (member before '(#\( #\))) (code-position-p text (1- offset)))
         (values text offset))
        (t nil)))))

;;; Motion ----------------------------------------------------------------------

(defun forward-sexp (text offset)
  "Past the end of the next sexp.  The text is unchanged; only the caret moves."
  (multiple-value-bind (start end) (sexp-span-at text offset)
    (declare (ignore start))
    (when end (values text end))))

(defun backward-sexp (text offset)
  "To the start of the sexp before the caret."
  (let* ((spans (sexp-spans text 0))
         (previous (find-if (lambda (span) (< (car span) offset)) (reverse spans))))
    (cond ((null previous) nil)
          ;; Inside a form: its children are what to step through.
          ((> offset (cdr previous))
           (values text (car previous)))
          (t (let ((inner (remove-if-not (lambda (span) (< (car span) offset))
                                         (sexp-spans text (car previous)))))
               (values text (car (or (first (last inner)) previous))))))))

;;; The structural commands ------------------------------------------------------
;;;
;;; Thin names over APPLY-STRUCTURAL-EDIT, so that a keymap entry and a test
;;; name a command rather than an operation keyword.

(macrolet ((define-structural-command (name operation documentation)
             `(defun ,name (text offset)
                ,documentation
                (apply-structural-edit ,operation text offset))))
  (define-structural-command kill-sexp :kill
    "Delete the sexp at the caret.")
  (define-structural-command wrap-round :wrap
    "Wrap the form at the caret in a new pair of parens.")
  (define-structural-command splice :splice
    "Remove the parens around the form at the caret, keeping its contents.")
  (define-structural-command slurp-forward :slurp
    "Pull the next sexp in through this form's closing paren.")
  (define-structural-command barf-forward :barf
    "Push this form's last sexp out past its closing paren.")
  (define-structural-command slurp-backward :slurp-back
    "Pull the previous sexp in through this form's opening paren.")
  (define-structural-command barf-backward :barf-back
    "Push this form's first sexp out past its opening paren.")
  (define-structural-command raise-sexp :raise
    "Replace the enclosing form with the form at the caret.")
  (define-structural-command transpose-sexps :transpose
    "Swap the sexp at the caret with the one after it."))

(defparameter *paredit-commands*
  '(insert-pair insert-quote close-or-skip delete-pair-backward
    forward-sexp backward-sexp
    kill-sexp wrap-round splice slurp-forward barf-forward
    slurp-backward barf-backward raise-sexp transpose-sexps)
  "Every command a key may be bound to.  Checked when a binding is set, so a
misspelling is refused at the prompt rather than silently doing nothing.")

(defun run-paredit-command (command text offset)
  "Run COMMAND, answering (values TEXT OFFSET) or NIL.  Never signals: a
command that breaks declines, and the key falls through to the toolkit."
  (when (and command (fboundp command))
    (handler-case (funcall command text offset)
      (error (condition)
        (note "paredit ~a: ~a" command condition)
        nil))))
