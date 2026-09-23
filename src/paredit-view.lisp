;;;; src/paredit-view.lisp -- running a paredit command on a text view.
;;;;
;;;; The one place a command meets a view, and the only place the two index
;;;; spaces meet: the commands in src/paredit.lisp count Lisp characters, a text
;;;; view counts UTF-16 units.  Both front ends come through here, so both get
;;;; the conversion, the read-only guard and the highlight refresh from one
;;;; piece of code, and neither needs to know how a command is shaped.
;;;;
;;;; Separate from src/paredit.lisp because that file is pure and this one is
;;;; not: this is what the headless test cannot reach, and keeping the two apart
;;;; is what keeps the pure half worth testing.

(in-package #:lisp-listener)

(defun run-paredit-at-caret (view pointer command)
  "Run COMMAND on the input region.  True when it did something.

NIL means the key was not handled and the caller should let the toolkit have
it.  The caret must be in the input region: paredit does not edit the
transcript, and a caret above the prompt is somebody reading, not typing."
  (when (and *paredit-enabled* command view pointer)
    (let ((start (view-input-start view))
          (caret (caret-index pointer)))
      (when (and start caret (>= caret start))
        (let* ((text (pending-input view pointer))
               (offset (utf-16-offset->index text (- caret start))))
          (multiple-value-bind (new-text new-offset)
              (run-paredit-command command text offset)
            (when new-text
              (unless (string= new-text text)
                (replace-pending-input view pointer new-text))
              (let ((caret-units (+ start (utf-16-length (subseq new-text 0 new-offset)))))
                (objc:invoke pointer "setSelectedRange:" (cons caret-units 0))
                (objc:invoke pointer "scrollRangeToVisible:" (cons caret-units 0)))
              (refresh-paren-highlight view pointer)
              t)))))))

(defun paredit-handles-character-p (view pointer character &optional (modifiers '()))
  "Run whatever CHARACTER with MODIFIERS is bound to, and say whether it was.

The front ends' insertion hooks call this with no modifiers, and their key
handlers with them."
  (run-paredit-at-caret view pointer
                        (paredit-command-for character modifiers)))
