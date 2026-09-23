;;;; src/paren-highlight.lisp -- the parenthesis under the caret, and its partner.
;;;;
;;;; When the caret rests just after a `)' or just before a `(', both it and its
;;;; partner are given a background tint.  A paren with no partner is tinted as
;;;; a mismatch instead, which is how a missing one gets noticed while it is
;;;; still one keystroke from being fixed.
;;;;
;;;; ONLY INSIDE THE INPUT REGION.  The transcript above the prompt is other
;;;; people's parens -- output, prompts, printed values -- and matching across
;;;; that boundary would both mislead and mark text that is not being edited.
;;;; So the scan runs over PENDING-INPUT and every offset is relative to
;;;; INPUT-START.
;;;;
;;;; Recomputed from scratch, never incrementally, and the ranges it marked last
;;;; time are kept on the view so they can be cleared.  That is deliberate:
;;;; REPLACE-PENDING-INPUT and REPLACE-TOKEN both reset the attributes over
;;;; their whole range with -setAttributes:range:, and output arriving from the
;;;; listener thread shifts every index, so any attempt to keep a highlight
;;;; alive across an edit would be wrong about half the time.  Cheap enough:
;;;; one line of input, two one-character attribute writes.

(in-package #:lisp-listener)

(defun clear-paren-highlight (view pointer)
  "Remove the tint from wherever it was last put.  Thread 1."
  (let ((storage (transcript-storage pointer))
        (length (transcript-length pointer)))
    (dolist (range (view-paren-marks view))
      ;; The text may have shrunk since: a range past the end is a range that
      ;; no longer exists, and TextKit must not be handed it.
      (when (and length (<= (+ (car range) (cdr range)) length))
        (objc:invoke storage "removeAttribute:range:"
                     (%ns-string-constant "NSBackgroundColorAttributeName")
                     range))))
  (setf (view-paren-marks view) '())
  view)

(defun mark-paren (view pointer offset kind)
  "Tint the one character at OFFSET, an index into the whole transcript."
  (let ((range (cons offset 1)))
    (objc:invoke (transcript-storage pointer) "addAttribute:value:range:"
                 (%ns-string-constant "NSBackgroundColorAttributeName")
                 (paren-background-color kind)
                 range)
    (push range (view-paren-marks view)))
  view)

(defun caret-paren (text offset)
  "The offset of the paren the caret is resting on, or NIL.

After a `)' first -- which is where the caret is when you have just typed one --
and otherwise before a `('.  Parens inside strings and comments are not parens
for this purpose, which is what CODE-POSITION-P answers."
  (cond ((and (plusp offset)
              (<= offset (length text))
              (char= (char text (1- offset)) #\))
              (code-position-p text (1- offset)))
         (1- offset))
        ((and (< offset (length text))
              (char= (char text offset) #\()
              (code-position-p text offset))
         offset)
        (t nil)))

(defun refresh-paren-highlight (view pointer)
  "Put the tint where the caret is now.  Thread 1; safe to call on every
keystroke and every selection change, and safe when there is no view at all."
  (handler-case
      (when (and view pointer)
        (clear-paren-highlight view pointer)
        (when *paren-highlight-enabled*
          (let* ((start (view-input-start view))
                 (caret (caret-index pointer)))
            (when (and start caret (>= caret start))
              (let* ((text (pending-input view pointer))
                     (offset (utf-16-offset->index text (- caret start)))
                     (paren (caret-paren text offset)))
                (when paren
                  (let ((partner (paren-match-offset text paren)))
                    (flet ((mark (index kind)
                             (mark-paren view pointer
                                         (+ start (utf-16-length (subseq text 0 index)))
                                         kind)))
                      (cond (partner
                             (mark paren :match)
                             (mark partner :match))
                            (t (mark paren :mismatch)))))))))))
    (error (condition)
      ;; A highlight is decoration.  It may not take the keystroke down with it.
      (note "paren highlight: ~a" condition)
      nil))
  view)
