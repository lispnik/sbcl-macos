;;;; src/completion.lisp -- Tab completes the symbol before the caret.
;;;;
;;;; NSTextView already has completion: -complete: asks the view for the range
;;;; being completed (-rangeForUserCompletion) and for the candidates
;;;; (-completionsForPartialWordRange:indexOfSelectedItem:), then shows its own
;;;; popup.  All this file supplies is Lisp's idea of both -- a symbol token
;;;; rather than a word, which would stop at every hyphen and colon, and the
;;;; symbols of the listener's package rather than a spelling dictionary.
;;;;
;;;; The package is the one thing thread 1 cannot simply look at: *PACKAGE* is
;;;; bound on the listener thread.  EMIT-PROMPT publishes it into the listener
;;;; structure at every prompt, and LISTENER-COMPLETION-PACKAGE reads it back.
;;;;
;;;; Everything above the IMPs is plain Lisp, and make test exercises it.

(in-package #:lisp-listener)

;;; Tokens and candidates ------------------------------------------------------

(defun symbol-constituent-p (char)
  (not (or (member char '(#\Space #\Tab #\Newline #\Return #\Page))
           (find char "()'`\",;|#"))))

(defun symbol-token-start (text &optional (end (length text)))
  "Where the symbol token ending at END in TEXT begins."
  (let ((start end))
    (loop while (and (plusp start) (symbol-constituent-p (char text (1- start))))
          do (decf start))
    start))

(defun utf-16-length (string)
  "STRING's length as NSString counts it: a character outside the BMP is two."
  (loop for char across string
        sum (if (> (char-code char) #xFFFF) 2 1)))

(defun completion-case (typed name)
  "NAME as the user would have typed it: lower case, unless they typed upper
case themselves or the name is not all upper case to begin with."
  (if (and (notany #'upper-case-p typed)
           (string= name (string-upcase name)))
      (string-downcase name)
      name))

(defun symbol-completions (token package)
  "Every completion of TOKEN in PACKAGE, as full replacement strings, sorted.

TOKEN may be qualified.  A leading colon completes keywords; `pkg:' completes
PKG's external symbols and `pkg::' all of its symbols, and the qualifier is
kept exactly as typed.  An unknown package has no completions."
  (let* ((colon (position #\: token :from-end t))
         (name (if colon (subseq token (1+ colon)) token))
         (qualifier (if colon (subseq token 0 (1+ colon)) ""))
         (package-name (and colon (string-right-trim ":" qualifier)))
         (internal (and colon (> (length qualifier) 1)
                        (string= "::" qualifier :start2 (- (length qualifier) 2))))
         (home (cond ((null colon) package)
                     ((string= package-name "") (find-package "KEYWORD"))
                     (t (find-package (string-upcase package-name)))))
         (results '()))
    (flet ((consider (symbol)
             (let ((symbol-name (symbol-name symbol)))
               (when (and (>= (length symbol-name) (length name))
                          (string-equal name symbol-name :end2 (length name)))
                 (push (concatenate 'string qualifier
                                    (completion-case name symbol-name))
                       results)))))
      (cond ((null home))
            ((or (null colon) internal) (do-symbols (symbol home) (consider symbol)))
            (t (do-external-symbols (symbol home) (consider symbol)))))
    (sort (remove-duplicates results :test #'string=) #'string<)))

;;; The token under the caret --------------------------------------------------

(defun completion-token (view pointer)
  "The symbol token ending at the caret, and its NSRange as (start . length).
NIL when the caret is outside the input region or there is no token there."
  (let* ((start (view-input-start view))
         (caret (caret-index pointer)))
    (when (and caret (>= caret start))
      (let* ((before (transcript-substring pointer start (- caret start)))
             (token (subseq before (symbol-token-start before))))
        (when (plusp (length token))
          (let ((length (utf-16-length token)))
            (values token (cons (- caret length) length))))))))

(defun listener-completions (token)
  "TOKEN's completions in the listener's package.  None for an empty token:
Escape after a space would otherwise offer every symbol in the package."
  (and (plusp (length token))
       (symbol-completions token (listener-completion-package *listener*))))

(defun replace-token (pointer range string)
  "Replace RANGE with STRING, as input, and put the caret after it."
  (let ((storage (transcript-storage pointer))
        (start (car range)))
    (objc:invoke storage "replaceCharactersInRange:withString:" range string)
    (let ((end (+ start (utf-16-length string))))
      (objc:invoke storage "setAttributes:range:"
                   (transcript-attributes :input) (cons start (- end start)))
      (objc:invoke pointer "setSelectedRange:" (cons end 0))
      (objc:invoke pointer "scrollRangeToVisible:" (cons end 0))))
  string)

;;; The Objective-C methods ---------------------------------------------------
;;;
;;; Tab goes to SUPER's -complete:, never to the view's own.  That override,
;;; in restarts.lisp, is Escape's, and it first cancels any debugger level --
;;; which Tab must not do.  Escape still completes, through its fall-through.
;;;
;;; One candidate is inserted straight away rather than offered in a popup of
;;; one.  None, or several, go to -complete:, which beeps for none.

(define-listener-method ("insertTab:" :void)
    ((sender objc:objc-object-pointer))
  (multiple-value-bind (token range) (completion-token self pointer)
    (let ((candidates (and token (listener-completions token))))
      (cond ((null token)
             (objc:invoke (objc:current-super) "insertTab:" sender))
            ((and candidates (null (rest candidates)))
             (replace-token pointer range (first candidates)))
            (t
             (objc:invoke (objc:current-super) "complete:" sender))))))

(define-listener-method ("rangeForUserCompletion" cocoa:ns-range) ()
  (multiple-value-bind (token range) (completion-token self pointer)
    (if token
        range
        (objc:invoke (objc:current-super) "rangeForUserCompletion"))))

;;; The array is autoreleased: an object a Lisp method returns is the caller's
;;; to release, and AppKit does not expect to own this one.  *INDEX is left at
;;; AppKit's own default, which selects the first candidate.

(define-listener-method ("completionsForPartialWordRange:indexOfSelectedItem:"
                         objc:objc-object-pointer)
    ((range cocoa:ns-range)
     (index :pointer))
  (let ((token (transcript-substring pointer (car range) (cdr range)))
        (array (objc:invoke "NSMutableArray" "array")))
    (dolist (candidate (listener-completions token) array)
      (objc:invoke array "addObject:" candidate))))
