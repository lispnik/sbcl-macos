;;;; src/completion.lisp -- Tab completes the symbol before the caret.
;;;;
;;;; What this supplies is Lisp's idea of completion: a symbol token rather than
;;;; a word, which would stop at every hyphen and colon, and the symbols of the
;;;; listener's package rather than a spelling dictionary.
;;;;
;;;; The package is the one thing thread 1 cannot simply look at: *PACKAGE* is
;;;; bound on the listener thread.  EMIT-PROMPT publishes it into the listener
;;;; structure at every prompt, and LISTENER-COMPLETION-PACKAGE reads it back.
;;;;
;;;; All of it is plain Lisp, and make test exercises it.  How Tab reaches it is
;;;; the front end's business: NSTextView's own completion popup on the Mac
;;;; (src/macos/view.lisp), a key command and the key bar on iOS.

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

;;; Completing without a popup -----------------------------------------------
;;;
;;; A toolkit with no completion popup of its own -- UIKit -- completes the way
;;; a shell does: one candidate is inserted, several extend the token as far
;;; as they agree, and when they agree no further the candidates are listed
;;; above a fresh copy of the prompt.

(defun common-prefix (strings)
  "The longest prefix every one of STRINGS shares, compared exactly."
  (if (null strings)
      ""
      (reduce (lambda (a b) (subseq a 0 (or (mismatch a b) (length a))))
              strings)))

(defun complete-at-caret (view pointer)
  "Complete the symbol before the caret.  Thread 1.

Returns :INSERTED, :EXTENDED, :LISTED, or NIL when there was nothing to
complete or nothing it could be."
  (multiple-value-bind (token range) (completion-token view pointer)
    (let* ((candidates (and token (listener-completions token)))
           (prefix (common-prefix candidates)))
      (cond ((null candidates) nil)
            ((null (rest candidates))
             (replace-token pointer range (first candidates))
             :inserted)
            ((> (length prefix) (length token))
             (replace-token pointer range prefix)
             :extended)
            (t
             (list-completions view candidates)
             :listed)))))

(defun list-completions (view candidates)
  "Put CANDIDATES in the transcript, followed by the prompt again, above what
is being typed -- which stays where it is and stays editable.  Thread 1."
  (let ((prompt (and *listener* (listener-prompt *listener*))))
    (transcript-insert view (format nil "~%~{~a~^  ~}~%" candidates) :note)
    (when prompt
      (transcript-insert view prompt :prompt))))
