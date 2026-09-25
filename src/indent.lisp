;;;; src/indent.lisp -- a newline, indented the way Lisp is indented.
;;;;
;;;; NEWLINE-AND-INDENT is a command of the same shape as the ones in
;;;; src/paredit.lisp -- (text offset) -> (values text offset) -- so it runs
;;;; through the same glue and make test covers it the same way.  On the Mac it
;;;; is Option-Return, which AppKit's standard key bindings already send to
;;;; -insertNewlineIgnoringFieldEditor:; on iOS it is a key command for the same
;;;; chord.  Plain Return still submits.
;;;;
;;;; Three rules, which between them are nearly everything typed at a listener:
;;;;
;;;;   (defun foo (n)          a BODY form: two past its paren, once its
;;;;     ...)                  distinguished arguments are all written
;;;;
;;;;   (list a                 an ordinary call with an argument on its first
;;;;         b)                line: under that argument
;;;;
;;;;   ((a b)                  anything else -- data, a list in operator
;;;;    (c d))                 position, a call with nothing after it: one past
;;;;
;;;; Which operators take a body is looked up in the live image: a macro whose
;;;; lambda list has &BODY says so itself, which covers the user's own macros
;;;; with no table to maintain.  The table below is for what cannot say --
;;;; special operators have no lambda list, and HANDLER-CASE and DEFMETHOD are
;;;; &REST where their shape is really a body -- and Emacs's own rule that
;;;; anything named DEF... is indented like DEFUN.
;;;;
;;;; Columns are TRANSCRIPT columns.  The input region's first line starts after
;;;; the prompt, so a paren there is further right than its offset into the
;;;; input says; *INDENT-FIRST-COLUMN* is how far, and the front end binds it.

(in-package #:lisp-listener)

(defvar *indent-first-column* 0
  "The transcript column the input region's first character sits in -- the
width of the prompt in front of it.")

(defvar *indent-package* nil
  "The package an operator is looked up in, to ask whether it takes a body.
NIL means *PACKAGE*.")

(defparameter *indent-body-counts*
  '(("BLOCK" . 1) ("CATCH" . 1) ("EVAL-WHEN" . 1) ("FLET" . 1) ("LABELS" . 1)
    ("MACROLET" . 1) ("SYMBOL-MACROLET" . 1) ("LET" . 1) ("LET*" . 1)
    ("LOCALLY" . 0) ("PROGN" . 0) ("PROGV" . 2) ("TAGBODY" . 0)
    ("UNWIND-PROTECT" . 1) ("MULTIPLE-VALUE-PROG1" . 1) ("LAMBDA" . 1)
    ("HANDLER-CASE" . 1) ("HANDLER-BIND" . 1) ("RESTART-CASE" . 1)
    ("RESTART-BIND" . 1) ("CASE" . 1) ("ECASE" . 1) ("CCASE" . 1)
    ("TYPECASE" . 1) ("ETYPECASE" . 1) ("CTYPECASE" . 1)
    ("DESTRUCTURING-BIND" . 2) ("MULTIPLE-VALUE-BIND" . 2)
    ("WITH-SLOTS" . 2) ("WITH-ACCESSORS" . 2) ("PRINT-UNREADABLE-OBJECT" . 1))
  "Operator name -> how many distinguished arguments precede its body, for the
operators whose lambda list cannot say.  By NAME, as Emacs does it, so a
shadowing symbol of the same name indents the same way.")

;;; Where things are ---------------------------------------------------------------

(defun innermost-open-paren (text offset)
  "The offset of the innermost ( before OFFSET that is not closed before it,
or NIL at top level.  The second value is true when OFFSET is inside a string,
where a newline is part of the string and no indentation belongs."
  (let ((stack '()) (i 0))
    (loop while (< i offset) do
      (let ((c (char text i)))
        (cond
          ((char= c #\;)
           (loop while (and (< i offset) (char/= (char text i) #\Newline)) do (incf i)))
          ((char= c #\")
           (incf i)
           (loop
             (when (>= i offset)
               (return-from innermost-open-paren (values (first stack) t)))
             (let ((d (char text i)))
               (incf i)
               (cond ((char= d #\\) (incf i))
                     ((char= d #\") (return))))))
          ((and (char= c #\#) (< (1+ i) offset) (char= (char text (1+ i)) #\\))
           (incf i 3))
          ((char= c #\() (push i stack) (incf i))
          ((char= c #\)) (pop stack) (incf i))
          (t (incf i)))))
    (values (first stack) nil)))

(defun text-column (text position)
  "The transcript column of POSITION in TEXT, the input region."
  (let ((newline (position #\Newline text :end position :from-end t)))
    (if newline
        (- position newline 1)
        (+ *indent-first-column* position))))

(defun same-line-p (text a b)
  (not (find #\Newline text :start (min a b) :end (max a b))))

(defun list-elements (text open offset)
  "The (START . END) spans of the elements of the list opening at OPEN that
begin before OFFSET."
  (let ((spans '()) (i (1+ open)) (visible (subseq text 0 offset)))
    (loop (multiple-value-bind (start end) (sexp-span-at visible i)
            (if (and start (< start offset) (< start end))
                (progn (push (cons start end) spans) (setf i end))
                (return))))
    (nreverse spans)))

;;; Which operators take a body ----------------------------------------------------

(defun token-symbol (token)
  "The symbol TOKEN names, if it exists; never interns.  NIL for a keyword, a
number, or anything else that is not an operator name."
  (unless (or (zerop (length token)) (char= (char token 0) #\:)
              (digit-char-p (char token 0)))
    (let* ((colon (position #\: token))
           (package (if colon
                        (find-package (string-upcase (subseq token 0 colon)))
                        (or *indent-package* *package*)))
           (name (string-upcase (string-left-trim ":" (subseq token (or colon 0))))))
      (and package (find-symbol name package)))))

(defun lambda-list-body-count (lambda-list)
  "How many arguments precede &BODY in LAMBDA-LIST, or NIL if it has none."
  (let ((count 0) (list lambda-list))
    (loop
      (when (atom list) (return nil))
      (let ((item (pop list)))
        (cond ((eq item '&body) (return count))
              ;; Each takes the variable after it, which is no argument.
              ((member item '(&whole &environment)) (pop list))
              ((member item lambda-list-keywords))
              (t (incf count)))))))

(defun operator-body-count (token)
  "How many distinguished arguments the operator TOKEN takes before its body:
0 or more for a body form, :DEFINITION for anything named DEF..., or NIL for an
ordinary call.

A WITH-... macro whose lambda list does not say is taken to have one, as
nearly all of them do; ECL's WITH-OPEN-FILE is &REST, for one."
  (let* ((name (string-upcase (subseq token (1+ (or (position #\: token :from-end t)
                                                     -1)))))
         (known (assoc name *indent-body-counts* :test #'string=))
         (prefixp (lambda (prefix)
                    (and (> (length name) (length prefix))
                         (string= prefix name :end2 (length prefix))))))
    (cond (known (cdr known))
          ((funcall prefixp "DEF") :definition)
          ((let ((symbol (token-symbol token)))
             (and symbol (macro-function symbol)
                  (ignore-errors
                   (lambda-list-body-count (macro-lambda-list symbol))))))
          ((funcall prefixp "WITH-") 1))))

;;; The indentation ----------------------------------------------------------------

(defun indentation-at (text offset)
  "The column a new line begun at OFFSET in TEXT should start in."
  (let ((open (innermost-open-paren text offset)))
    (if (null open)
        0
        (let* ((column (text-column text open))
               (elements (list-elements text open offset))
               (operator (first elements))
               (token (and operator (subseq text (car operator) (cdr operator)))))
          (cond
            ;; A quoted list is data, whatever its first element looks like.
            ((and (plusp open) (find (char text (1- open)) "'`"))
             (1+ column))
            ((or (null operator) (not (token-symbol-like-p token)))
             (1+ column))
            (t
             (let ((count (operator-body-count token))
                   (arguments (rest elements)))
               (cond
                 ((eq count :definition) (+ column 2))
                 ((and count (>= (length arguments) count)) (+ column 2))
                 ;; Still among a body form's distinguished arguments, or an
                 ;; ordinary call: under the first argument if it is on the
                 ;; operator's line.
                 ((and arguments (same-line-p text (car operator) (car (first arguments))))
                  (text-column text (car (first arguments))))
                 (count (+ column 4))
                 (t (1+ column))))))))))

(defun token-symbol-like-p (token)
  "True when TOKEN could name an operator: not a list, a string, a keyword or a
number."
  (and (plusp (length token))
       (not (find (char token 0) "(\"':#`,"))
       (not (digit-char-p (char token 0)))))

(defun newline-and-indent (text offset)
  "Break the line at OFFSET and indent the new one.  Never declines.

Spaces either side of the break go: trailing ones would be left dangling on the
line above, and leading ones would push the rest of the line past the column
this chose.  Inside a string nothing is touched but the newline itself."
  (multiple-value-bind (open in-string) (innermost-open-paren text offset)
    (declare (ignore open))
    (if in-string
        (values (concatenate 'string (subseq text 0 offset) (string #\Newline)
                             (subseq text offset))
                (1+ offset))
        (let* ((before (string-right-trim '(#\Space #\Tab) (subseq text 0 offset)))
               (after (string-left-trim '(#\Space #\Tab) (subseq text offset)))
               (indent (if *auto-indent-enabled*
                           (indentation-at before (length before))
                           0))
               (head (concatenate 'string before (string #\Newline)
                                  (make-string indent :initial-element #\Space))))
          (values (concatenate 'string head after) (length head))))))
