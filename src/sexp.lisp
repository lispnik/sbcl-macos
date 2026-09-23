;;;; src/sexp.lisp -- reading Lisp structure out of a string.
;;;;
;;;; Everything here takes a STRING and a CHARACTER OFFSET and answers offsets.
;;;; No view, no toolkit, no reader: the text in the input region is usually
;;;; half-written and unbalanced, which is exactly what CL:READ cannot help
;;;; with, and what a paren scanner can.
;;;;
;;;; COPIED, with renaming, from revl -- lispnik's own MIT-licensed editor:
;;;; PAREN-MATCH-OFFSET from revl/logic/lisp-text.lisp, and SEXP-BOUNDS,
;;;; INNER-LIST, SEXP-SPAN-AT, SEXP-SPANS, PARENT-SIBLINGS and CODE-POSITION-P
;;;; from revl/logic/app-logic.lisp.  APPLY-STRUCTURAL-EDIT is its REVL-PAREDIT.
;;;; They were already string-and-offset shaped, which is the whole reason they
;;;; could come here at all.  THE TWO COPIES ARE NOW SEPARATE: a fix here does
;;;; not reach revl, and vice versa.
;;;;
;;;; What the scanners all know, and all agree about: a `;' comment runs to the
;;;; end of the line, a "string" ends at the first unescaped quote, and #\( is a
;;;; character literal rather than a paren.  What none of them know: [ and {,
;;;; #|block comments|#, and |symbols with bars|.  Each call rescans from the
;;;; start of the string, which for one line of input is nothing.

(in-package #:lisp-listener)

;;; Scanning -------------------------------------------------------------------

(defun paren-match-offset (text target)
  "TEXT[TARGET] is ( or ).  The matching paren's offset, or NIL."
  (let ((n (length text)) (stack '()) (i 0))
    (loop while (< i n) do
      (let ((c (char text i)))
        (cond
          ((char= c #\;)
           (loop while (and (< i n) (char/= (char text i) #\Newline)) do (incf i)))
          ((char= c #\")
           (incf i)
           (loop while (< i n) do
             (let ((d (char text i)))
               (incf i)
               (cond ((char= d #\\) (incf i))
                     ((char= d #\") (return))))))
          ((and (char= c #\#) (< (1+ i) n) (char= (char text (1+ i)) #\\))
           (incf i 3))
          ((char= c #\() (push i stack) (incf i))
          ((char= c #\))
           (let ((open (and stack (pop stack))))
             (when (and open (or (= open target) (= i target)))
               (return-from paren-match-offset (if (= i target) open i))))
           (incf i))
          (t (incf i)))))
    nil))

(defun code-position-p (text position)
  "True when POSITION in TEXT is ordinary code -- not inside a string, a `;'
comment or a #\\ character literal."
  (let ((len (length text)) (i 0))
    (loop while (< i len) do
      (when (> i position) (return))
      (let ((c (char text i)))
        (cond
          ((char= c #\;)
           (let ((j i))
             (loop while (and (< i len) (char/= (char text i) #\Newline)) do (incf i))
             (when (and (<= j position) (< position i))
               (return-from code-position-p nil))))
          ((char= c #\")
           (let ((j i))
             (incf i)
             (loop while (< i len) do
               (let ((d (char text i)))
                 (incf i)
                 (cond ((char= d #\\) (incf i))
                       ((char= d #\") (return)))))
             (when (and (<= j position) (< position i))
               (return-from code-position-p nil))))
          ((and (char= c #\#) (< (1+ i) len) (char= (char text (1+ i)) #\\))
           (let ((j i))
             (incf i 3)
             (when (and (<= j position) (< position i))
               (return-from code-position-p nil))))
          (t (incf i)))))
    t))

(defun sexp-bounds (text offset)
  "(values START END) of the innermost () form containing OFFSET, or NIL.
END is exclusive of nothing: it is one past the closing paren."
  (let ((len (length text)) (stack '()) (best nil) (i 0))
    (loop while (< i len) do
      (let ((c (char text i)))
        (cond
          ((char= c #\;)
           (loop while (and (< i len) (char/= (char text i) #\Newline)) do (incf i)))
          ((char= c #\")
           (incf i)
           (loop while (< i len) do
             (let ((d (char text i)))
               (incf i)
               (cond ((char= d #\\) (incf i))
                     ((char= d #\") (return))))))
          ((and (char= c #\#) (< (1+ i) len) (char= (char text (1+ i)) #\\))
           (incf i 3))
          ((char= c #\() (push i stack) (incf i))
          ((char= c #\))
           (when stack
             (let ((start (pop stack)))
               (when (and (<= start offset) (<= offset (1+ i))
                          (or (null best) (> start (car best))))
                 (setf best (cons start (1+ i))))))
           (incf i))
          (t (incf i)))))
    (when best (values (car best) (cdr best)))))

(defun inner-list (text offset)
  "Like SEXP-BOUNDS but with an exclusive end: a position sitting just past a
form's closing `)' belongs to the ENCLOSING list, not to that form.  So a caret
in the whitespace between two siblings resolves to their parent, which is what
transposing two of them wants."
  (let ((len (length text)) (stack '()) (best nil) (i 0))
    (loop while (< i len) do
      (let ((c (char text i)))
        (cond
          ((char= c #\;)
           (loop while (and (< i len) (char/= (char text i) #\Newline)) do (incf i)))
          ((char= c #\")
           (incf i)
           (loop while (< i len) do
             (let ((d (char text i)))
               (incf i)
               (cond ((char= d #\\) (incf i))
                     ((char= d #\") (return))))))
          ((and (char= c #\#) (< (1+ i) len) (char= (char text (1+ i)) #\\))
           (incf i 3))
          ((char= c #\() (push i stack) (incf i))
          ((char= c #\))
           (when stack
             (let ((start (pop stack)))
               (when (and (<= start offset) (< offset (1+ i))
                          (or (null best) (> start (car best))))
                 (setf best (cons start (1+ i))))))
           (incf i))
          (t (incf i)))))
    (when best (values (car best) (cdr best)))))

(defun sexp-span-at (text from)
  "From FROM, skip whitespace and comments, then (values START END) of the one
sexp beginning there -- an atom, a string, or a balanced () list, with any
leading reader prefixes (' ` , ,@) -- or NIL when none remains."
  (let ((len (length text)) (i from))
    (loop while (< i len) do
      (let ((c (char text i)))
        (cond ((member c '(#\Space #\Tab #\Newline #\Return #\Page)) (incf i))
              ((char= c #\;)
               (loop while (and (< i len) (char/= (char text i) #\Newline)) do (incf i)))
              (t (return)))))
    (when (< i len)
      (let ((start i))
        (loop while (and (< i len) (member (char text i) '(#\' #\` #\,)))
              do (incf i)
                 (when (and (< i len) (char= (char text i) #\@)) (incf i)))
        (when (< i len)
          (let ((c (char text i)))
            (cond
              ((char= c #\()
               (let ((depth 0))
                 (loop while (< i len) do
                   (let ((d (char text i)))
                     (cond
                       ((char= d #\;)
                        (loop while (and (< i len) (char/= (char text i) #\Newline))
                              do (incf i)))
                       ((char= d #\")
                        (incf i)
                        (loop while (< i len) do
                          (let ((e (char text i)))
                            (incf i)
                            (cond ((char= e #\\) (incf i))
                                  ((char= e #\") (return))))))
                       ((and (char= d #\#) (< (1+ i) len)
                             (char= (char text (1+ i)) #\\))
                        (incf i 3))
                       ((char= d #\() (incf depth) (incf i))
                       ((char= d #\)) (incf i) (decf depth)
                        (when (zerop depth) (return)))
                       (t (incf i)))))))
              ((char= c #\")
               (incf i)
               (loop while (< i len) do
                 (let ((e (char text i)))
                   (incf i)
                   (cond ((char= e #\\) (incf i))
                         ((char= e #\") (return))))))
              (t
               (loop while (and (< i len)
                                (not (member (char text i)
                                             '(#\Space #\Tab #\Newline #\Return
                                               #\Page #\( #\) #\" #\;))))
                     do (incf i))))))
        (values start i)))))

(defun sexp-spans (text start &optional (limit (length text)))
  "The (START . END) spans of the successive sexps from START up to LIMIT: the
direct children of a list, or the top-level forms of a whole string."
  (let ((spans '()) (i start))
    (loop (multiple-value-bind (a b) (sexp-span-at text i)
            (if (and a (< a limit))
                (progn (push (cons a b) spans) (setf i b))
                (return))))
    (nreverse spans)))

(defun parent-siblings (text start end)
  "The spans of the form at START..END and all its siblings: the children of the
list that directly contains it, or the top-level forms when there is none."
  (or (when (> start 0)
        (multiple-value-bind (parent-start parent-end) (sexp-bounds text (1- start))
          (when (and parent-start (< parent-start start) (> parent-end end))
            (sexp-spans text (1+ parent-start) (1- parent-end)))))
      (sexp-spans text 0)))

;;; Structural edits -----------------------------------------------------------

(defun trim-left-whitespace (string)
  (string-left-trim '(#\Space #\Tab #\Newline #\Return) string))

(defun apply-structural-edit (operation text offset)
  "OPERATION at OFFSET in TEXT -> (values NEW-TEXT NEW-OFFSET), or NIL when it
does not apply -- an unbalanced line, or nothing of that shape here.

The operations are paredit's: :WRAP :SPLICE :RAISE :SLURP :BARF :SLURP-BACK
:BARF-BACK :TRANSPOSE :KILL.  src/paredit.lisp binds only some of them to keys
by default; the rest are reachable by name through *PAREDIT-KEYS*."
  (macrolet ((sub (&rest arguments) `(subseq text ,@arguments)))
    (ecase operation
      (:wrap
       (multiple-value-bind (start end) (sexp-bounds text offset)
         (when start
           (values (concatenate 'string (sub 0 start) "(" (sub start end) ")" (sub end))
                   (1+ start)))))
      (:splice
       (multiple-value-bind (start end) (sexp-bounds text offset)
         (when (and start (> end start))
           (values (concatenate 'string (sub 0 start) (sub (1+ start) (1- end)) (sub end))
                   (max start (1- offset))))))
      (:raise
       (multiple-value-bind (inner-start inner-end) (sexp-bounds text offset)
         (when inner-start
           (multiple-value-bind (outer-start outer-end)
               (sexp-bounds text (max 0 (1- inner-start)))
             (when (and outer-start (< outer-start inner-start) (>= outer-end inner-end))
               (values (concatenate 'string (sub 0 outer-start)
                                    (sub inner-start inner-end) (sub outer-end))
                       outer-start))))))
      (:slurp
       (multiple-value-bind (start end) (sexp-bounds text offset)
         (when (and start (> end start))
           (let ((close (1- end)))
             (multiple-value-bind (next-start next-end) (sexp-span-at text end)
               (declare (ignore next-start))
               (when next-end
                 (values (concatenate 'string (sub 0 close) (sub (1+ close) next-end)
                                      ")" (sub next-end))
                         offset)))))))
      (:barf
       (multiple-value-bind (start end) (sexp-bounds text offset)
         (when (and start (> (- end start) 2))
           (let ((close (1- end)) (last nil) (i (1+ start)))
             (loop (multiple-value-bind (a b) (sexp-span-at text i)
                     (if (and a (< a close))
                         (progn (setf last (cons a b) i b))
                         (return))))
             (when last
               (let* ((last-start (car last))
                      (last-end (min (cdr last) close))
                      (trimmed (string-right-trim
                                '(#\Space #\Tab #\Newline #\Return)
                                (sub (1+ start) last-start))))
                 (values (concatenate 'string (sub 0 (1+ start)) trimmed ") "
                                      (sub last-start last-end) (sub (1+ close)))
                         offset)))))))
      (:slurp-back
       (multiple-value-bind (start end) (sexp-bounds text offset)
         (when (and start (> end start))
           (let* ((siblings (parent-siblings text start end))
                  (mine (position start siblings :key #'car))
                  (previous (and mine (> mine 0) (nth (1- mine) siblings))))
             (when previous
               (values (concatenate 'string (sub 0 (car previous)) "("
                                    (sub (car previous) (cdr previous)) " "
                                    (sub (1+ start) end) (sub end))
                       (1+ (car previous))))))))
      (:barf-back
       (multiple-value-bind (start end) (sexp-bounds text offset)
         (when (and start (> (- end start) 2))
           (let ((first-child (first (sexp-spans text (1+ start) (1- end)))))
             (when first-child
               (values (concatenate 'string (sub 0 start)
                                    (sub (car first-child) (cdr first-child)) " ("
                                    (trim-left-whitespace (sub (cdr first-child) (1- end)))
                                    (sub (1- end)))
                       start))))))
      (:transpose
       (multiple-value-bind (start end) (inner-list text offset)
         (when start
           (let* ((children (sexp-spans text (1+ start) (1- end)))
                  (index (or (position-if (lambda (child)
                                            (and (<= (car child) offset)
                                                 (< offset (cdr child))))
                                          children)
                             (position-if (lambda (child) (<= (cdr child) offset))
                                          children :from-end t))))
             (when (and index (< (1+ index) (length children)))
               (let* ((a (nth index children))
                      (b (nth (1+ index) children))
                      (gap (sub (cdr a) (car b))))
                 (values (concatenate 'string (sub 0 (car a)) (sub (car b) (cdr b))
                                      gap (sub (car a) (cdr a)) (sub (cdr b)))
                         (+ (car a) (- (cdr b) (car b)) (length gap)))))))))
      (:kill
       (multiple-value-bind (start end) (sexp-span-at text offset)
         (when start
           (values (concatenate 'string (sub 0 start)
                                (string-left-trim '(#\Space #\Tab) (sub end)))
                   start)))))))
