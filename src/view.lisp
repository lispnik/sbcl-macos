;;;; src/view.lisp -- LispListenerView, and the transcript underneath it.
;;;;
;;;; One NSTextView holds the whole session: prompts, what you typed, what it
;;;; printed and what it returned, interleaved, the way a terminal REPL reads.
;;;; The text above the prompt is not editable and the text below it is, and
;;;; the boundary between them is a single integer, INPUT-START.
;;;;
;;;; INPUT-START is the whole correctness story of this file.  It is an index
;;;; into the text storage -- so it counts UTF-16 units, not Lisp characters,
;;;; and it is only ever read or written on thread 1.  Three things move it:
;;;; output arriving from the listener thread (inserted ABOVE the pending
;;;; input, which is why TRANSCRIPT-INSERT exists at all), submitting a line,
;;;; and clearing the transcript.

(in-package #:lisp-listener)

;;; Cocoa constants -----------------------------------------------------------

(defconstant +ns-view-width-and-height-sizable+ 18)
(defconstant +ns-window-style-titled+ 1)
(defconstant +ns-window-style-closable+ 2)
(defconstant +ns-window-style-mask+ 15
  "Titled, closable, miniaturizable, resizable.")
(defconstant +ns-backing-store-buffered+ 2)
(defconstant +png-file-type+ 4
  "NSBitmapImageFileTypePNG, for -representationUsingType:properties:.")

(defun %ns-string-constant (name)
  "The NSString an exported Objective-C string constant points at.

Keys like NSFontAttributeName are `extern NSString * const' -- a symbol whose
value is the pointer -- so the symbol's address has to be dereferenced once
before it is an object.  Taken from lispnik/objc's examples, where the same
function appears twice under two names because everything that builds an
attributed string needs it.

The framework has to be open first: FOREIGN-SYMBOL-POINTER searches the images
already loaded, and ENSURE-OBJC-INITIALIZED's :MODULES is what loads AppKit."
  (let ((symbol (cffi:foreign-symbol-pointer name)))
    (unless symbol
      (error "lisp-listener: the Objective-C string constant ~a is not available. ~
Was AppKit loaded?" name))
    (cffi:mem-ref symbol :pointer)))

;;; Text attributes -----------------------------------------------------------
;;;
;;; One NSDictionary per kind of text, made once and retained.  The cache holds
;;; foreign pointers, so it MUST be emptied when an image starts: a dumped core
;;; would otherwise come back holding an NSDictionary from the process that
;;; dumped it.  RESET-TRANSCRIPT-ATTRIBUTES is called from MAIN.

(defvar *transcript-attributes* (make-hash-table :test 'eq))

(defparameter *font-size* 13d0)

(defun reset-transcript-attributes ()
  (clrhash *transcript-attributes*)
  (values))

(defun transcript-color (kind)
  (ecase kind
    ((:output :input) (objc:invoke "NSColor" "textColor"))
    (:prompt (objc:invoke "NSColor" "systemBlueColor"))
    (:value (objc:invoke "NSColor" "systemGreenColor"))
    (:error (objc:invoke "NSColor" "systemRedColor"))
    (:note (objc:invoke "NSColor" "secondaryLabelColor"))))

(defun transcript-attributes (kind)
  "The retained attributes dictionary for KIND.

NSMutableDictionary plus -setObject:forKey: rather than
+dictionaryWithObjectsAndKeys:, which is variadic and would need
:VARIADIC-NUM-OF-FIXED to be called at all."
  (or (gethash kind *transcript-attributes*)
      (setf (gethash kind *transcript-attributes*)
            (let ((attributes (objc:invoke "NSMutableDictionary" "dictionary"))
                  (font (objc:invoke "NSFont" "monospacedSystemFontOfSize:weight:"
                                     *font-size* 0d0)))
              (objc:invoke attributes "setObject:forKey:"
                           font (%ns-string-constant "NSFontAttributeName"))
              (objc:invoke attributes "setObject:forKey:"
                           (transcript-color kind)
                           (%ns-string-constant "NSForegroundColorAttributeName"))
              ;; +dictionary is autoreleased and this one outlives the pool.
              (objc:retain attributes)))))

;;; The view ------------------------------------------------------------------

(objc:define-objc-class listener-text-view ()
  ((input-start :initform 0 :accessor view-input-start
                :documentation "Index in the text storage where editable text
begins.  UTF-16 units, thread 1 only.")
   (history :initform '() :accessor view-history
            :documentation "Submitted lines, newest first.")
   (history-index :initform nil :accessor view-history-index
                  :documentation "How far back RECALL-HISTORY has gone, or NIL
while a fresh line is being typed."))
  (:objc-class-name "LispListenerView")
  (:objc-superclass-name "NSTextView"))

;;; Transcript primitives -----------------------------------------------------
;;; All of these are thread 1 only, and all index arithmetic is done on values
;;; that came from the text storage.  Summing Lisp string lengths instead would
;;; be right until the first character outside the BMP and silently wrong after.

(defun transcript-storage (view)
  (objc:invoke view "textStorage"))

(defun transcript-length (view)
  (objc:invoke (transcript-storage view) "length"))

(defun transcript-substring (view start length)
  "LENGTH units of the transcript from START, as a Lisp string."
  (if (plusp length)
      (objc:ns-string-to-string
       (objc:invoke (objc:invoke (transcript-storage view) "string")
                    "substringWithRange:" (cons start length)))
      ""))

(defun make-attributed-string (string kind)
  "A +1 NSAttributedString of STRING in KIND's attributes.  Caller releases.

STRING is a Lisp string, which INVOKE converts to a temporary NSString for the
duration of the call; -initWithString:attributes: copies the characters, so the
temporary going away afterwards is fine."
  (objc:invoke (objc:invoke "NSAttributedString" "alloc")
               "initWithString:attributes:" string (transcript-attributes kind)))

(defun transcript-insert (view string kind)
  "Insert STRING above the pending input, and move INPUT-START past it.

Inserting at INPUT-START rather than appending at the end is the point.  Output
arrives while the user is already typing the next form -- a FORMAT from a
computation still running, a warning, the tail of a long print -- and appending
it would splice it into the middle of what they are typing.  Above the prompt
is where it belongs and where a terminal puts it."
  (when (plusp (length string))
    (let* ((storage (transcript-storage view))
           (attributed (make-attributed-string string kind))
           (length (objc:invoke attributed "length")))
      (objc:invoke storage "insertAttributedString:atIndex:"
                   attributed (view-input-start view))
      (objc:release attributed)
      (incf (view-input-start view) length)))
  string)

(defun transcript-append (view string kind)
  "Append STRING at the very end and leave INPUT-START after it.
What submitting a line does with the newline the user pressed."
  (let* ((storage (transcript-storage view))
         (attributed (make-attributed-string string kind)))
    (objc:invoke storage "appendAttributedString:" attributed)
    (objc:release attributed)
    (setf (view-input-start view) (transcript-length view)))
  string)

(defun scroll-to-end (view)
  (let ((end (transcript-length view)))
    (objc:invoke view "setSelectedRange:" (cons end 0))
    (objc:invoke view "scrollRangeToVisible:" (cons end 0)))
  view)

(defun apply-typing-attributes (view)
  "Make what the user types next look like input rather than like whatever
was printed last."
  (objc:invoke view "setTypingAttributes:" (transcript-attributes :input))
  view)

;;; The input region ----------------------------------------------------------

(defun pending-input (view pointer)
  "What has been typed since the last prompt and not yet submitted."
  (let ((start (view-input-start view)))
    (transcript-substring pointer start (- (transcript-length pointer) start))))

(defun replace-pending-input (view pointer string)
  "Replace the pending input with STRING.

Straight at the text storage: -insertText:replacementRange: would run the
delegate and the undo manager from inside the key handler that called us, and
neither has anything useful to do here."
  (let* ((storage (transcript-storage pointer))
         (start (view-input-start view))
         (length (- (transcript-length pointer) start)))
    (objc:invoke storage "replaceCharactersInRange:withString:"
                 (cons start length) string)
    (let ((new-length (- (transcript-length pointer) start)))
      (when (plusp new-length)
        (objc:invoke storage "setAttributes:range:"
                     (transcript-attributes :input) (cons start new-length))))
    (scroll-to-end pointer))
  string)

(defun submit-input (view pointer)
  "Send the pending input to the listener thread.

The newline the user pressed goes into the transcript here rather than through
-insertText:, and INPUT-START moves past it, so everything submitted becomes
read-only in the same breath.  Nothing is echoed: it is already on screen."
  (let ((text (pending-input view pointer)))
    (transcript-append view (string #\Newline) :input)
    (setf (view-history-index view) nil)
    (let ((trimmed (string-trim '(#\Space #\Tab #\Newline #\Return) text))
          (history (view-history view)))
      (when (and (plusp (length trimmed))
                 (not (and history (string= trimmed (first history)))))
        (push trimmed (view-history view))))
    (when *listener*
      (queue-push-string (listener-input *listener*)
                         (concatenate 'string text (string #\Newline))))
    (apply-typing-attributes pointer)
    (scroll-to-end pointer)
    text))


(defun clear-transcript (&optional (listener *listener*))
  "Empty the transcript, keeping whatever has been typed but not submitted.

Deleting the characters rather than assigning a fresh empty attributed string:
-setAttributedString: would want one at +1 that nothing here would release."
  (when listener
    (on-main-thread ()
      (let* ((view (listener-view-object listener))
             (pointer (listener-view listener))
             (pending (pending-input view pointer)))
        (objc:invoke (transcript-storage pointer) "deleteCharactersInRange:"
                     (cons 0 (transcript-length pointer)))
        (setf (view-input-start view) 0)
        (transcript-append view pending :input)
        ;; TRANSCRIPT-APPEND left INPUT-START past the pending text.  It belongs
        ;; in front of it, so what was typed stays editable.
        (setf (view-input-start view) 0)
        (apply-typing-attributes pointer)
        (scroll-to-end pointer))))
  t)

;;; History -------------------------------------------------------------------

(defun caret-index (pointer)
  (car (objc:invoke pointer "selectedRange")))

(defun caret-on-first-input-line-p (view pointer)
  (let ((start (view-input-start view))
        (caret (caret-index pointer)))
    (and (>= caret start)
         (not (find #\Newline (transcript-substring pointer start (- caret start)))))))

(defun caret-on-last-input-line-p (view pointer)
  (let ((caret (caret-index pointer))
        (end (transcript-length pointer)))
    (and (>= caret (view-input-start view))
         (not (find #\Newline (transcript-substring pointer caret (- end caret)))))))

(defun recall-history (view pointer direction)
  "Put the previous (DIRECTION -1) or next (1) history entry in the input
region.  Returns T when it did, NIL to let NSTextView move the caret instead --
which is what multi-line input needs."
  (let* ((history (view-history view))
         (count (length history))
         (current (view-history-index view)))
    (when (plusp count)
      (let ((index (if (minusp direction)
                       (if (null current) 0 (min (1+ current) (1- count)))
                       (cond ((null current) nil)
                             ((zerop current) :fresh)
                             (t (1- current))))))
        (cond
          ((null index) nil)
          ((eq index :fresh)
           (setf (view-history-index view) nil)
           (replace-pending-input view pointer "")
           t)
          (t
           (setf (view-history-index view) index)
           (replace-pending-input view pointer (nth index history))
           t))))))

;;; The Objective-C methods ---------------------------------------------------
;;;
;;; A Lisp condition must never unwind into an AppKit frame: there is no
;;; handler on the Objective-C side and the unwind aborts the process.  So
;;; every body below is wrapped, and the wrapper is a macro rather than a
;;; convention, because a convention gets forgotten exactly once.

(defmacro define-listener-method ((selector result-type &key on-error)
                                  (&rest argspecs) &body body)
  "An IMP on the listener view.  SELF is bound to the Lisp view object and
POINTER to its Objective-C pointer; BODY may not unwind."
  `(objc:define-objc-method (,selector ,result-type)
       ((self listener-text-view pointer) ,@argspecs)
     (declare (ignorable pointer ,@(mapcar #'first argspecs)))
     (handler-case (progn ,@body)
       (error (condition)
         (note "~a: ~a" ,selector condition)
         ,on-error))))

(define-listener-method ("listenerDrainQueue" :void) ()
  (drain-main-thread-queue))

(define-listener-method ("insertNewline:" :void)
    ((sender objc:objc-object-pointer))
  (submit-input self pointer))

(define-listener-method ("moveUp:" :void)
    ((sender objc:objc-object-pointer))
  (unless (and (caret-on-first-input-line-p self pointer)
               (recall-history self pointer -1))
    (objc:invoke (objc:current-super) "moveUp:" sender)))

(define-listener-method ("moveDown:" :void)
    ((sender objc:objc-object-pointer))
  (unless (and (caret-on-last-input-line-p self pointer)
               (recall-history self pointer 1))
    (objc:invoke (objc:current-super) "moveDown:" sender)))

;;; The view is its own delegate.  AppKit dispatches a delegate method through
;;; -respondsToSelector:, which a real class_addMethod'd IMP satisfies, so
;;; there is nothing to declare and no second object to keep alive.
;;;
;;; The affected range arrives as a CONS (location . length): NSRange is the
;;; one Cocoa structure this bridge represents as a cons rather than a vector,
;;; which is the LispWorks manual's inconsistency and is load bearing.
;;;
;;; On an error the edit is ALLOWED.  Refusing by default would make a bug in
;;; here look like a text view that has stopped accepting typing.

(define-listener-method ("textView:shouldChangeTextInRange:replacementString:"
                         objc:objc-bool :on-error t)
    ((text-view objc:objc-object-pointer)
     (affected cocoa:ns-range)
     (replacement objc:objc-object-pointer))
  (>= (car affected) (view-input-start self)))

(define-listener-method ("acceptsFirstResponder" objc:objc-bool :on-error t) ()
  t)
