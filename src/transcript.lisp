;;;; src/transcript.lisp -- the transcript, on either platform's text view.
;;;;
;;;; One text view holds the whole session: prompts, what you typed, what it
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
;;;;
;;;; Everything here goes through -textStorage, -selectedRange,
;;;; -scrollRangeToVisible: and -typingAttributes, which NSTextView and
;;;; UITextView both have under those names, so it is shared by the two front
;;;; ends.  The view CLASS is not: LISTENER-TEXT-VIEW, with the same slots, is
;;;; defined over NSTextView in src/macos/view.lisp and over UITextView in
;;;; src/ios/view.lisp, and each defines the methods its toolkit sends it.

(in-package #:lisp-listener)

(defun %ns-string-constant (name)
  "The NSString an exported Objective-C string constant points at.

Keys like NSFontAttributeName are `extern NSString * const' -- a symbol whose
value is the pointer -- so the symbol's address has to be dereferenced once
before it is an object.  Taken from lispnik/objc's examples, where the same
function appears twice under two names because everything that builds an
attributed string needs it.

The framework has to be open first: FOREIGN-SYMBOL-POINTER searches the images
already loaded.  On the Mac ENSURE-OBJC-INITIALIZED's :MODULES loads AppKit;
on iOS UIKit is linked into the app."
  (let ((symbol (cffi:foreign-symbol-pointer name)))
    (unless symbol
      (error "lisp-listener: the Objective-C string constant ~a is not available. ~
Was AppKit or UIKit loaded?" name))
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

(defun transcript-attributes (kind)
  "The retained attributes dictionary for KIND.

NSMutableDictionary plus -setObject:forKey: rather than
+dictionaryWithObjectsAndKeys:, which is variadic and would need
:VARIADIC-NUM-OF-FIXED to be called at all."
  (or (gethash kind *transcript-attributes*)
      (setf (gethash kind *transcript-attributes*)
            (let ((attributes (objc:invoke "NSMutableDictionary" "dictionary"))
                  (font (transcript-font *font-size*)))
              (objc:invoke attributes "setObject:forKey:"
                           font (%ns-string-constant "NSFontAttributeName"))
              (objc:invoke attributes "setObject:forKey:"
                           (transcript-color kind)
                           (%ns-string-constant "NSForegroundColorAttributeName"))
              ;; +dictionary is autoreleased and this one outlives the pool.
              (objc:retain attributes)))))

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
           (length (objc:invoke attributed "length"))
           (at (view-input-start view))
           (pointer (objc:objc-object-pointer view))
           (caret (caret-index pointer)))
      (objc:invoke storage "insertAttributedString:atIndex:" attributed at)
      (objc:release attributed)
      (incf (view-input-start view) length)
      ;; A caret in the input region has to move with it.  NSTextView carries
      ;; a caret at the insertion point along by itself; UITextView leaves it
      ;; where it was -- in front of the output, on the line the user was not
      ;; typing on.  So it is moved here only if the toolkit did not.
      (when (and caret (>= caret at) (eql (caret-index pointer) caret))
        (objc:invoke pointer "setSelectedRange:" (cons (+ caret length) 0)))))
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
        (push trimmed (view-history view))
        ;; Saved as it is submitted, not when the application quits; see
        ;; src/history.lisp.
        (record-history-line trimmed)))
    ;; THIS view's listener, worked out from the view, rather than whichever
    ;; one *LISTENER* happens to name.  The IMP that calls this binds it
    ;; correctly, but SUBMIT-INPUT is also called directly -- by the screenshot
    ;; driver and the self-test -- and a function that reads the ambient value
    ;; puts one window's keystrokes on another window's input queue.
    (let ((listener (or (listener-for-view-object view) *listener*)))
      (when listener
        (queue-push-string (listener-input listener)
                           (concatenate 'string text (string #\Newline)))))
    (apply-typing-attributes pointer)
    (scroll-to-end pointer)
    text))


(defun clear-transcript (&optional (listener *listener*))
  "Empty the transcript, keeping whatever has been typed but not submitted.

If the listener is waiting at a prompt, the cleared window starts with that
prompt, so what is still typed sits after it as it did before.  While a form
is being evaluated there is no prompt to put back, and none is invented: the
next one arrives when the form finishes.

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
        (let ((prompt (listener-prompt listener)))
          (when prompt
            (transcript-append view prompt :prompt)))
        (let ((start (view-input-start view)))
          (transcript-append view pending :input)
          ;; TRANSCRIPT-APPEND left INPUT-START past the pending text.  It
          ;; belongs in front of it, so what was typed stays editable.
          (setf (view-input-start view) start))
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

;;; Defining the Objective-C methods --------------------------------------------
;;;
;;; A Lisp condition must never unwind into an AppKit or UIKit frame: there is no
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
     (handler-case
         ;; Bound, not read: every IMP here speaks for the listener whose view
         ;; it is, which is not necessarily the one in front.  A key press
         ;; arrives at the window that has the keyboard, but a drain hop is
         ;; delivered to whichever view was handy, and output bound for a
         ;; background window must not be submitted to the front one.
         (let ((*listener* (or (listener-for-view-object self) *listener*)))
           ,@body)
       (error (condition)
         (note "~a: ~a" ,selector condition)
         ,on-error))))

(defun input-edit-allowed-p (view range)
  "Whether an edit of RANGE, a (location . length) cons, may go ahead: only in
the input region.  Both front ends' should-change delegate methods answer this.

The range arrives as a CONS: NSRange is the one Cocoa structure the bridge
represents as a cons rather than a vector, which is the LispWorks manual's
inconsistency and is load bearing."
  (>= (car range) (view-input-start view)))
