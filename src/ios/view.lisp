;;;; src/ios/view.lisp -- LispListenerView, over UITextView.
;;;;
;;;; The transcript is src/transcript.lisp, shared with the Mac.  What UIKit
;;;; needs besides is here, and it differs from AppKit in more than names:
;;;;
;;;;   - There is no -insertNewline: to override.  Return arrives as the text
;;;;     view's delegate being asked whether "\n" may replace a range, which
;;;;     is also where the input region is enforced.  The delegate method is
;;;;     -textView:shouldChangeTextInRange:replacementTEXT: -- AppKit's is
;;;;     replacementSTRING:, and implementing that name here does nothing.
;;;;
;;;;   - Tab, the arrows, Escape and the Command shortcuts only reach an
;;;;     application through UIKeyCommands, and a text view keeps Tab and the
;;;;     arrows for itself unless each command asks for priority.  Most phones
;;;;     have no keyboard at all, so the same actions are also on a bar above
;;;;     the on-screen one.
;;;;
;;;;   - UIKit resets the typing attributes whenever the selection moves, so
;;;;     they are put back each time; otherwise what is typed after a tap takes
;;;;     the colour of whatever was tapped.

(in-package #:lisp-listener)

;;; Colours, font, run loop ---------------------------------------------------

(defun history-directory ()
  "The app's Documents directory, which is HOME here.

From the environment, NOT from USER-HOMEDIR-PATHNAME: asdf-ios-app's ECLBoot
points HOME at Documents -- the one directory the app may write -- before
cl_boot, and it is where its own console.log goes.  ECL\'s
USER-HOMEDIR-PATHNAME ignores HOME and asks the password database, which in the
simulator answers the MAC\'s home directory: the history was read from and
written to ~/Library on the development machine, and nothing appeared in the
container at all.  Measured, once the simulator reported a history three lines
long on a first run."
  (let ((home (getenv "HOME")))
    (when home
      (pathname (concatenate 'string (string-right-trim "/" home) "/")))))

(defun transcript-color (kind)
  (ecase kind
    ((:output :input) (objc:invoke "UIColor" "labelColor"))
    (:prompt (objc:invoke "UIColor" "systemBlueColor"))
    (:value (objc:invoke "UIColor" "systemGreenColor"))
    (:error (objc:invoke "UIColor" "systemRedColor"))
    (:note (objc:invoke "UIColor" "secondaryLabelColor"))))

(defun transcript-font (size)
  (objc:invoke "UIFont" "monospacedSystemFontOfSize:weight:" size 0d0))

(defun paren-background-color (kind)
  "The tint behind a parenthesis: a quiet fill for a matched pair, red for one
with no partner."
  (ecase kind
    (:match (objc:invoke "UIColor" "systemFillColor"))
    (:mismatch (objc:invoke (objc:invoke "UIColor" "systemRedColor")
                            "colorWithAlphaComponent:" 0.35d0))))

(defparameter +ios-run-loop-modes+
  #("NSDefaultRunLoopMode" "UITrackingRunLoopMode")
  "The default mode, and the one the main thread runs while a scroll view is
being dragged -- so output still arrives while the transcript is scrolled by
hand.  Named one by one, never as the common-modes pseudo mode; see
src/macos/view.lisp.  UIKit has no NSModalPanelRunLoopMode.")

(defun main-thread-run-loop-modes () +ios-run-loop-modes+)

;;; The view ------------------------------------------------------------------

(objc:define-objc-class listener-text-view ()
  ((input-start :initform 0 :accessor view-input-start
                :documentation "Index in the text storage where editable text
begins.  UTF-16 units, thread 1 only.")
   (history :initform '() :accessor view-history
            :documentation "Submitted lines, newest first.")
   (history-index :initform nil :accessor view-history-index
                  :documentation "How far back RECALL-HISTORY has gone, or NIL
while a fresh line is being typed.")
   (paren-marks :initform '() :accessor view-paren-marks
                :documentation "The ranges the paren highlight last tinted, so
that they can be untinted.  Thread 1 only; see src/paren-highlight.lisp."))
  (:objc-class-name "LispListenerView")
  (:objc-superclass-name "UITextView")
  (:objc-protocols "UITextViewDelegate"))

;;; UIKit enumerations, by value.
(defconstant +ui-text-autocorrection-no+ 1)
(defconstant +ui-text-autocapitalization-none+ 0)
(defconstant +ui-text-smart-no+ 1
  "UITextSmartQuotesTypeNo, and the same value for dashes and insert/delete.")
(defconstant +ui-text-spell-checking-no+ 1)
(defconstant +ui-keyboard-ascii-capable+ 1)
(defconstant +ui-key-modifier-command+ #x100000)
(defconstant +ui-key-modifier-control+ #x40000)
(defconstant +ui-key-modifier-alternate+ #x80000)
(defconstant +ui-view-flexible-width+ 2)

(defun make-listener-view ()
  "Allocate a LispListenerView set up for Lisp source.
Returns (VALUES POINTER OBJECT)."
  (let* ((object (make-instance 'listener-text-view
                                :init-function
                                (lambda (pointer &rest initargs)
                                  (declare (ignore initargs))
                                  (objc:invoke pointer "initWithFrame:"
                                               (vector 0d0 0d0 0d0 0d0)))
                                :allow-other-keys t))
         (view (objc:objc-object-pointer object)))
    (objc:invoke view "setTranslatesAutoresizingMaskIntoConstraints:" nil)
    ;; Every substitution off, for the reason the Mac turns them off: a smart
    ;; quote is not a STRING delimiter, and an autocorrected symbol is a
    ;; different symbol.
    (objc:invoke view "setAutocorrectionType:" +ui-text-autocorrection-no+)
    (objc:invoke view "setAutocapitalizationType:" +ui-text-autocapitalization-none+)
    (objc:invoke view "setSmartQuotesType:" +ui-text-smart-no+)
    (objc:invoke view "setSmartDashesType:" +ui-text-smart-no+)
    (objc:invoke view "setSmartInsertDeleteType:" +ui-text-smart-no+)
    (objc:invoke view "setSpellCheckingType:" +ui-text-spell-checking-no+)
    (objc:invoke view "setKeyboardType:" +ui-keyboard-ascii-capable+)
    (objc:invoke view "setDataDetectorTypes:" 0)
    (objc:invoke view "setEditable:" t)
    (objc:invoke view "setSelectable:" t)
    (objc:invoke view "setAlwaysBounceVertical:" t)
    (objc:invoke view "setBackgroundColor:"
                 (objc:invoke "UIColor" "systemBackgroundColor"))
    (objc:invoke view "setTypingAttributes:" (transcript-attributes :input))
    (initialize-view-history object)
    ;; Its own delegate, as on the Mac.  The delegate property is weak, and
    ;; the view is held by its superview and by the listener, so nothing more
    ;; is needed to keep it.
    (objc:invoke view "setDelegate:" view)
    (let ((bar (make-key-bar object)))
      (objc:invoke view "setInputAccessoryView:" bar)
      ;; -setInputAccessoryView: retains it; the +1 from -alloc is ours.
      (objc:release bar))
    (values view object)))

;;; The key bar -----------------------------------------------------------------

(defparameter *key-bar-height* 44d0)

(defun make-key-bar (object)
  "The row of keys above the on-screen keyboard: Tab, Esc, the arrows, Clear
and Stop.  Each does what its hardware key does.

A frame rather than constraints: an input accessory view is sized by the
keyboard from its frame's height, and stretched across from its autoresizing
mask."
  (let ((bar (objc:invoke (objc:invoke "UIInputView" "alloc")
                          "initWithFrame:inputViewStyle:"
                          (vector 0d0 0d0 0d0 *key-bar-height*)
                          0))                   ; UIInputViewStyleDefault
        (stack (uikit:new "UIStackView")))
    (objc:invoke bar "setAutoresizingMask:" +ui-view-flexible-width+)
    (objc:invoke stack "setAxis:" 0)              ; horizontal
    (objc:invoke stack "setDistribution:" 1)      ; fill equally
    (objc:invoke bar "addSubview:" stack)
    (uikit:pin stack "leadingAnchor" bar "leadingAnchor" 4)
    (uikit:pin stack "trailingAnchor" bar "trailingAnchor" -4)
    (uikit:pin stack "topAnchor" bar "topAnchor")
    (uikit:pin stack "bottomAnchor" bar "bottomAnchor")
    (flet ((key (title function)
             (let ((button (uikit:system-button title)))
               (objc:invoke (objc:invoke button "titleLabel") "setFont:"
                            (uikit:mono-font 16))
               (uikit:on-tap button
                          (lambda (sender)
                            (declare (ignore sender))
                            (let ((*listener* (or (listener-for-view-object object)
                                                  *listener*)))
                              (funcall function))))
               (objc:invoke stack "addArrangedSubview:" button))))
      (key "Tab" (lambda () (key-tab object)))
      (key "Esc" (lambda () (key-escape)))
      (key "↑" (lambda () (key-arrow object -1)))
      (key "↓" (lambda () (key-arrow object 1)))
      (key "Clear" (lambda () (clear-transcript *listener*)))
      (key "Stop" (lambda () (abort-evaluation *listener*))))
    bar))

;;; What the keys do ------------------------------------------------------------
;;;
;;; Shared by the hardware key commands and the key bar.  All thread 1.

(defun view-pointer (object)
  (objc:objc-object-pointer object))

(defun key-tab (object)
  (complete-at-caret object (view-pointer object)))

(defun key-escape ()
  (cancel-to-top-level))

(defun key-arrow (object direction)
  "Up or down: the history when the caret is on the input's first or last line,
as on the Mac, and otherwise a line up or down within what is being typed."
  (let ((pointer (view-pointer object)))
    (if (minusp direction)
        (unless (and (caret-on-first-input-line-p object pointer)
                     (recall-history object pointer -1))
          (move-caret-line object pointer -1))
        (unless (and (caret-on-last-input-line-p object pointer)
                     (recall-history object pointer 1))
          (move-caret-line object pointer 1)))))

(defun move-caret-line (view pointer direction)
  "Move the caret one line up or down within the input, keeping its column
where the line is long enough.  What NSTextView's -moveUp: does for free and
UITextView gives up once the arrow keys are claimed for the history."
  (let* ((start (view-input-start view))
         (caret (caret-index pointer)))
    (when (>= caret start)
      (let* ((text (pending-input view pointer))
             (here (utf-16-offset->index text (- caret start)))
             (line-start (1+ (or (position #\Newline text :end here :from-end t) -1)))
             (column (- here line-start))
             (target
               (if (minusp direction)
                   (when (plusp line-start)
                     (let* ((end (1- line-start))
                            (begin (1+ (or (position #\Newline text :end end
                                                                    :from-end t)
                                           -1))))
                       (min (+ begin column) end)))
                   (let ((newline (position #\Newline text :start here)))
                     (when newline
                       (let* ((begin (1+ newline))
                              (end (or (position #\Newline text :start begin)
                                       (length text))))
                         (min (+ begin column) end)))))))
        (when target
          (objc:invoke pointer "setSelectedRange:"
                       (cons (+ start (utf-16-length (subseq text 0 target))) 0)))))))

;;; Hardware key commands --------------------------------------------------------

(defvar *key-commands* nil
  "The retained NSArray -keyCommands answers.  Made on first request, at run
time: a pointer made at load time would not survive into the app.")

(defun key-command (input selector &optional (modifiers 0))
  (let ((command (objc:invoke "UIKeyCommand" "keyCommandWithInput:modifierFlags:action:"
                              input modifiers (objc:coerce-to-selector selector))))
    ;; Without this a text view keeps Tab and the arrows for itself, and the
    ;; command is never sent.
    (objc:invoke command "setWantsPriorityOverSystemBehavior:" t)
    command))

(defun key-commands ()
  (or *key-commands*
      (setf *key-commands*
            (let ((array (objc:invoke "NSMutableArray" "array")))
              (dolist (command
                       (list (key-command (string #\Tab) "listenerTab:")
                             (key-command (%ns-string-constant "UIKeyInputUpArrow")
                                          "listenerUp:")
                             (key-command (%ns-string-constant "UIKeyInputDownArrow")
                                          "listenerDown:")
                             (key-command (%ns-string-constant "UIKeyInputEscape")
                                          "listenerEscape:")
                             (key-command "." "listenerInterrupt:"
                                          +ui-key-modifier-command+)
                             (key-command "k" "listenerClear:"
                                          +ui-key-modifier-command+)))
                (objc:invoke array "addObject:" command))
              ;; And one per CHORD in *PAREDIT-KEYS*.  The bare characters are
              ;; not here: they arrive as text, through the delegate below.
              (dolist (spec (mapcar #'car *paredit-keys*))
                (unless (paredit-self-insert-key-p spec)
                  (multiple-value-bind (modifiers character) (parse-key-spec spec)
                    (when character
                      (objc:invoke array "addObject:"
                                   (key-command (string character) "listenerParedit:"
                                                (ui-modifier-flags modifiers)))))))
              (objc:retain array)))))

(defun invalidate-key-commands ()
  "Forget the built array, so a rebinding is picked up.

UIKit asks -keyCommands again as it rebuilds the responder chain's command
list, which happens whenever the first responder changes -- so this is enough,
without telling UIKit anything."
  (let ((array *key-commands*))
    (setf *key-commands* nil)
    (when (and array (cffi:pointerp array)) (objc:release array)))
  nil)

(defun ui-modifier-flags (modifiers)
  "Our :CONTROL and :META as UIKeyModifierFlags.  Meta is Alternate: it is the
key in that position on a keyboard attached to an iPad."
  (let ((flags 0))
    (when (member :control modifiers) (setf flags (logior flags +ui-key-modifier-control+)))
    (when (member :meta modifiers) (setf flags (logior flags +ui-key-modifier-alternate+)))
    flags))

(defun key-command-modifiers (command)
  "A UIKeyCommand's modifier flags, back as our own list."
  (let ((flags (objc:invoke command "modifierFlags"))
        (modifiers '()))
    (when (plusp (logand flags +ui-key-modifier-control+)) (push :control modifiers))
    (when (plusp (logand flags +ui-key-modifier-alternate+)) (push :meta modifiers))
    modifiers))

;;; The Objective-C methods ---------------------------------------------------

(define-listener-method ("listenerDrainQueue" :void) ()
  (drain-main-thread-queue))

(define-listener-method ("keyCommands" objc:objc-object-pointer) ()
  (key-commands))

(define-listener-method ("listenerTab:" :void) ((sender objc:objc-object-pointer))
  (key-tab self))

(define-listener-method ("listenerUp:" :void) ((sender objc:objc-object-pointer))
  (key-arrow self -1))

(define-listener-method ("listenerDown:" :void) ((sender objc:objc-object-pointer))
  (key-arrow self 1))

(define-listener-method ("listenerEscape:" :void) ((sender objc:objc-object-pointer))
  (key-escape))

(define-listener-method ("listenerInterrupt:" :void) ((sender objc:objc-object-pointer))
  (abort-evaluation *listener*))

(define-listener-method ("listenerClear:" :void) ((sender objc:objc-object-pointer))
  (clear-transcript *listener*))

;;; One IMP for every paredit chord: the UIKeyCommand says which key it was, so
;;; the keymap can be consulted exactly as the Mac's -keyDown: does.
(define-listener-method ("listenerParedit:" :void) ((command objc:objc-object-pointer))
  (let ((input (ignore-errors (objc:ns-string-to-string
                               (objc:invoke command "input")))))
    (when (and input (= 1 (length input)))
      (paredit-handles-character-p self pointer (char input 0)
                                   (key-command-modifiers command)))))

;;; The delegate.  Return is a "\n" replacing the selection: submit instead,
;;; and refuse the edit, since SUBMIT-INPUT appends the newline itself.  Any
;;; other edit is allowed only in the input region.  On an error the edit is
;;; ALLOWED, as on the Mac, so a bug here does not look like a dead keyboard.

(define-listener-method ("textView:shouldChangeTextInRange:replacementText:"
                         objc:objc-bool :on-error t)
    ((text-view objc:objc-object-pointer)
     (affected cocoa:ns-range)
     (replacement objc:objc-object-pointer))
  (let ((string (objc:ns-string-to-string replacement)))
    (cond
      ;; Return submits; SUBMIT-INPUT appends the newline itself.
      ((string= string (string #\Newline))
       (submit-input self pointer)
       nil)
      ;; A self-inserting paredit key -- ( ) " -- or a deletion, which arrives
      ;; here as an empty replacement over the character being removed.  Which
      ;; character that is says whether it was Backspace or forward Delete:
      ;; UIKit gives no other clue, and the two must not be confused, since one
      ;; takes the character behind the caret and the other the one in front.
      ((and (input-edit-allowed-p self affected)
            (if (zerop (length string))
                (let ((caret (caret-index pointer)))
                  (cond ((null caret) nil)
                        ((= (car affected) (1- caret))
                         (paredit-handles-character-p self pointer #\Backspace))
                        ((= (car affected) caret)
                         (paredit-handles-character-p self pointer #\Rubout))
                        (t nil)))
                (and (= 1 (length string))
                     (paredit-handles-character-p self pointer (char string 0)))))
       nil)
      (t (input-edit-allowed-p self affected)))))

(define-listener-method ("textViewDidChangeSelection:" :void)
    ((text-view objc:objc-object-pointer))
  (apply-typing-attributes pointer)
  (refresh-paren-highlight self pointer))
