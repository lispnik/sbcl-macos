;;;; src/macos/view.lisp -- LispListenerView, over NSTextView.
;;;;
;;;; The transcript itself is src/transcript.lisp and is shared with iOS.  This
;;;; is what AppKit needs besides: the class, the colours and font, the run loop
;;;; modes a hop to thread 1 is queued in, and the methods NSTextView sends --
;;;; Return, the arrow keys, Tab, and the delegate's edit check.

(in-package #:lisp-listener)

;;; AppKit constants ----------------------------------------------------------

(defconstant +ns-view-width-and-height-sizable+ 18)
(defconstant +ns-window-style-titled+ 1)
(defconstant +ns-window-style-closable+ 2)
(defconstant +ns-window-style-mask+ 15
  "Titled, closable, miniaturizable, resizable.")
(defconstant +ns-backing-store-buffered+ 2)
(defconstant +png-file-type+ 4
  "NSBitmapImageFileTypePNG, for -representationUsingType:properties:.")

(defparameter +common-run-loop-modes+
  #("NSDefaultRunLoopMode" "NSEventTrackingRunLoopMode" "NSModalPanelRunLoopMode")
  "The run loop modes a queued selector is delivered in: the default one, and
the ones the main thread runs while a window is being resized or a panel is up,
so a request made then still arrives.

Named one by one rather than as the common-modes pseudo mode.  Measured in
lem-cocoa: a perform queued in the mode named kCFRunLoopCommonModes from a Lisp
thread never ran, and every perform queued after it stayed behind it.

A Lisp vector; INVOKE converts it to an NSArray of NSStrings on the way in.")

(defun main-thread-run-loop-modes () +common-run-loop-modes+)

(defun history-directory ()
  "~/Library/Application Support/Lisp Listener/, which is where a Mac
application keeps something it wrote for itself."
  (merge-pathnames "Library/Application Support/Lisp Listener/"
                   (user-homedir-pathname)))

(defun transcript-color (kind)
  (ecase kind
    ((:output :input) (objc:invoke "NSColor" "textColor"))
    (:prompt (objc:invoke "NSColor" "systemBlueColor"))
    (:value (objc:invoke "NSColor" "systemGreenColor"))
    (:error (objc:invoke "NSColor" "systemRedColor"))
    (:note (objc:invoke "NSColor" "secondaryLabelColor"))))


(defun transcript-font (size)
  (objc:invoke "NSFont" "monospacedSystemFontOfSize:weight:" size 0d0))

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



;;; The Objective-C methods ---------------------------------------------------

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
;;; On an error the edit is ALLOWED.  Refusing by default would make a bug in
;;; here look like a text view that has stopped accepting typing.

(define-listener-method ("textView:shouldChangeTextInRange:replacementString:"
                         objc:objc-bool :on-error t)
    ((text-view objc:objc-object-pointer)
     (affected cocoa:ns-range)
     (replacement objc:objc-object-pointer))
  (input-edit-allowed-p self affected))

(define-listener-method ("acceptsFirstResponder" objc:objc-bool :on-error t) ()
  t)

;;; Completion ------------------------------------------------------------------
;;;
;;; NSTextView already has completion: -complete: asks the view for the range
;;; being completed (-rangeForUserCompletion) and for the candidates
;;; (-completionsForPartialWordRange:indexOfSelectedItem:), then shows its own
;;; popup.  These answer with src/completion.lisp's symbol tokens and symbols.
;;;
;;; Tab goes to SUPER's -complete:, never to the view's own.  That override,
;;; in restarts-panel.lisp, is Escape's, and it first cancels any debugger level --
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
