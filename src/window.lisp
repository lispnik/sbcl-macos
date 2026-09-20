;;;; src/window.lisp -- the window, the scroll view, and the menu.
;;;;
;;;; Thread 1 only, all of it.

(in-package #:lisp-listener)

(defparameter +default-frame+ #(0d0 0d0 760d0 520d0))

;;; The view ------------------------------------------------------------------

(defun make-listener-view (&optional (frame +default-frame+))
  "Allocate a LispListenerView and configure it for Lisp source.
Returns (VALUES POINTER OBJECT)."
  (let* ((object (make-instance 'listener-text-view
                                :init-function
                                (lambda (pointer &rest initargs)
                                  (declare (ignore initargs))
                                  (objc:invoke pointer "initWithFrame:" frame))
                                :allow-other-keys t))
         (view (objc:objc-object-pointer object)))
    ;; Rich text stays ON: the transcript is coloured per kind, which is the
    ;; whole reason to use an attributed string.  What has to go is every
    ;; substitution AppKit would otherwise perform, each of which silently
    ;; corrupts Lisp source -- a "smart" quote is not a STRING delimiter and a
    ;; smart dash is not a minus sign.
    (objc:invoke view "setAutomaticQuoteSubstitutionEnabled:" nil)
    (objc:invoke view "setAutomaticDashSubstitutionEnabled:" nil)
    (objc:invoke view "setAutomaticTextReplacementEnabled:" nil)
    (objc:invoke view "setAutomaticSpellingCorrectionEnabled:" nil)
    (objc:invoke view "setContinuousSpellCheckingEnabled:" nil)
    (objc:invoke view "setGrammarCheckingEnabled:" nil)
    (objc:invoke view "setImportsGraphics:" nil)
    ;; Undo across a transcript that is mostly read-only is a trap: the undo
    ;; manager would happily try to put back text the delegate now refuses.
    (objc:invoke view "setAllowsUndo:" nil)
    (objc:invoke view "setEditable:" t)
    (objc:invoke view "setSelectable:" t)
    (objc:invoke view "setAutoresizingMask:" +ns-view-width-and-height-sizable+)
    (objc:invoke view "setTypingAttributes:" (transcript-attributes :input))
    ;; Its own delegate.  AppKit finds a delegate method through
    ;; -respondsToSelector:, which a real class_addMethod'd IMP satisfies, so
    ;; this needs no protocol declaration and no second object to keep alive.
    (objc:invoke view "setDelegate:" view)
    (values view object)))

;;; The window ----------------------------------------------------------------

(objc:define-objc-class listener-window-delegate ()
  ()
  (:objc-class-name "LispListenerWindowDelegate"))

(defparameter +modal-run-loop-modes+
  #("NSModalPanelRunLoopMode" "kCFRunLoopDefaultMode")
  "The modes a modal session may be running in.  Anything scheduled for
delivery during -runModalForWindow: has to name NSModalPanelRunLoopMode: the
session runs in that mode, so work queued for the default mode alone is queued
for a mode that is not running and never arrives.")

(defun stop-modal-soon ()
  "Ask for -stopModal on the next pass of the run loop.

NEVER send -stopModal synchronously from inside an AppKit callback.  A real
click on the close widget runs inside that button's mouse-tracking loop, itself
nested inside the modal loop, and -windowWillClose: fires down there; ending
the session on the spot hands control back while AppKit is still unwinding, and
leaves the window drawn but dead with nothing pumping events.  Deferring lets
the current event, and everything nested in it, finish first."
  (objc:invoke (objc.runloop:shared-application)
               "performSelector:withObject:afterDelay:inModes:"
               (objc:coerce-to-selector "stopModal")
               nil 0d0 +modal-run-loop-modes+))

(objc:define-objc-method ("windowWillClose:" :void)
    ((self listener-window-delegate) (notification objc:objc-object-pointer))
  (declare (ignorable notification))
  ;; Nothing may unwind into AppKit.
  (handler-case
      (let ((listener *listener*))
        (when listener
          ;; End of input, so a listener parked in READ stops rather than
          ;; waiting on a window that has gone.
          (queue-set-eof (listener-input listener))
          (abort-evaluation listener))
        ;; AppKit does not end a modal session just because the window closed,
        ;; so RUN-LISTENER would never return without this.  Harmless when
        ;; there is no session -- MAIN is in -run, not in a modal loop.
        (stop-modal-soon))
    (error (condition) (note "windowWillClose: ~a" condition))))

(defun make-listener-window (listener &key (title "Lisp Listener")
                                           (frame +default-frame+))
  "Build the window around a fresh listener view and fill in LISTENER.
Main thread only.  Returns LISTENER."
  (objc.runloop:check-main-thread "Making the listener window")
  (multiple-value-bind (view object) (make-listener-view frame)
    (let ((window (objc:invoke (objc:invoke "NSWindow" "alloc")
                               "initWithContentRect:styleMask:backing:defer:"
                               frame +ns-window-style-mask+
                               +ns-backing-store-buffered+ nil))
          (scroll (objc:invoke (objc:invoke "NSScrollView" "alloc")
                               "initWithFrame:" frame))
          (delegate (make-instance 'listener-window-delegate)))
      ;; -releasedWhenClosed defaults to YES for a window made this way, so
      ;; clicking the close button would DEALLOCATE it -- and everything still
      ;; holding the pointer, this structure included, would be messaging freed
      ;; memory, which corrupts rather than errors.  Lisp owns this window.
      (objc:invoke window "setReleasedWhenClosed:" nil)
      (objc:invoke window "setTitle:" title)
      (objc:invoke scroll "setHasVerticalScroller:" t)
      (objc:invoke scroll "setAutoresizingMask:" +ns-view-width-and-height-sizable+)
      (objc:invoke scroll "setDocumentView:" view)
      (objc:invoke window "setContentView:" scroll)
      (objc:invoke window "setInitialFirstResponder:" view)
      (objc:invoke window "setDelegate:" (objc:objc-object-pointer delegate))
      (objc:invoke window "center")
      (setf (listener-view listener) view
            (listener-view-object listener) object
            (listener-window listener) window
            ;; The delegate is unretained by the window, so the structure holds
            ;; it.  (The view object is held by the bridge's identity map until
            ;; -dealloc, but the delegate has nowhere else to live.)
            *main-thread-target* view)
      (setf (getf (listener-retained listener) :window-delegate) delegate)
      listener)))

(defun show-listener-window (listener)
  (let ((window (listener-window listener)))
    (objc:invoke window "makeKeyAndOrderFront:" nil)
    (objc:invoke window "makeFirstResponder:" (listener-view listener))
    (objc:invoke (objc.runloop:shared-application) "activateIgnoringOtherApps:" t))
  listener)

;;; The menu ------------------------------------------------------------------
;;;
;;; A program with no nib has no menu bar until it makes one.  Every item's
;;; target is nil so that the responder chain finds whoever answers -- the text
;;; view for Cut and Paste, the application for Hide and Quit -- except the two
;;; that are ours, which name the controller.

(defun add-menu-item (menu title action &optional (key "") target)
  (let ((item (objc:invoke (objc:invoke "NSMenuItem" "alloc")
                           "initWithTitle:action:keyEquivalent:"
                           title (objc:coerce-to-selector action) key)))
    (when target
      (objc:invoke item "setTarget:" target))
    (objc:invoke menu "addItem:" item)
    (objc:invoke item "release")
    item))

(defun add-submenu (main-menu title items &optional target)
  "A top-level menu of ITEMS, each (TITLE ACTION [KEY]) or :SEPARATOR."
  (let ((menu (objc:invoke (objc:invoke "NSMenu" "alloc") "initWithTitle:" title))
        (holder (objc:invoke (objc:invoke "NSMenuItem" "alloc")
                             "initWithTitle:action:keyEquivalent:" title nil "")))
    (dolist (item items)
      (if (eq item :separator)
          (objc:invoke menu "addItem:" (objc:invoke "NSMenuItem" "separatorItem"))
          (destructuring-bind (item-title action &optional (key "")) item
            (add-menu-item menu item-title action key target))))
    (objc:invoke holder "setSubmenu:" menu)
    (objc:invoke main-menu "addItem:" holder)
    (objc:invoke holder "release")
    menu))

(defun install-menu (controller)
  (let ((main (objc:invoke (objc:invoke "NSMenu" "alloc") "initWithTitle:" "Main"))
        (application (objc.runloop:shared-application)))
    (add-submenu main "Lisp Listener"
                 '(("About Lisp Listener" "orderFrontStandardAboutPanel:")
                   :separator
                   ("Hide Lisp Listener" "hide:" "h")
                   ("Hide Others" "hideOtherApplications:" "H")
                   :separator
                   ("Quit Lisp Listener" "terminate:" "q")))
    (add-submenu main "Edit"
                 '(("Cut" "cut:" "x")
                   ("Copy" "copy:" "c")
                   ("Paste" "paste:" "v")
                   :separator
                   ("Select All" "selectAll:" "a")))
    ;; These two are the listener's own, so they name the controller rather
    ;; than trusting the responder chain to find something that answers.
    (add-submenu main "Listener"
                 '(("Interrupt" "listenerInterrupt:" ".")
                   ("Clear Transcript" "listenerClearTranscript:" "k"))
                 controller)
    (let ((windows (add-submenu main "Window"
                                '(("Minimize" "performMiniaturize:" "m")
                                  ("Zoom" "performZoom:")
                                  :separator
                                  ("Bring All to Front" "arrangeInFront:")))))
      (objc:invoke application "setWindowsMenu:" windows))
    (objc:invoke application "setMainMenu:" main)
    main))
