;;;; src/macos/window.lisp -- the window, the scroll view, and the menu.
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
    (initialize-view-history object)
    ;; Its own delegate.  AppKit finds a delegate method through
    ;; -respondsToSelector:, which a real class_addMethod'd IMP satisfies, so
    ;; this needs no protocol declaration and no second object to keep alive.
    (objc:invoke view "setDelegate:" view)
    (values view object)))

;;; The window ----------------------------------------------------------------

(objc:define-objc-class listener-window-delegate ()
  ()
  (:objc-class-name "LispListenerWindowDelegate"))

(defvar *stop-run-loop-on-last-close* nil
  "Whether closing the last listener window should end the event loop.

Bound by RUN-LISTENER, which borrowed thread 1 from a REPL and has to give it
back.  Left NIL by MAIN, where the whole process is the application and the
delegate's -applicationShouldTerminateAfterLastWindowClosed: quits instead.

A dynamic binding and not an assignment, and that is sound rather than lucky:
-windowWillClose: is an IMP that AppKit calls on thread 1, from inside the
-[NSApplication run] that RUN-LISTENER is blocked in, so it runs within the
binding's extent.")

(defun current-listener ()
  "The listener a menu command means: the one whose window is key.

A menu item's action arrives saying nothing about which window it was meant
for, so the application has to be asked.  Falling back to *LISTENER* keeps the
answer sensible while no window is key -- during startup, or when another
application is in front."
  (or (let ((key (ignore-errors
                  (objc:invoke (objc.runloop:shared-application) "keyWindow"))))
        (listener-for-window key))
      *listener*
      (first *listeners*)))

(defparameter +ns-event-type-application-defined+ 15
  "NSEventTypeApplicationDefined.  An event AppKit has no meaning for, which
is what makes it safe to post purely to wake the loop up.")

(defun post-wakeup-event (application)
  "Queue a do-nothing event, so the next -nextEventMatchingMask: returns.

-[NSApplication stop:] only raises a flag, which -run tests after it finishes
the event in hand and then asks for the NEXT one.  Closing the last window is
very often the last event there is, so without something in the queue -run
sits blocked with the flag set, the window gone and the REPL never coming
back.  This is that something."
  (let ((event (ignore-errors
                (objc:invoke "NSEvent"
                             "otherEventWithType:location:modifierFlags:timestamp:windowNumber:context:subtype:data1:data2:"
                             +ns-event-type-application-defined+
                             #(0d0 0d0) 0 0d0 0 nil 0 0 0))))
    (when (and event (cffi:pointerp event) (not (cffi:null-pointer-p event)))
      (objc:invoke application "postEvent:atStart:" event t)
      t)))

(defun stop-run-loop-soon ()
  "Ask -[NSApplication run] to return, on the next pass of the run loop.

NEVER send -stop: synchronously from inside an AppKit callback.  A real click
on the close widget runs inside that button's mouse-tracking loop, and
-windowWillClose: fires down there; ending the loop on the spot hands control
back while AppKit is still unwinding, and leaves the application drawn but
dead with nothing pumping events.  Deferring lets the current event, and
everything nested in it, finish first.

The wake-up is queued first on purpose.  It simply waits until -run asks for
an event, which is the moment that must not block; whether it was posted
before or after the flag was raised makes no difference to AppKit."
  (when *stop-run-loop-on-last-close*
    (let ((application (objc.runloop:shared-application)))
      (post-wakeup-event application)
      (objc:invoke application
                   "performSelector:withObject:afterDelay:inModes:"
                   (objc:coerce-to-selector "stop:")
                   nil 0d0 +common-run-loop-modes+))))

(defun retarget-main-thread ()
  "Point the drain hop at a view that still exists.

The hop is -performSelectorOnMainThread: to one particular object and the
queue behind it is shared, so any live listener's view will carry it -- but
the one that was chosen may be the one whose window has just closed, and
messaging a view whose last reference went with it corrupts rather than
errors."
  (unless (and *main-thread-target*
               (find *main-thread-target* *listeners*
                     :test #'same-objc-object-p :key #'listener-view))
    ;; Only ever REPOINTED, never cleared.  The closing listener's thread is
    ;; still unwinding and still writing -- the prompt it is about to print,
    ;; the note that it aborted -- and a NIL target makes each of those writes
    ;; signal, inside the debugger hook, which aborts, which loops.  That is
    ;; not hypothetical: it span 204 times in the space of one close.  The old
    ;; view stays a valid receiver either way, because -releasedWhenClosed is
    ;; off and closing a window therefore deallocates nothing.
    (let ((listener (first *listeners*)))
      (when listener
        (setf *main-thread-target* (listener-view listener))))))

(objc:define-objc-method ("windowWillClose:" :void)
    ((self listener-window-delegate) (notification objc:objc-object-pointer))
  (declare (ignorable notification))
  ;; Nothing may unwind into AppKit.
  (handler-case
      ;; THIS window's listener, asked of the notification, never *LISTENER*.
      ;; With two open, closing the background one would otherwise send the
      ;; front one's reader an end of file and abort what it was evaluating.
      (let* ((window (ignore-errors (objc:invoke notification "object")))
             (listener (or (listener-for-window window) *listener*)))
        (when listener
          ;; End of input, so a listener parked in READ stops rather than
          ;; waiting on a window that has gone.
          (queue-set-eof (listener-input listener))
          (abort-evaluation listener)
          (unregister-listener listener)
          (retarget-main-thread))
        ;; The session ends with the LAST window, not with any window: one of
        ;; three closing leaves two that still need an event loop under them.
        (unless *listeners*
          (stop-run-loop-soon)))
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
            (listener-window listener) window)
      ;; The drain target is whichever view is handy -- one queue, and any live
      ;; view can carry the hop -- so a second listener does not take it over.
      ;; Claiming it every time would leave it on the newest window, and the
      ;; newest is as likely as any other to be the first one closed.
      ;; ... and the first of a fresh session claims it outright, so a second
      ;; RUN-LISTENER in one REPL does not keep hopping through the window of
      ;; the session before it.
      (when (or (null *listeners*) (null *main-thread-target*))
        (setf *main-thread-target* view))
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

(defun menu-item-present-p (menu-title item-title)
  "Whether the menu bar really carries ITEM-TITLE under MENU-TITLE.

Asked of AppKit rather than of the list INSTALL-MENU was written from, so that
it answers for the menu that exists.  In the bundle that is the question: the
image is a dumped core, every Objective-C class in it is rebuilt on the way up,
and a menu item whose action no longer resolves is one nothing else notices."
  (let* ((main (objc:invoke (objc.runloop:shared-application) "mainMenu"))
         (holder (and main (cffi:pointerp main) (not (cffi:null-pointer-p main))
                      (objc:invoke main "itemWithTitle:" menu-title)))
         (submenu (and holder (cffi:pointerp holder) (not (cffi:null-pointer-p holder))
                       (objc:invoke holder "submenu")))
         (item (and submenu (cffi:pointerp submenu) (not (cffi:null-pointer-p submenu))
                    (objc:invoke submenu "itemWithTitle:" item-title))))
    (and item (cffi:pointerp item) (not (cffi:null-pointer-p item))
         ;; A target too.  An item with none is an item the responder chain
         ;; will quietly decline, which looks exactly like a working menu.
         (let ((target (objc:invoke item "target")))
           (and target (cffi:pointerp target) (not (cffi:null-pointer-p target))))
         t)))

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
    ;; These three are the listener's own, so they name the controller rather
    ;; than trusting the responder chain to find something that answers.  The
    ;; controller is the application's, not any window's: a menu item's target
    ;; is not retained, and one hung off the first listener would be pointing
    ;; at freed memory the moment that window closed.
    (add-submenu main "Listener"
                 '(("New Listener" "listenerNewListener:" "n")
                   :separator
                   ("Interrupt" "listenerInterrupt:" ".")
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
