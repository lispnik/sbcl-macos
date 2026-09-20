;;;; src/listener.lisp -- what a listener is.
;;;;
;;;; One structure holding the two halves and the two queues between them.
;;;; Every foreign pointer in it is made at run time, in MAIN: nothing here
;;;; may be filled in at load time, because a pointer does not survive
;;;; SAVE-LISP-AND-DIE and the bundle's image is a dumped core.

(in-package #:lisp-listener)

(defvar *listener* nil
  "The listener this image is running, or NIL.

A special rather than an argument because the Objective-C callbacks reach it:
an IMP is handed self and its arguments and nothing else.")

(defstruct (listener (:constructor %make-listener))
  ;; The Cocoa side.  Main thread only.
  view                                  ; the LispListenerView pointer
  view-object                           ; its Lisp object, held so it stays
  window
  ;; The two queues.
  (input (make-character-queue))
  output                                ; a LISTENER-OUTPUT-STREAM
  ;; The Lisp side.
  thread
  (debug-level 0)
  ;; Lisp objects for Objective-C classes that something unretained points at:
  ;; a window's delegate and the application's are both weak references on the
  ;; Cocoa side, so if nothing here held them they would be collected while
  ;; still installed.  A plist, because there are three of them.
  (retained '())
  ;; The restarts panel and the two controls inside it that anything outside
  ;; needs to reach, while a debugger level has one up.  Thread 1 only.
  restarts-panel
  restarts-table
  restarts-invoke
  ;; Set from MAIN when the application is a bundle, so that quitting can go
  ;; through -[NSApplication terminate:] rather than SB-EXT:EXIT.
  (bundled nil))

(defun safepoint-build-p ()
  "True on an SBCL built --with-sb-safepoint.

Such a build stops the world by polling rather than by signalling, which is
what makes it safe for Lisp to run on a thread Darwin will not let anyone
signal -- a libdispatch worker.  AppKit reaches libdispatch on its own, so a
Cocoa application wants one whether or not it uses GCD itself.  See
lispnik/objc's doc/sbcl-libdispatch-safepoint.md."
  (and (member :sb-safepoint *features*) t))

(defun report-condition (condition)
  "CONDITION's report as a string, even when the report itself signals.

Here rather than beside the debugger because both the transcript and the
restarts panel need it, and they load in that order."
  (handler-case (princ-to-string condition)
    (error (inner)
      (format nil "A condition of type ~a whose own report signalled: ~a"
              (type-of condition) inner))))
