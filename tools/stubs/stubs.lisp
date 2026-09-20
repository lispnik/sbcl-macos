;;;; tools/stubs/stubs.lisp -- just enough of the world to COMPILE the listener
;;;; on a machine that has no Objective-C runtime.
;;;;
;;;; Nothing here is a mock and, with one exception, nothing here runs.  It
;;;; exists so that SBCL's file compiler can be pointed at src/*.lisp off macOS
;;;; and report what it always reports: undefined functions and variables, wrong
;;;; argument counts, malformed lambda lists, macros that will not expand, a
;;;; DEFCLASS with a bad slot.  tools/syntax-check.lisp answers "does it parse";
;;;; this answers "does it compile", which is a great deal more.
;;;;
;;;; The exception is BORDEAUX-THREADS, which delegates to SB-THREAD and really
;;;; works.  That is what lets tools/headless-test.lisp run an actual listener
;;;; on top of this file -- real threads, streams, reader, evaluator and
;;;; debugger, with only Cocoa hollowed out -- and so answer a third question,
;;;; "does the listener work", for everything that is not a window.  What it
;;;; still cannot answer is whether the window works.  Only a Mac does that.
;;;;
;;;; The two defining macros mirror the real expanders' BINDING STRUCTURE, which
;;;; is the part the body depends on: SELF, the optional pointer variable, one
;;;; variable per argument, declarations landing inside, and CURRENT-SUPER
;;;; shadowed by a local macro.  Their foreign half is deliberately absent.

(in-package #:cl-user)

(defpackage #:cffi
  (:use #:cl)
  (:export #:foreign-symbol-pointer #:mem-ref #:mem-aref #:pointerp
           #:null-pointer #:null-pointer-p #:pointer-eq #:pointer-address))

(defpackage #:bordeaux-threads
  (:nicknames #:bt)
  (:use #:cl)
  (:export #:make-lock #:with-lock-held #:make-condition-variable
           #:condition-wait #:condition-notify #:make-thread #:interrupt-thread
           #:thread-alive-p #:current-thread #:thread-name))

(defpackage #:objc
  (:use #:cl)
  (:export #:ensure-objc-initialized
           #:invoke #:invoke-bool #:invoke-into #:invoke* #:current-super
           #:alloc-init-object #:description
           #:coerce-to-objc-class #:objc-class-name #:coerce-to-selector
           #:objc-object-pointer #:objc-class #:sel #:objc-c-string #:objc-bool
           #:retain #:release #:autorelease #:retain-count #:with-autorelease-pool
           #:ns-string-to-string #:string-to-ns-string
           #:standard-objc-object #:define-objc-class #:define-objc-method
           #:define-objc-class-method #:objc-object-from-pointer))

(defpackage #:cocoa
  (:use #:cl)
  (:export #:ns-point #:ns-size #:ns-rect #:ns-range #:ns-not-found))

(defpackage #:objc.runloop
  (:use #:cl)
  (:export #:main-thread-p #:check-main-thread #:shared-application
           #:set-activation-policy #:pump-events #:pump-run-loop
           #:run-cocoa-application #:window-server-p
           #:remember-frontmost #:restore-frontmost))

;;; ---------------------------------------------------------------------------

(in-package #:cffi)

(defun foreign-symbol-pointer (name &key library) (declare (ignore name library)) nil)
(defun mem-ref (pointer type &optional (offset 0)) (declare (ignore pointer type offset)) nil)
(defun mem-aref (pointer type &optional (index 0)) (declare (ignore pointer type index)) nil)
(defun pointerp (object) (declare (ignore object)) nil)
(defun null-pointer () nil)
(defun null-pointer-p (pointer) (declare (ignore pointer)) t)
(defun pointer-eq (a b) (declare (ignore a b)) nil)
(defun pointer-address (pointer) (declare (ignore pointer)) 0)

;;; ---------------------------------------------------------------------------

(in-package #:bordeaux-threads)

;;; These are the one part of this file that is NOT a stub.  Everything else
;;; here is a name with no behaviour, because the compiler only needs the name;
;;; threads are different, because tools/headless-test.lisp drives a real
;;; listener on this file and a listener whose thread never starts does
;;; nothing.  SB-THREAD is portable to every platform the check runs on, and
;;; the listener uses only this much of bordeaux-threads, so delegating costs
;;; nothing and buys a second use for the file.
(defun make-lock (&optional name)
  (sb-thread:make-mutex :name (or name "lisp-listener lock")))
(defmacro with-lock-held ((place) &body body)
  `(sb-thread:with-mutex (,place) ,@body))
(defun make-condition-variable (&key name)
  (sb-thread:make-waitqueue :name (or name "lisp-listener condition")))
(defun condition-wait (condition lock &key timeout)
  (sb-thread:condition-wait condition lock :timeout timeout))
(defun condition-notify (condition)
  (sb-thread:condition-notify condition))
(defun make-thread (function &key name)
  (sb-thread:make-thread function :name (or name "lisp-listener thread")))
(defun interrupt-thread (thread function)
  (sb-thread:interrupt-thread thread function))
(defun thread-alive-p (thread) (sb-thread:thread-alive-p thread))
(defun current-thread () sb-thread:*current-thread*)
(defun thread-name (thread) (sb-thread:thread-name thread))

;;; ---------------------------------------------------------------------------

(in-package #:objc)

(defclass standard-objc-object () ())

(defun ensure-objc-initialized (&key modules) (declare (ignore modules)) t)
(defun invoke (receiver method &rest arguments)
  (declare (ignore receiver method arguments)) nil)
(defun invoke-bool (receiver method &rest arguments)
  (declare (ignore receiver method arguments)) nil)
(defun invoke-into (result receiver method &rest arguments)
  (declare (ignore result receiver method arguments)) nil)
(defun alloc-init-object (class) (declare (ignore class)) nil)
(defun description (pointer) (declare (ignore pointer)) "")
(defun coerce-to-objc-class (class) (declare (ignore class)) nil)
(defun objc-class-name (class) (declare (ignore class)) "")
(defun coerce-to-selector (method) (declare (ignore method)) nil)
(defgeneric objc-object-pointer (object))
(defmethod objc-object-pointer ((object t)) object)
(defun retain (pointer) pointer)
(defun release (pointer) (declare (ignore pointer)) (values))
(defun autorelease (pointer) pointer)
(defun retain-count (pointer) (declare (ignore pointer)) 1)
(defun ns-string-to-string (ns-string &optional preserve)
  (declare (ignore ns-string preserve)) "")
(defun string-to-ns-string (string &optional autoreleasep)
  (declare (ignore string autoreleasep)) nil)
(defun objc-object-from-pointer (pointer) (declare (ignore pointer)) nil)
(defmacro with-autorelease-pool ((&rest options) &body body)
  (declare (ignore options)) `(progn ,@body))
(defmacro current-super ()
  (error "CURRENT-SUPER is only meaningful inside a method body."))

(defun %parse-body (body)
  "Split BODY into (VALUES FORMS DECLARATIONS), as the real expander does."
  (let ((declarations '()))
    (loop while (and body (consp (first body)) (eq (car (first body)) 'declare))
          do (push (pop body) declarations))
    (values body (nreverse declarations))))

(defmacro define-objc-class (name superclasses slots &rest options)
  (let ((defclass-options
          (remove-if (lambda (option)
                       (member (first option)
                               '(:objc-class-name :objc-superclass-name
                                 :objc-instance-vars :objc-protocols)))
                     options)))
    `(progn
       (defclass ,name ,(or superclasses '(standard-objc-object))
         ,slots
         ,@defclass-options)
       ',name)))

(defun %expand-method (selector result-type object-argspec argspecs body)
  (destructuring-bind (object-var class-name &optional pointer-var) object-argspec
    (multiple-value-bind (forms declarations) (%parse-body body)
      (let ((variables (append (list object-var)
                               (when pointer-var (list pointer-var))
                               (mapcar #'first argspecs))))
        `(defun ,(intern (format nil "STUB-~a-~a" class-name
                                 (string-upcase (substitute #\- #\: selector))))
             ,variables
           (declare (ignorable ,@variables))
           ,@declarations
           (macrolet ((current-super () ''stub-super))
             ,@forms
             ,@(when (eq result-type :void) '(nil))))))))

(defmacro define-objc-method ((selector result-type &optional result-style)
                              (object-argspec &rest argspecs) &body body)
  (declare (ignore result-style))
  (%expand-method selector result-type object-argspec argspecs body))

(defmacro define-objc-class-method ((selector result-type &optional result-style)
                                    (object-argspec &rest argspecs) &body body)
  (declare (ignore result-style))
  (%expand-method selector result-type object-argspec argspecs body))

;;; The eight type descriptor symbols are names the method macros read, never
;;; values, so declaiming them as types is all that is needed for the bodies
;;; above to compile.
(deftype objc-class () t)
(deftype sel () t)
(deftype objc-c-string () t)
(deftype objc-bool () t)

;;; ---------------------------------------------------------------------------

(in-package #:cocoa)

(deftype ns-point () t)
(deftype ns-size () t)
(deftype ns-rect () t)
(deftype ns-range () t)
(defconstant ns-not-found (1- (expt 2 63)))

;;; ---------------------------------------------------------------------------

(in-package #:objc.runloop)

(defun main-thread-p () t)
(defun check-main-thread (&optional operation) (declare (ignore operation)) t)
(defun shared-application (&key activation-policy) (declare (ignore activation-policy)) nil)
(defun set-activation-policy (policy) (declare (ignore policy)) nil)
(defun pump-events (&key seconds until max-seconds)
  (declare (ignore seconds until max-seconds)) 0)
(defun pump-run-loop (&key seconds) (declare (ignore seconds)) 0)
(defun run-cocoa-application (&key activation-policy)
  (declare (ignore activation-policy)) nil)
(defun window-server-p () nil)
(defun remember-frontmost () nil)
(defun restore-frontmost () t)
