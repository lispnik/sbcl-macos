;;;; src/ios/restarts-sheet.lisp -- the restarts as an action sheet.
;;;;
;;;; iOS's half of src/restarts.lisp, which explains the design.  Where the Mac
;;;; puts up a floating panel with a table, a phone has one idiom for "choose
;;;; one of these": a UIAlertController in the action-sheet style.  One action
;;;; per restart, with the same titles the transcript numbers, and Cancel,
;;;; which -- as on the Mac -- takes the restart back to the top level rather
;;;; than merely closing the sheet.
;;;;
;;;; A tap does what a button on the Mac does: it TYPES the restart's number,
;;;; through CHOOSE-RESTART, because the restart belongs to the listener thread.
;;;; The sheet has no room for the backtrace; the transcript has it.
;;;;
;;;; The handlers are blocks made from Lisp closures.  UIKit copies them, and
;;;; the copy keeps the closure alive, so WITH-OBJC-BLOCK is enough -- see
;;;; lispnik/objc's README on block lifetime.

(in-package #:lisp-listener)

(defconstant +alert-style-action-sheet+ 0)
(defconstant +alert-action-default+ 0)
(defconstant +alert-action-cancel+ 1)

(defun add-alert-action (alert title style function)
  (objc:with-objc-block (handler '(:void (objc:objc-object-pointer))
                                 (lambda (action)
                                   (declare (ignore action))
                                   (handler-case (funcall function)
                                     (error (condition)
                                       (note "restart sheet: ~a" condition)))))
    (objc:invoke alert "addAction:"
                 (objc:invoke "UIAlertAction" "actionWithTitle:style:handler:"
                              title style handler))))

(defun presenting-controller (listener)
  "The controller to present the sheet from: the listener view's window's root."
  (let ((window (objc:invoke (listener-view listener) "window")))
    (objc:invoke window "rootViewController")))

(defun show-restarts-panel (listener heading backtrace titles cancel-index)
  "Present TITLES as an action sheet under HEADING.  Thread 1."
  (declare (ignore backtrace))
  (hide-restarts-panel listener)
  (let ((alert (objc:invoke "UIAlertController"
                            "alertControllerWithTitle:message:preferredStyle:"
                            "Restarts" heading +alert-style-action-sheet+))
        (controller (getf (listener-retained listener) :restarts-controller)))
    (when controller
      (setf (controller-titles controller) titles
            (controller-cancel-index controller) cancel-index))
    (loop for title in titles
          for index from 0
          do (let ((index index))
               (add-alert-action alert title +alert-action-default+
                                 (lambda ()
                                   (let ((*listener* listener))
                                     (choose-restart index))))))
    (add-alert-action alert "Cancel" +alert-action-cancel+
                      (lambda ()
                        (let ((*listener* listener))
                          (unless (cancel-to-top-level listener)
                            (hide-restarts-panel listener)))))
    ;; On an iPad an action sheet is a popover and has to be told what it
    ;; points at; without a source it raises an exception on presentation.
    (let ((popover (objc:invoke alert "popoverPresentationController"))
          (view (listener-view listener)))
      (when (and popover (not (cffi:null-pointer-p popover)))
        (objc:invoke popover "setSourceView:" view)
        (let ((bounds (objc:invoke view "bounds")))
          (objc:invoke popover "setSourceRect:"
                       (vector (/ (aref bounds 2) 2) (/ (aref bounds 3) 2) 0d0 0d0)))))
    (setf (listener-restarts-panel listener) (objc:retain alert))
    (objc:invoke (presenting-controller listener)
                 "presentViewController:animated:completion:" alert t nil)
    alert))

(defun hide-restarts-panel (&optional (listener *listener*))
  "Dismiss the sheet if it is still up, and forget it.  Thread 1.  Idempotent.

After a tap UIKit has already dismissed it, and -presentingViewController is
nil; this only has work to do when the restart was chosen some other way, by
typing its number."
  (let ((alert (and listener (listener-restarts-panel listener))))
    (when (and alert (cffi:pointerp alert) (not (cffi:null-pointer-p alert)))
      (let ((presenter (objc:invoke alert "presentingViewController")))
        (when (and presenter (not (cffi:null-pointer-p presenter)))
          (objc:invoke alert "dismissViewControllerAnimated:completion:" t nil)))
      (objc:release alert))
    (forget-restarts listener))
  t)

(defun restarts-panel-visible-p (&optional (listener *listener*))
  (let ((alert (and listener (listener-restarts-panel listener))))
    (and alert (cffi:pointerp alert) (not (cffi:null-pointer-p alert))
         (let ((presenter (objc:invoke alert "presentingViewController")))
           (and presenter (not (cffi:null-pointer-p presenter)))))))
