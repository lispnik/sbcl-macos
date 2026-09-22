;;;; src/ios/restarts-sheet.lisp -- the restarts as a sheet with a table.
;;;;
;;;; iOS's half of src/restarts.lisp, which explains the design.  Where the Mac
;;;; puts up a floating NSPanel with an NSTableView, this is a UIViewController
;;;; presented as a sheet, holding a UITableView of the same titles the
;;;; transcript numbers, and a Cancel button which -- as on the Mac -- takes the
;;;; restart back to the top level rather than merely closing the sheet.
;;;;
;;;; A sheet rather than a UIAlertController action sheet, which this was first:
;;;; an alert is sized by its content and there is no supported way to ask it
;;;; for more room, so a couple of restarts came up as a stub at the bottom of
;;;; the screen.  A UISheetPresentationController takes DETENTS, so the height
;;;; is ours: it opens at the medium one, half the screen, and can be dragged up
;;;; to full height when the list is long.
;;;;
;;;; A tap does what a button on the Mac does: it TYPES the restart's number,
;;;; through CHOOSE-RESTART, because the restart belongs to the listener thread.
;;;; The sheet has no room for the backtrace; the transcript has it.
;;;;
;;;; The table's data source is the RESTARTS-CONTROLLER that src/restarts.lisp
;;;; already keeps per listener -- its TITLES slot is what a data source is for
;;;; -- so the methods below are the UIKit counterparts of the NSTableView ones
;;;; in src/macos/restarts-panel.lisp.

(in-package #:lisp-listener)

(defparameter *sheet-row-height* 52d0)
(defparameter *sheet-margin* 16d0)
(defparameter *sheet-header-height* 52d0)
(defparameter *sheet-button-height* 44d0)
(defparameter *sheet-title-font-size* 17d0)
(defparameter *sheet-row-font-size* 13d0)

;;; The table's data source and delegate --------------------------------------
;;;
;;; NSInteger is (:SIGNED :LONG-LONG); see src/macos/restarts-panel.lisp.

(objc:define-objc-method ("tableView:numberOfRowsInSection:" (:signed :long-long))
    ((self restarts-controller)
     (table objc:objc-object-pointer)
     (section (:signed :long-long)))
  (declare (ignorable table section))
  (handler-case (length (controller-titles self))
    (error (condition) (note "numberOfRowsInSection: ~a" condition) 0)))

(objc:define-objc-method ("tableView:cellForRowAtIndexPath:" objc:objc-object-pointer)
    ((self restarts-controller)
     (table objc:objc-object-pointer)
     (index-path objc:objc-object-pointer))
  (declare (ignorable table))
  (handler-case
      (let* ((row (objc:invoke index-path "row"))
             (titles (controller-titles self))
             (title (and (>= row 0) (< row (length titles)) (nth row titles))))
        (make-restart-cell (or title "")))
    (error (condition)
      (note "cellForRowAtIndexPath: ~a" condition)
      (cffi:null-pointer))))

(objc:define-objc-method ("tableView:didSelectRowAtIndexPath:" :void)
    ((self restarts-controller)
     (table objc:objc-object-pointer)
     (index-path objc:objc-object-pointer))
  (declare (ignorable table))
  (handler-case
      (let ((*listener* (or (controller-listener self) *listener*)))
        (choose-restart (objc:invoke index-path "row")))
    (error (condition) (note "didSelectRowAtIndexPath: ~a" condition))))

(defun make-restart-cell (title)
  "One row, AUTORELEASED.

Autoreleased for the reason the Mac's row views are: an object returned from a
Lisp method is the caller's to release, UIKit asks again on every redraw, and a
+1 object here would leak one per row per reload."
  (let ((cell (objc:invoke (objc:invoke "UITableViewCell" "alloc")
                           "initWithStyle:reuseIdentifier:" 0 "restart"))
        (label nil))
    (setf label (objc:invoke cell "textLabel"))
    (objc:invoke label "setText:" title)
    (objc:invoke label "setFont:" (uikit:mono-font *sheet-row-font-size*))
    ;; Two lines, then the tail is truncated: a restart's report can be far
    ;; wider than a phone, and one row growing to five lines would push the
    ;; others off the sheet.
    (objc:invoke label "setNumberOfLines:" 2)
    (objc:invoke label "setLineBreakMode:" 4)   ; NSLineBreakByTruncatingTail
    (objc:autorelease cell)))

;;; The sheet -----------------------------------------------------------------

(defun sheet-height (count)
  "How tall the sheet wants to be for COUNT restarts, in points.

Only a wish: it is resolved against the screen, and the medium detent is what
it opens at.  The table scrolls, so a long list is not a taller sheet."
  (+ *sheet-header-height* *sheet-button-height*
     (* 3 *sheet-margin*)
     (* (max 2 count) *sheet-row-height*)))

(defun make-sheet-header (heading)
  "A title and the condition's one-line summary, stacked."
  (let ((stack (uikit:new "UIStackView"))
        (title (uikit:new "UILabel"))
        (message (uikit:new "UILabel")))
    (objc:invoke stack "setAxis:" 1)              ; vertical
    (objc:invoke stack "setSpacing:" 2d0)
    (objc:invoke title "setText:" "Restarts")
    (objc:invoke title "setFont:" (uikit:bold-font *sheet-title-font-size*))
    (objc:invoke message "setText:" heading)
    (objc:invoke message "setFont:" (uikit:mono-font *sheet-row-font-size*))
    (objc:invoke message "setTextColor:" (objc:invoke "UIColor" "secondaryLabelColor"))
    (objc:invoke message "setNumberOfLines:" 2)
    (objc:invoke stack "addArrangedSubview:" title)
    (objc:invoke stack "addArrangedSubview:" message)
    stack))

(defun configure-sheet-detents (controller count)
  "Open at half the screen, and let it be dragged to full height.

-sheetPresentationController is iOS 15; on anything older this is NIL and the
sheet comes up at whatever the system chooses, which is still a sheet."
  (let ((sheet (objc:invoke controller "sheetPresentationController")))
    (when (and sheet (not (cffi:null-pointer-p sheet)))
      (let ((medium (objc:invoke "UISheetPresentationControllerDetent" "mediumDetent"))
            (large (objc:invoke "UISheetPresentationControllerDetent" "largeDetent"))
            (detents (objc:invoke "NSMutableArray" "array")))
        (objc:invoke detents "addObject:" medium)
        (objc:invoke detents "addObject:" large)
        (objc:invoke sheet "setDetents:" detents)
        (objc:invoke sheet "setPrefersGrabberVisible:" t)
        ;; A long list opens at full height instead, so that what is on offer
        ;; is on screen rather than behind a scroll.
        (when (> (sheet-height count) 520d0)
          (objc:invoke sheet "setSelectedDetentIdentifier:" "com.apple.UIKit.large")))))
  controller)

(defun build-restarts-sheet (listener heading titles cancel-index)
  "The sheet's controller, filled in.  Main thread only.

Takes finished strings rather than the restarts themselves: see RESTART-TITLES
for why they cannot be printed here."
  (let* ((controller (objc:invoke (objc:invoke "UIViewController" "alloc") "init"))
         (root (objc:invoke controller "view"))
         (data-source (getf (listener-retained listener) :restarts-controller))
         (target (and data-source (objc:objc-object-pointer data-source)))
         (header (make-sheet-header heading))
         (table (uikit:new "UITableView"))
         (cancel (uikit:system-button "Cancel")))
    (when data-source
      (setf (controller-titles data-source) titles
            (controller-cancel-index data-source) cancel-index))
    (objc:invoke root "setBackgroundColor:"
                 (objc:invoke "UIColor" "systemBackgroundColor"))
    (objc:invoke table "setDataSource:" target)
    (objc:invoke table "setDelegate:" target)
    (objc:invoke table "setRowHeight:" *sheet-row-height*)
    (objc:invoke table "setAllowsMultipleSelection:" nil)
    (objc:invoke (objc:invoke cancel "titleLabel") "setFont:"
                 (uikit:font *sheet-title-font-size*))
    (uikit:on-tap cancel
                  (lambda (sender)
                    (declare (ignore sender))
                    (let ((*listener* listener))
                      (unless (cancel-to-top-level listener)
                        (hide-restarts-panel listener)))))
    (dolist (view (list header table cancel))
      (objc:invoke root "addSubview:" view))
    (let ((safe (objc:invoke root "safeAreaLayoutGuide")))
      (uikit:pin header "topAnchor" root "topAnchor" *sheet-margin*)
      (uikit:pin header "leadingAnchor" root "leadingAnchor" *sheet-margin*)
      (uikit:pin header "trailingAnchor" root "trailingAnchor" (- *sheet-margin*))
      (uikit:pin table "topAnchor" header "bottomAnchor" *sheet-margin*)
      (uikit:pin table "leadingAnchor" root "leadingAnchor")
      (uikit:pin table "trailingAnchor" root "trailingAnchor")
      (uikit:pin table "bottomAnchor" cancel "topAnchor" (- *sheet-margin*))
      (uikit:fix cancel "heightAnchor" *sheet-button-height*)
      (uikit:pin cancel "leadingAnchor" root "leadingAnchor" *sheet-margin*)
      (uikit:pin cancel "trailingAnchor" root "trailingAnchor" (- *sheet-margin*))
      (uikit:pin cancel "bottomAnchor" safe "bottomAnchor" (- *sheet-margin*)))
    (setf (listener-restarts-table listener) table
          (listener-restarts-invoke listener) cancel)
    (configure-sheet-detents controller (length titles))
    controller))

(defun show-restarts-panel (listener heading backtrace titles cancel-index)
  "Present TITLES as a sheet under HEADING.  Thread 1."
  (declare (ignore backtrace))
  (hide-restarts-panel listener)
  (let ((controller (build-restarts-sheet listener heading titles cancel-index)))
    ;; The +1 from -alloc is the listener's, until HIDE-RESTARTS-PANEL.
    (setf (listener-restarts-panel listener) controller)
    (objc:invoke (presenting-controller listener)
                 "presentViewController:animated:completion:" controller t nil)
    controller))

(defun presenting-controller (listener)
  "The controller to present from: the listener view's window's root."
  (let ((window (objc:invoke (listener-view listener) "window")))
    (objc:invoke window "rootViewController")))

(defun hide-restarts-panel (&optional (listener *listener*))
  "Dismiss the sheet if it is up, and forget it.  Thread 1.  Idempotent.

A tap leaves the sheet on screen -- unlike an alert, which dismisses itself --
so this is what takes it down, whether the restart was chosen in the table or
by typing its number at the prompt."
  (let ((controller (and listener (listener-restarts-panel listener))))
    (when (and controller (cffi:pointerp controller)
               (not (cffi:null-pointer-p controller)))
      (let ((presenter (objc:invoke controller "presentingViewController")))
        (when (and presenter (not (cffi:null-pointer-p presenter)))
          (objc:invoke controller "dismissViewControllerAnimated:completion:" t nil)))
      (objc:release controller))
    (forget-restarts listener))
  t)

(defun restarts-panel-visible-p (&optional (listener *listener*))
  (let ((controller (and listener (listener-restarts-panel listener))))
    (and controller (cffi:pointerp controller)
         (not (cffi:null-pointer-p controller))
         (let ((presenter (objc:invoke controller "presentingViewController")))
           (and presenter (not (cffi:null-pointer-p presenter))))
         t)))

(defun restarts-table-row-count (&optional (listener *listener*))
  "How many rows the table believes it has.  Thread 1.

Asked from outside so that a test can check the data source was consulted: a
table that renders blank still answers this, and one whose data source was
never found answers zero."
  (let ((table (and listener (listener-restarts-table listener))))
    (when (and table (cffi:pointerp table) (not (cffi:null-pointer-p table)))
      (objc:invoke table "numberOfRowsInSection:" 0))))
