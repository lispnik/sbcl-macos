;;;; src/ios/history-sheet.lisp -- the history as a sheet with a search bar.
;;;;
;;;; iOS's half of src/history-search.lisp: a UIViewController presented through
;;;; a UISheetPresentationController, holding a UISearchBar above a
;;;; UITableView.  Typing narrows the list; tapping a row puts that form in the
;;;; input region ready to edit.  Opened by ⌘R on a keyboard, or the Hist key on
;;;; the bar above the on-screen one.
;;;;
;;;; It opens at the LARGE detent, unlike the restarts sheet: this is a list you
;;;; read and search, not two or three things you choose between, so the room is
;;;; worth having.

(in-package #:lisp-listener)

(defparameter *history-sheet-row-height* 44d0)
(defparameter *history-sheet-margin* 12d0)
(defparameter *history-sheet-search-height* 44d0)

;;; The table's data source and delegate, and the search bar's delegate.

(objc:define-objc-method ("tableView:numberOfRowsInSection:" (:signed :long-long))
    ((self history-controller)
     (table objc:objc-object-pointer)
     (section (:signed :long-long)))
  (declare (ignorable table section))
  (handler-case (length (history-controller-filtered self))
    (error (condition) (note "history numberOfRows: ~a" condition) 0)))

(objc:define-objc-method ("tableView:cellForRowAtIndexPath:" objc:objc-object-pointer)
    ((self history-controller)
     (table objc:objc-object-pointer)
     (index-path objc:objc-object-pointer))
  (declare (ignorable table))
  (handler-case
      (make-history-cell (or (history-row self (objc:invoke index-path "row")) ""))
    (error (condition)
      (note "history cellForRow: ~a" condition)
      (cffi:null-pointer))))

(objc:define-objc-method ("tableView:didSelectRowAtIndexPath:" :void)
    ((self history-controller)
     (table objc:objc-object-pointer)
     (index-path objc:objc-object-pointer))
  (declare (ignorable table))
  (handler-case
      (let ((listener (history-controller-listener self)))
        (let ((*listener* (or listener *listener*)))
          (choose-history-row listener (objc:invoke index-path "row"))))
    (error (condition) (note "history didSelectRow: ~a" condition))))

(objc:define-objc-method ("searchBar:textDidChange:" :void)
    ((self history-controller)
     (search-bar objc:objc-object-pointer)
     (text objc:objc-object-pointer))
  (declare (ignorable search-bar))
  (handler-case
      (let ((listener (history-controller-listener self)))
        (refilter-history self (objc:ns-string-to-string text))
        (let ((table (and listener (listener-history-table listener))))
          (when (and table (cffi:pointerp table) (not (cffi:null-pointer-p table)))
            (objc:invoke table "reloadData"))))
    (error (condition) (note "history textDidChange: ~a" condition))))

(defun make-history-cell (line)
  "One row, AUTORELEASED; see MAKE-RESTART-CELL in src/ios/restarts-sheet.lisp."
  (let* ((cell (objc:invoke (objc:invoke "UITableViewCell" "alloc")
                            "initWithStyle:reuseIdentifier:" 0 "history"))
         (label (objc:invoke cell "textLabel")))
    (objc:invoke label "setText:" (squeeze-whitespace line))
    (objc:invoke label "setFont:" (uikit:mono-font 13))
    (objc:invoke label "setNumberOfLines:" 1)
    (objc:invoke label "setLineBreakMode:" 4)   ; NSLineBreakByTruncatingTail
    (objc:autorelease cell)))

;;; The sheet ---------------------------------------------------------------------

(defun build-history-sheet (listener)
  "The sheet's controller, filled in.  Main thread only."
  (let* ((controller (objc:invoke (objc:invoke "UIViewController" "alloc") "init"))
         (root (objc:invoke controller "view"))
         (data-source (listener-history-controller listener))
         (target (and data-source (objc:objc-object-pointer data-source)))
         (search-bar (uikit:new "UISearchBar"))
         (table (uikit:new "UITableView")))
    (objc:invoke root "setBackgroundColor:"
                 (objc:invoke "UIColor" "systemBackgroundColor"))
    (objc:invoke search-bar "setPlaceholder:" "Search history")
    (objc:invoke search-bar "setAutocorrectionType:" +ui-text-autocorrection-no+)
    (objc:invoke search-bar "setAutocapitalizationType:"
                 +ui-text-autocapitalization-none+)
    (objc:invoke search-bar "setDelegate:" target)
    (objc:invoke table "setDataSource:" target)
    (objc:invoke table "setDelegate:" target)
    (objc:invoke table "setRowHeight:" *history-sheet-row-height*)
    (objc:invoke table "setAllowsMultipleSelection:" nil)
    (objc:invoke root "addSubview:" search-bar)
    (objc:invoke root "addSubview:" table)
    (let ((safe (objc:invoke root "safeAreaLayoutGuide")))
      (uikit:pin search-bar "topAnchor" root "topAnchor" *history-sheet-margin*)
      (uikit:pin search-bar "leadingAnchor" root "leadingAnchor")
      (uikit:pin search-bar "trailingAnchor" root "trailingAnchor")
      (uikit:fix search-bar "heightAnchor" *history-sheet-search-height*)
      (uikit:pin table "topAnchor" search-bar "bottomAnchor")
      (uikit:pin table "leadingAnchor" root "leadingAnchor")
      (uikit:pin table "trailingAnchor" root "trailingAnchor")
      (uikit:pin table "bottomAnchor" safe "bottomAnchor"))
    (setf (listener-history-table listener) table)
    (configure-history-detents controller)
    controller))

(defun configure-history-detents (controller)
  "Open at full height, and let it be dragged down to half."
  (let ((sheet (objc:invoke controller "sheetPresentationController")))
    (when (and sheet (not (cffi:null-pointer-p sheet)))
      (let ((detents (objc:invoke "NSMutableArray" "array")))
        (objc:invoke detents "addObject:"
                     (objc:invoke "UISheetPresentationControllerDetent" "mediumDetent"))
        (objc:invoke detents "addObject:"
                     (objc:invoke "UISheetPresentationControllerDetent" "largeDetent"))
        (objc:invoke sheet "setDetents:" detents)
        (objc:invoke sheet "setPrefersGrabberVisible:" t)
        (objc:invoke sheet "setSelectedDetentIdentifier:" "com.apple.UIKit.large"))))
  controller)

(defun show-history-popup (listener)
  "Present the list.  Thread 1."
  (hide-history-popup listener)
  (let ((controller (build-history-sheet listener)))
    ;; The +1 from -alloc is the listener's, until HIDE-HISTORY-POPUP.
    (setf (listener-history-panel listener) controller)
    (objc:invoke (presenting-controller listener)
                 "presentViewController:animated:completion:" controller t nil)
    controller))

(defun hide-history-popup (&optional (listener *listener*))
  "Dismiss the list and forget it.  Thread 1.  Idempotent."
  (let ((controller (and listener (listener-history-panel listener))))
    (when (and controller (cffi:pointerp controller)
               (not (cffi:null-pointer-p controller)))
      (let ((presenter (objc:invoke controller "presentingViewController")))
        (when (and presenter (not (cffi:null-pointer-p presenter)))
          (objc:invoke controller "dismissViewControllerAnimated:completion:" t nil)))
      (objc:release controller))
    (forget-history-popup listener))
  t)

(defun history-popup-visible-p (&optional (listener *listener*))
  (let ((controller (and listener (listener-history-panel listener))))
    (and controller (cffi:pointerp controller)
         (not (cffi:null-pointer-p controller))
         (let ((presenter (objc:invoke controller "presentingViewController")))
           (and presenter (not (cffi:null-pointer-p presenter))))
         t)))

(defun history-row-count (&optional (listener *listener*))
  "How many rows the table believes it has.  Thread 1, for the self-test."
  (let ((table (and listener (listener-history-table listener))))
    (when (and table (cffi:pointerp table) (not (cffi:null-pointer-p table)))
      (objc:invoke table "numberOfRowsInSection:" 0))))

(defun type-history-query (query &optional (listener *listener*))
  "Narrow the list to QUERY through the search bar's own delegate method,
which is the path a keystroke takes.  Thread 1."
  (let ((controller (listener-history-controller listener)))
    (when controller
      (objc:invoke (objc:objc-object-pointer controller) "searchBar:textDidChange:"
                   (cffi:null-pointer) query)
      query)))
