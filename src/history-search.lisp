;;;; src/history-search.lisp -- picking an old form out of the history.
;;;;
;;;; ⌘R opens a list of everything submitted, newest first, with a search field
;;;; above it; typing narrows the list; choosing a row puts that form in the
;;;; input region READY TO EDIT and does not submit it.  Not submitting is the
;;;; point: the form you want is usually almost the form you want.
;;;;
;;;; The filtering is here and is pure, so `make test' covers it.  The list
;;;; itself is the front end's -- an NSPanel with a table on the Mac
;;;; (src/macos/history-panel.lisp), a sheet on iOS (src/ios/history-sheet.lisp)
;;;; -- behind SHOW-HISTORY-POPUP, HIDE-HISTORY-POPUP and
;;;; HISTORY-POPUP-VISIBLE-P, the same shape the restarts panel uses.
;;;;
;;;; The history it shows is the VIEW's -- the list SRC/HISTORY.LISP loaded at
;;;; startup and SUBMIT-INPUT has been pushing to -- not a fresh read of the
;;;; file, so what was typed a moment ago is in it.

(in-package #:lisp-listener)

(defparameter *history-popup-rows* 300
  "How many rows the list will show.  The history is capped at
*HISTORY-LIMIT* anyway; this is a second bound so that a hand-edited file
cannot make the panel take a noticeable time to build.")

(defun history-lines (view)
  "What the list shows: the view's history, newest first, without repeats.

Repeats are dropped for the list only -- the file keeps them, because it is a
record of what was typed and this is a menu of what might be typed again."
  (when view
    (let ((seen (make-hash-table :test 'equal))
          (lines '()))
      (dolist (line (view-history view) (nreverse lines))
        (unless (gethash line seen)
          (setf (gethash line seen) t)
          (push line lines))))))

(defun history-search (lines query)
  "The lines of LINES that match QUERY, in order.

Every whitespace-separated term in QUERY must appear in the line, in any
position and in any order, ignoring case: `list map' finds
(mapcar #'list ...) as readily as (list (mapcar ...)).  An empty query matches
everything, which is what the list shows when it opens."
  (let ((terms (remove "" (split-on-whitespace (or query "")) :test #'string=)))
    (let ((matches (if (null terms)
                       (copy-list lines)
                       (remove-if-not
                        (lambda (line)
                          (every (lambda (term) (search term line :test #'char-equal))
                                 terms))
                        lines))))
      (if (> (length matches) *history-popup-rows*)
          (subseq matches 0 *history-popup-rows*)
          matches))))

(defun split-on-whitespace (string)
  (let ((terms '()) (start nil))
    (dotimes (index (length string))
      (let ((whitespace (member (char string index)
                                '(#\Space #\Tab #\Newline #\Return #\Page))))
        (cond ((and whitespace start)
               (push (subseq string start index) terms)
               (setf start nil))
              ((not (or whitespace start)) (setf start index)))))
    (when start (push (subseq string start) terms))
    (nreverse terms)))

;;; The controller ---------------------------------------------------------------
;;;
;;; One per listener, holding what the table is showing.  A data source is
;;; asked for its rows whenever the toolkit feels like redrawing, long after the
;;; panel was built, so the rows have to live somewhere that outlives the call
;;; that made them -- the same reasoning as the restarts controller, and the
;;; table methods are likewise the front ends' (an NSTableView and a
;;; UITableView answer different selectors).

(objc:define-objc-class history-controller ()
  ((listener :initform nil :accessor history-controller-listener
             :documentation "The listener whose input region a chosen row goes
into.  Not *LISTENER*: a background window is perfectly able to have its
history open.")
   (lines :initform '() :accessor history-controller-lines
          :documentation "Every line on offer, newest first.")
   (filtered :initform '() :accessor history-controller-filtered
             :documentation "The lines the query matched: what the table shows."))
  (:objc-class-name "LispListenerHistoryController"))

(defun listener-history-controller (listener)
  (and listener (getf (listener-retained listener) :history-controller)))

(defun refilter-history (controller query)
  "Narrow the controller's rows to QUERY, and answer them."
  (setf (history-controller-filtered controller)
        (history-search (history-controller-lines controller) query)))

(defun history-row (controller row)
  "The line at ROW, or NIL when there is none -- which a table may well ask
about, between a query being typed and the table being told to reload."
  (let ((lines (history-controller-filtered controller)))
    (when (and (>= row 0) (< row (length lines)))
      (nth row lines))))

;;; Opening, choosing, closing ---------------------------------------------------

(defun open-history-popup (&optional (listener (current-listener)))
  "Put the history on screen.  Thread 1.  True when there was anything to show."
  (let* ((view (and listener (listener-view-object listener)))
         (lines (history-lines view))
         (controller (listener-history-controller listener)))
    (cond ((null lines)
           (note "history: nothing submitted yet")
           nil)
          ((null controller) nil)
          (t
           (setf (history-controller-listener controller) listener
                 (history-controller-lines controller) lines)
           (refilter-history controller "")
           (show-history-popup listener)
           t))))

(defun choose-history-line (listener line)
  "Put LINE in the input region, ready to edit, and take the list down.

Thread 1.  Deliberately NOT submitted: what you wanted is usually a small edit
away from what you ran before."
  (let ((view (and listener (listener-view-object listener)))
        (pointer (and listener (listener-view listener))))
    (when (and view pointer line)
      (hide-history-popup listener)
      (replace-pending-input view pointer line)
      (let ((end (transcript-length pointer)))
        (objc:invoke pointer "setSelectedRange:" (cons end 0))
        (objc:invoke pointer "scrollRangeToVisible:" (cons end 0)))
      (refresh-paren-highlight view pointer)
      line)))

(defun choose-history-row (listener row)
  "Choose the line at ROW.  What a double click and Return both do."
  (let ((controller (listener-history-controller listener)))
    (when controller
      (choose-history-line listener (history-row controller row)))))

(defun forget-history-popup (listener)
  "Clear what a list that is going away leaves behind: the panel and the table.
Thread 1.

NOT the controller's rows.  SHOW-HISTORY-POPUP takes any previous list down
before building the new one, so clearing the rows here emptied the very list
OPEN-HISTORY-POPUP had just put there and the table came up with nothing in it.
Stale rows are harmless -- the next open replaces them -- and the table is gone
by then in any case."
  (when listener
    (setf (listener-history-panel listener) nil
          (listener-history-table listener) nil))
  t)
