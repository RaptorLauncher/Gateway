;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; GATEWAY
;;;; © Michał "phoe" Herda 2016-2018
;;;; spellchecked-test-edit.lisp

(in-package #:gateway.ui)
(in-readtable :qtools)

(define-widget spellchecked-text-edit (qtextedit qtools-ui:fixed-qtextedit)
  ((delay :accessor delay :initarg :delay))
  (:default-initargs :delay 1000))

(define-subwidget (spellchecked-text-edit timer) (q+:make-qtimer)
  (setf (q+:single-shot timer) t))

(define-slot (spellchecked-text-edit spellchecked-text-changed) ()
  (declare (connected spellchecked-text-edit (text-changed)))
  (setf (q+:extra-selections spellchecked-text-edit) '())
  (when delay (q+:start timer delay)))

(define-slot (spellchecked-text-edit timer-timeout) ()
  (declare (connected timer (timeout)))
  (spellcheck spellchecked-text-edit))

(defgeneric spellcheck (text-edit)
  (:method ((text-edit spellchecked-text-edit))
    (with-slots-bound (text-edit spellchecked-text-edit)
      (with-finalizing ((cursor (q+:make-qtextcursor (q+:document text-edit))))
        (loop with text = (q+:to-plain-text text-edit)
              with offsets = (spell:english-check-paragraph text)
              with cursor = (q+:make-qtextcursor cursor)
              for (begin . end) in offsets
              for selection = (q+:make-qtextedit-extraselection)
              do (setf (q+:position cursor) begin
                       (q+:position cursor)
                       (values end (q+:qtextcursor.keep-anchor))
                       (q+:format selection) (make-spellcheck-qtextcharformat)
                       (q+:cursor selection) cursor)
              collect selection into selections
              finally (setf (q+:extra-selections text-edit) selections))))))

(defun make-spellcheck-qtextcharformat ()
  (let ((format (q+:make-qtextcharformat)))
    (setf (q+:underline-color format) (q+:make-qcolor "red")
          (q+:underline-style format)
          (q+:qtextcharformat.spell-check-underline))
    format))
