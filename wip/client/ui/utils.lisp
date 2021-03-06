;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; GATEWAY
;;;; © Michał "phoe" Herda 2017-2019
;;;; utils.lisp

(in-package :gateway.client.ui)
(in-readtable :qtools)

(defun homepath (filename)
  (uiop:native-namestring
   (uiop:nest
    (merge-pathnames filename)
    (merge-pathnames "Projects/Raptor Chat/")
    (user-homedir-pathname))))

;;; PLACEHOLDER-TEXT-EDIT

(defun make-placeholder-text-edit (placeholder)
  (make-instance 'qui:placeholder-text-edit :placeholder placeholder))

;;; PLACECHCKED-TEXT-EDIT

(define-widget input-text-edit
      (qtextedit qui:spellchecked-text-edit qui:placeholder-text-edit) ())

(defun make-input-text-edit (placeholder)
  (make-instance 'input-text-edit :placeholder placeholder))
