;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; GATEWAY
;;;; © Michał "phoe" Herda 2016-2018
;;;; engine/t/data-object.lisp

(in-package #:gateway.engine/test)
(in-readtable protest/parachute)

;;; Test suite

(define-test-case data-object
    (:documentation "Test suite for DATA-OBJECT."
     :tags (:gateway :engine :condition :data-object :suite)))

(define-test data-object
  :parent engine)

;;; DATA-OBJECT test

;; TODO same test for DATA-MESSAGE
(defparameter *data-object-input*
  (uiop:while-collecting (collect)
    (let ((reason "a") (expression '(1 2 :foo :bar)))
      (dolist (class '(message-read-error invalid-message-id
                       invalid-message-type invalid-message-body))
        (collect `(,(make-instance class :expression expression)
                   (,class ,expression)))
        (collect `(,(make-instance class :expression expression :reason reason)
                   (,class ,expression ,reason)))))))

(defun data-object-untested-classes ()
  (let* ((subclasses (append (subclasses (find-class 'gateway-object))
                             (subclasses (find-class 'gateway-condition))))
         (concrete-subclasses (remove-if #'protocol-object-p subclasses))
         (hash-table (make-hash-table)))
    (dolist (class concrete-subclasses)
      (setf (gethash class hash-table) t))
    (dolist (entry *data-object-input*)
      (remhash (class-of (first entry)) hash-table))
    (hash-table-keys hash-table)))

(define-test-case data-object-all-objects
    (:documentation "Test of DATA-OBJECT and OBJECT-DATA for all concrete
Gateway objects.
\
The test body runs for each entry in *DATA-OBJECT-INPUT*, which in turn contains
two test inputs, an object (indirect instance of GATEWAY-OBJECT or
GATEWAY-CONDITION) and cable data matching the object. The test asserts whether
calling DATA-OBJECT and OBJECT-DATA on the object and data produces consistent
results."
     :tags (:gateway :data-object :gateway-object :gateway-condition))
  :precondition
  1 "Assert that all concrete Gateway classes and condition types are tested."
  :act
  2 "Convert the object into new data."
  3 "Convert data into a new object."
  4 "Convert the new object into new new data."
  :assert
  5 "Assert that the object and the new object are of the same class."
  6 "Assert that data and new data are cable-equal."
  7 "Assert that new data and new new data are cable-equal.")

(define-test data-object-all-objects
  :parent data-object
  #1?(false (data-object-untested-classes))
  (loop for (object data) in *data-object-input*
        for new-data = #2?(object-data object)
        for new-object = #3?(data-object data)
        for new-new-data = #4?(object-data new-object)
        do #5?(is eql (class-of object) (class-of new-object))
        do #6?(is cable-equal data new-data)
        do #7?(is cable-equal new-data new-new-data)))
