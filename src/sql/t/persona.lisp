;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; GATEWAY
;;;; © Michał "phoe" Herda 2016-2019
;;;; sql/t/persona.lisp

(in-package #:gateway.sql/test)
(in-readtable protest/parachute)

(define-test-case persona-positive
    (:documentation "Positive test suite for the persona table."
     :tags (:gateway :sql :suite :positive :persona)))

(define-test persona-positive
  :parent sql-positive
  (with-sql-test ()
    (let* ((test-data '(:name "Test Persona 01"
                        :description "Description 01"))
           (inserted-id (apply #'insert-persona test-data)))
      (flet ((verify (selected-value)
               (destructuring-bind
                   (id name description creation-time last-edit-time)
                   selected-value
                 (is = id inserted-id)
                 (is string= name (getf test-data :name))
                 (is string= description (getf test-data :description))
                 (true (typep creation-time 'local-time:timestamp))
                 (true (typep last-edit-time 'local-time:timestamp)))))
        (verify (select-persona-by-id inserted-id))
        (verify (first (select-personas-by-name (getf test-data :name)
                                                :limit 1))))
      (let ((mdf-test-data '(:name "Test Persona 02"
                             :description "Description 02")))
        (update-persona-name-by-id (getf mdf-test-data :name) inserted-id)
        (update-persona-description-by-id (getf mdf-test-data :description)
                                          inserted-id)
        (destructuring-bind
            (id name description creation-time last-edit-time)
            (select-persona-by-id inserted-id)
          (is = id inserted-id)
          (is string= name (getf mdf-test-data :name))
          (is string= description (getf mdf-test-data :description))
          (true (typep creation-time 'local-time:timestamp))
          (true (typep last-edit-time 'local-time:timestamp)))
        (delete-persona-by-id inserted-id)
        (false (select-persona-by-id inserted-id))
        (false (first (select-personas-by-name (getf test-data :name)
                                               :limit 1)))))))
