;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; GATEWAY
;;;; © Michał "phoe" Herda 2016-2019
;;;; sql/t/persona.lisp

(in-package #:gateway.sql/test)
(in-readtable protest/parachute)

(define-test-case persona-select-dummy
    (:documentation "Dummy select test suite for the persona table."
     :tags (:gateway :sql :suite :positive :persona)))

(define-test persona-select-dummy
  :parent sql-select-dummy
  (with-sql-test ()
    (flet ((check (result i)
             (destructuring-bind
                 (id name description creation-time last-edit-time)
                 result
               (is = id i)
               (is string= name (format nil "Persona ~D" i))
               (true (search (format nil "Persona ~D" i) description))
               (true (typep creation-time 'local-time:timestamp))
               (true (typep last-edit-time 'local-time:timestamp))
               (true (timestamp<= creation-time last-edit-time)))))
      (loop for i from 1 to 8
            for result = (select-persona-by-id i)
            do (check result i))
      (loop for i from 1 to 8
            for name = (format nil "Persona ~D" i)
            for result = (first (select-personas-by-name i :limit 1))
            do (check result i)))))

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

(define-test-case persona-negative
    (:documentation "Negative test suite for the persona table."
     :tags (:gateway :sql :suite :negative :persona))
  :assert
  1 "Personas must have different IDs."
  2 "A persona's name must be valid."
  3 "A persona's description must be valid."
  4 "Trying to return a non-existent persona returns nothing."
  5 "Trying to update a non-existent persona returns nothing."
  6 "Trying to delete a non-existent persona returns nothing."
  7"Last edit time must not be earlier than creation time.")

(define-test persona-negative
  :parent sql-negative
  (with-sql-test ()
    (let ((query (sql-template '(:insert-into 'persona :set
                                 :id $$ :name "Test Persona 1"))))
      (pomo:query (funcall query 1))
      #1?(fail (pomo:query (funcall query 1)))
      (pomo:query (:delete-from 'persona)))
    (flet ((insert (&key (name "Test Persona 1") (description ""))
             (insert-persona :name name :description description)))
      #2?(db-fail (insert :name ""))
      #2?(db-fail (insert :name (coerce '(#\Newline) 'string)))
      #2?(db-fail (insert :name (coerce '(#\a #\Newline #\b) 'string)))
      #3?(db-fail (insert :description (long-string 8193))))
    #4?(is = 0 (nth-value 1 (select-persona-by-id 1)))
    #4?(is = 0 (nth-value 1 (select-personas-by-name "Persona" :limit 1)))
    #5?(is = 0 (nth-value 1 (update-persona-name-by-id "Persona" 1)))
    #5?(is = 0 (nth-value 1 (update-persona-description-by-id "Persona" 1)))
    #6?(is = 0 (nth-value 1 (delete-persona-by-id 1)))
    (let* ((query (sql-template '(:insert-into 'persona :set :name "Test User 1"
                                  :creation-time $$ :last-edit-time $$)))
           (now (local-time:now))
           (day-before-now (local-time:timestamp- now 1 :day)))
      #7?(db-fail (pomo:query (funcall query now day-before-now))))))
