;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; GATEWAY
;;;; © Michał "phoe" Herda 2016-2019
;;;; sql/t/timeline.lisp

(in-package #:gateway.sql/test)
(in-readtable protest/parachute)

(define-test-case timeline-select-dummy
    (:documentation "Dummy select test suite for the timeline table."
     :tags (:gateway :sql :suite :positive :timeline)))

(define-test timeline-select-dummy
  :parent sql-select-dummy
  (with-sql-test ()
    (flet ((check (result i)
             (destructuring-bind
                 (id name description creation-time last-edit-time)
                 result
               (is = id i)
               (is string= name (format nil "Timeline ~D" i))
               (true (search (format nil "Timeline ~D" i) description))
               (true (typep creation-time 'local-time:timestamp))
               (true (typep last-edit-time 'local-time:timestamp))
               (true (timestamp<= creation-time last-edit-time)))))
      (loop for i from 1 to 6
            for result = (select-timeline-by-id i)
            do (check result i))
      (loop for i from 1 to 6
            for name = (format nil "Timeline ~D" i)
            for result = (first (select-timelines-by-name i :limit 1))
            do (check result i)))))

(define-test-case timeline-positive
    (:documentation "Positive test suite for the timeline table."
     :tags (:gateway :sql :suite :positive :timeline)))

(define-test timeline-positive
  :parent sql-positive
  (with-sql-test ()
    (let* ((test-data '(:name "Test Timeline 01"
                        :description "Description 01"))
           (inserted-id (apply #'insert-timeline test-data)))
      (flet ((verify (selected-value)
               (destructuring-bind
                   (id name description creation-time last-edit-time)
                   selected-value
                 (is = id inserted-id)
                 (is string= name (getf test-data :name))
                 (is string= description (getf test-data :description))
                 (true (typep creation-time 'local-time:timestamp))
                 (true (typep last-edit-time 'local-time:timestamp)))))
        (verify (select-timeline-by-id inserted-id))
        (verify (first (select-timelines-by-name (getf test-data :name)
                                                :limit 1))))
      (let ((mdf-test-data '(:name "Test Timeline 02"
                             :description "Description 02")))
        (is = 1 (nth-value 1 (update-timeline-name-by-id
                              (getf mdf-test-data :name) inserted-id)))
        (is = 1 (nth-value 1 (update-timeline-description-by-id
                              (getf mdf-test-data :description) inserted-id)))
        (destructuring-bind
            (id name description creation-time last-edit-time)
            (select-timeline-by-id inserted-id)
          (is = id inserted-id)
          (is string= name (getf mdf-test-data :name))
          (is string= description (getf mdf-test-data :description))
          (true (typep creation-time 'local-time:timestamp))
          (true (typep last-edit-time 'local-time:timestamp)))
        (is = 1 (nth-value 1 (delete-timeline-by-id inserted-id)))
        (is = 0 (nth-value 1 (select-timeline-by-id inserted-id)))
        (is = 0 (nth-value 1 (select-timelines-by-name (getf test-data :name)
                                                      :limit 1)))))))

(define-test-case timeline-negative
    (:documentation "Negative test suite for the timeline table."
     :tags (:gateway :sql :suite :negative :timeline))
  :assert
  1 "Timelines must have different IDs."
  2 "A timeline's name must be valid."
  3 "A timeline's description must be valid."
  4 "Trying to return a non-existent timeline returns nothing."
  5 "Trying to update a non-existent timeline returns nothing."
  6 "Trying to delete a non-existent timeline returns nothing."
  7"Last edit time must not be earlier than creation time.")

(define-test timeline-negative
  :parent sql-negative
  (with-sql-test ()
    (let ((query (sql-template '(:insert-into 'timeline :set
                                 :id $$ :name "Test Timeline 1"))))
      (pomo:query (funcall query 1))
      #1?(fail (pomo:query (funcall query 1)))
      (pomo:query (:delete-from 'timeline)))
    (flet ((insert (&key (name "Test Timeline 1") (description ""))
             (insert-timeline :name name :description description)))
      #2?(db-fail (insert :name ""))
      #2?(db-fail (insert :name (coerce '(#\Newline) 'string)))
      #2?(db-fail (insert :name (coerce '(#\a #\Newline #\b) 'string)))
      #3?(db-fail (insert :description (long-string 8193))))
    #4?(is = 0 (nth-value 1 (select-timeline-by-id 1)))
    #4?(is = 0 (nth-value 1 (select-timelines-by-name "Timeline" :limit 1)))
    #5?(is = 0 (nth-value 1 (update-timeline-name-by-id "Timeline" 1)))
    #5?(is = 0 (nth-value 1 (update-timeline-description-by-id "Timeline" 1)))
    #6?(is = 0 (nth-value 1 (delete-timeline-by-id 1)))
    (let* ((query (sql-template '(:insert-into 'timeline
                                  :set :name "Test User 1"
                                  :creation-time $$ :last-edit-time $$)))
           (now (local-time:now))
           (day-before-now (local-time:timestamp- now 1 :day)))
      #7?(db-fail (pomo:query (funcall query now day-before-now))))))
