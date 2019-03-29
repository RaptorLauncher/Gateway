;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; GATEWAY
;;;; © Michał "phoe" Herda 2016-2019
;;;; sql/t/player-group.lisp

(in-package #:gateway.sql/test)
(in-readtable protest/parachute)

(define-test-case player-group-select-dummy
    (:documentation "Dummy select test suite for the player group table."
     :tags (:gateway :sql :suite :positive :player-group)))

(define-test player-group-select-dummy
  :parent sql-select-dummy
  (with-sql-test ()
    (flet ((check (result i)
             (destructuring-bind (id name description) result
               (is = id i)
               (is string= name (format nil "Group ~D" i))
               (true (search (format nil "Group ~D" i) description)))))
      (loop for i from 1 to 3
            for result = (select-player-group-by-id i)
            do (check result i))
      (loop for i from 1 to 3
            for name = (format nil "Group ~D" i)
            for result = (first (select-player-groups-by-name name :limit 1))
            do (check result i)))))

(define-test-case player-group-positive
    (:documentation "Positive test suite for the player group table."
     :tags (:gateway :sql :suite :positive :player-group)))

(define-test player-group-positive
  :parent sql-positive
  (with-sql-test ()
    (let* ((test-data '(:name "Test Player Group 01"
                        :description "Description 01"))
           (inserted-id (apply #'insert-player-group test-data)))
      (flet ((verify (selected-value)
               (destructuring-bind (id name description) selected-value
                 (is = id inserted-id)
                 (is string= name (getf test-data :name))
                 (is string= description (getf test-data :description)))))
        (verify (select-player-group-by-id inserted-id))
        (verify (first (select-player-groups-by-name (getf test-data :name)
                                                     :limit 1))))
      (let ((mdf-test-data '(:name "Test Player Group 02"
                             :description "Description 02")))
        (update-player-group-name-by-id (getf mdf-test-data :name) inserted-id)
        (update-player-group-description-by-id (getf mdf-test-data :description)
                                               inserted-id)
        (destructuring-bind (id name description)
            (select-player-group-by-id inserted-id)
          (is = id inserted-id)
          (is string= name (getf mdf-test-data :name))
          (is string= description (getf mdf-test-data :description)))
        (delete-player-group-by-id inserted-id)
        (false (select-player-group-by-id inserted-id))
        (false (first (select-player-groups-by-name (getf test-data :name)
                                                    :limit 1)))))))

(define-test-case player-group-negative
    (:documentation "Negative test suite for the player group table."
     :tags (:gateway :sql :suite :negative :player-group))
  :assert
  1 "Player groups must have different IDs."
  2 "A player group's name must be valid."
  3 "A player group's description must be valid."
  4 "Trying to return a non-existent player group returns nothing."
  5 "Trying to update a non-existent player group should affect no rows."
  6 "Trying to delete a non-existent player group should affect no rows.")

(define-test player-group-negative
  :parent sql-negative
  (with-sql-test ()
    (let* ((query (sql-template '(:insert-into 'player-group :set
                                  :id $$ :name "Test Group 1"
                                  :description "Test Group 1"))))
      (pomo:query (funcall query 1))
      #1?(fail (pomo:query (funcall query 1)))
      (pomo:query (:delete-from 'player-group)))
    (flet ((insert (name)
             (insert-player-group :name name :description "Test Group")))
      #2?(db-fail (insert ""))
      #2?(db-fail (insert (long-string 65)))
      #2?(db-fail (insert (coerce '(#\a #\b #\Newline #\c #\d) 'string))))
    (flet ((insert (description)
             (insert-player-group :description description :name "Test Group")))
      #3?(db-fail (insert (long-string 8193))))
    #4?(is = 0 (nth-value 1 (select-player-group-by-id 1)))
    #4?(is = 0 (nth-value 1 (select-player-groups-by-name "Group" :limit 1)))
    #5?(is = 0 (nth-value 1 (update-player-group-name-by-id "Group" 1)))
    #5?(is = 0 (nth-value 1 (update-player-group-description-by-id "Group" 1)))
    #6?(is = 0 (nth-value 1 (delete-player-group-by-id 1)))))
