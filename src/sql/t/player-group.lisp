;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; GATEWAY
;;;; © Michał "phoe" Herda 2016-2019
;;;; sql/t/player-group.lisp

(in-package #:gateway.sql/test)
(in-readtable protest/parachute)

(define-test-case player-group
    (:documentation "Positive test suite for the player group table."
     :tags (:gateway :sql :suite :positive :player-group)))

(define-test player-group
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