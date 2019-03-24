;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; GATEWAY
;;;; © Michał "phoe" Herda 2016-2019
;;;; sql/t/tests.lisp

(in-package #:gateway.sql/test)
(in-readtable protest/parachute)

;;; TODO reader tests on dummy data

(defmacro with-sql-test (() &body body)
  `(with-test-db ()
     (handler-bind ((error (lambda (e) (declare (ignore e)) (reinstall))))
       ,@body)))

;;; Player tests

(define-test-case player
    (:documentation "Test suite for the player table."
     :tags (:gateway :sql :suite :player)))

(define-test player
  :parent sql
  (with-sql-test ()
    (let* ((test-data '(:login "gateway-01-test"
                        :email "gateway01@te.st"
                        :name "Gateway 01 Test"
                        :hash "01234567"
                        :salt "89abcdef"
                        :activatedp nil))
           (inserted-id (apply #'insert-player test-data)))
      (flet ((verify (selected-value)
               (destructuring-bind
                   (id login email name hash salt activatedp
                    creation-time last-edit-time)
                   selected-value
                 (is = id inserted-id)
                 (is string= login (getf test-data :login))
                 (is string= email (getf test-data :email))
                 (is string= name (getf test-data :name))
                 (is string= (ironclad:byte-array-to-hex-string hash)
                     (getf test-data :hash))
                 (is string= (ironclad:byte-array-to-hex-string salt)
                     (getf test-data :salt))
                 (true (eql (getf test-data :activatedp) activatedp))
                 (true (typep creation-time 'local-time:timestamp))
                 (true (typep last-edit-time 'local-time:timestamp)))))
        (verify (select-player-by-id inserted-id))
        (verify (select-player-by-login (getf test-data :login)))
        (verify (select-player-by-email (getf test-data :email)))
        (verify (first (select-players-by-name (getf test-data :name)
                                               :limit 1))))
      (let ((mdf-test-data '(:login "gateway-02-test"
                             :email "gateway02@te.st"
                             :name "Gateway 02 Test"
                             :hash "76543210"
                             :salt "fedcba98"
                             :activatedp t)))
        (update-player-login-by-id (getf mdf-test-data :login) inserted-id)
        (update-player-email-by-id (getf mdf-test-data :email) inserted-id)
        (update-player-name-by-id (getf mdf-test-data :name) inserted-id)
        (update-player-password-by-id (getf mdf-test-data :hash)
                                      (getf mdf-test-data :salt)
                                      inserted-id)
        (update-player-activatedp-by-id (getf mdf-test-data :activatedp)
                                        inserted-id)
        (destructuring-bind
            (id login email name hash salt activatedp
             creation-time last-edit-time)
            (select-player-by-id inserted-id)
          (is = id inserted-id)
          (is string= login (getf mdf-test-data :login))
          (is string= email (getf mdf-test-data :email))
          (is string= name (getf mdf-test-data :name))
          (is string= (ironclad:byte-array-to-hex-string hash)
              (getf mdf-test-data :hash))
          (is string= (ironclad:byte-array-to-hex-string salt)
              (getf mdf-test-data :salt))
          (true (eql (getf mdf-test-data :activatedp) activatedp))
          (true (typep creation-time 'local-time:timestamp))
          (true (typep last-edit-time 'local-time:timestamp)))
        (delete-player-by-id inserted-id)
        (false (select-player-by-id inserted-id))
        (false (select-player-by-login (getf test-data :login)))
        (false (select-player-by-email (getf test-data :email)))
        (false (first (select-players-by-name (getf test-data :name)
                                              :limit 1)))))))

(define-test-case player-group
    (:documentation "Test suite for the player group table."
     :tags (:gateway :sql :suite :player-group)))

(define-test player-group
  :parent sql
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
