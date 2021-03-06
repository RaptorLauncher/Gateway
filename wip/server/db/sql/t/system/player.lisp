;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; GATEWAY
;;;; © Michał "phoe" Herda 2016-2019
;;;; sql/t/player.lisp

(in-package #:gateway.sql/test)
(in-readtable protest/parachute)

(define-test-case player-select-dummy
    (:documentation "Dummy select test suite for the player table."
     :tags (:gateway :sql :suite :select-dummy :player)))

(define-test player-select-dummy
  :parent sql-select-dummy
  (with-sql-test ()
    (flet ((check (result i)
             (destructuring-bind (id login email name hash salt activatedp
                                  creation-time last-edit-time)
                 result
               (is = id i)
               (is string= login (format nil "player~D" i))
               (is string= email (format nil "player~D@gate.way" i))
               (is string= name (format nil "Player ~D" i))
               (is vector= hash (vector i))
               (is vector= salt (vector i))
               (is eq activatedp (oddp i))
               (true (typep creation-time 'local-time:timestamp))
               (true (typep last-edit-time 'local-time:timestamp))
               (true (timestamp<= creation-time last-edit-time)))))
      (loop for i from 1 to 8
            for result = (select-player-by-id i)
            do (check result i))
      (loop for i from 1 to 8
            for login = (format nil "player~D" i)
            for result = (select-player-by-login login)
            do (check result i))
      (loop for i from 1 to 8
            for email = (format nil "player~D@gate.way" i)
            for result = (select-player-by-email email)
            do (check result i))
      (loop for i from 1 to 8
            for name = (format nil "Player ~D" i)
            for result = (first (select-players-by-name name :limit 1))
            do (check result i)))))

(define-test-case player-positive
    (:documentation "Positive test suite for the player table."
     :tags (:gateway :sql :suite :positive :player)))

(define-test player-positive
  :parent sql-positive
  (with-sql-test ()
    (let* ((data `(:login "gateway-01-test"
                   :email "gateway01@te.st"
                   :name "Gateway 01 Test"
                   :hash ,(ironclad:hex-string-to-byte-array "01234567")
                   :salt ,(ironclad:hex-string-to-byte-array "89abcdef")
                   :activatedp nil))
           (inserted-id (apply #'insert-player data)))
      (flet ((verify (selected-value)
               (destructuring-bind
                   (id login email name hash salt activatedp
                    creation-time last-edit-time)
                   selected-value
                 (is = id inserted-id)
                 (is string= login (getf data :login))
                 (is string= email (getf data :email))
                 (is string= name (getf data :name))
                 (is vector= hash (getf data :hash))
                 (is vector= salt (getf data :salt))
                 (true (eql (getf data :activatedp) activatedp))
                 (true (typep creation-time 'local-time:timestamp))
                 (true (typep last-edit-time 'local-time:timestamp)))))
        (verify (select-player-by-id inserted-id))
        (verify (select-player-by-login (getf data :login)))
        (verify (select-player-by-email (getf data :email)))
        (verify (first (select-players-by-name (getf data :name) :limit 1))))
      (let ((data2 `(:login "gateway-02-test"
                     :email "gateway02@te.st"
                     :name "Gateway 02 Test"
                     :hash ,(ironclad:hex-string-to-byte-array "76543210")
                     :salt ,(ironclad:hex-string-to-byte-array "fedcba98")
                     :activatedp t)))
        (is = 1 (nth-value 1 (update-player-login-by-id (getf data2 :login)
                                                        inserted-id)))
        (is = 1 (nth-value 1 (update-player-email-by-id (getf data2 :email)
                                                        inserted-id)))
        (is = 1 (nth-value 1 (update-player-name-by-id (getf data2 :name)
                                                       inserted-id)))
        (is = 1 (nth-value 1 (update-player-password-by-id (getf data2 :hash)
                                                           (getf data2 :salt)
                                                           inserted-id)))
        (is = 1 (nth-value 1 (update-player-activatedp-by-id
                              (getf data2 :activatedp) inserted-id)))
        (destructuring-bind
            (id login email name hash salt activatedp
             creation-time last-edit-time)
            (select-player-by-id inserted-id)
          (is = id inserted-id)
          (is string= login (getf data2 :login))
          (is string= email (getf data2 :email))
          (is string= name (getf data2 :name))
          (is vector= hash (getf data2 :hash))
          (is vector= salt (getf data2 :salt))
          (is eq activatedp (getf data2 :activatedp))
          (true (typep creation-time 'local-time:timestamp))
          (true (typep last-edit-time 'local-time:timestamp)))
        (is = 1 (nth-value 1 (delete-player-by-id inserted-id)))
        (is = 0 (nth-value 1 (select-player-by-id inserted-id)))
        (is = 0 (nth-value 1 (select-player-by-login (getf data :login))))
        (is = 0 (nth-value 1 (select-player-by-email (getf data :email))))
        (is = 0 (nth-value 1 (select-players-by-name (getf data :name)
                                                     :limit 1)))))))

(define-test-case player-negative
    (:documentation "Negative test suite for the player table."
     :tags (:gateway :sql :suite :negative :player))
  :assert
  1 "Players must have different IDs."
  2 "A player's login must be valid."
  3 "A player's name must be valid."
  4 "A player's email must be valid."
  5 "A player's password hash and salt must be no more than 256 bytes long."
  6 "Trying to return a non-existent player returns nothing."
  7 "Trying to update a non-existent player should affect no rows."
  8 "Trying to delete a non-existent player should affect no rows."
  9 "Last edit time must not be earlier than creation time.")

(define-test player-negative
  :parent sql-negative
  (with-sql-test ()
    (flet ((ub8 (n) (make-array n :element-type '(unsigned-byte 8))))
      (let ((query (sql-template '(:insert-into 'player :set
                                   :id $$ :login "gateway01"
                                   :email "ga1@te.way" :name "Test User 1"))))
        (pomo:query (funcall query 1))
        #1?(fail (pomo:query (funcall query 1)))
        (pomo:query (:delete-from 'player)))
      (flet ((insert (login)
               (insert-player :login login :email "ga@te.way" :name "User"
                              :hash "" :salt "" :activatedp t)))
        #2?(db-fail (insert ""))
        #2?(db-fail (insert "aa"))
        #2?(db-fail (insert "!@#$%"))
        #2?(db-fail (insert (long-string 65)))
        #2?(db-fail (insert (coerce '(#\Newline) 'string))))
      (flet ((insert (&key (email "") (name ""))
               (insert-player :login "GatewayTest1" :email email
                              :name name :hash "" :salt "" :activatedp t)))
        #3?(db-fail (insert :email "ga@te.way"))
        #3?(db-fail (insert :email (uiop:strcat "ga@te." (long-string 251))))
        #4?(db-fail (insert :name "Test User 1"))
        #4?(db-fail (insert :name (coerce '(#\a #\b #\Newline #\c #\d)
                                          'string)))
        #4?(db-fail (insert :name (long-string 257))))
      (flet ((insert (&key (hash (ub8 0)) (salt (ub8 0)))
               (insert-player :login "GatewayTest1" :email "ga@te.way"
                              :name "Test User 1" :hash hash :salt salt
                              :activatedp t)))
        #5?(db-fail (insert :hash (ub8 257)))
        #5?(db-fail (insert :salt (ub8 257))))
      #6?(is = 0 (nth-value 1 (select-player-by-id 1)))
      #6?(is = 0 (nth-value 1 (select-player-by-login "GatewayTest1")))
      #6?(is = 0 (nth-value 1 (select-player-by-email "ga@te.way")))
      #6?(is = 0 (nth-value 1 (select-players-by-name "User" :limit 1)))
      #7?(is = 0 (nth-value 1 (update-player-login-by-id "abc" 1)))
      #7?(is = 0 (nth-value 1 (update-player-email-by-id "ga@we.tay" 1)))
      #7?(is = 0 (nth-value 1 (update-player-name-by-id "Use Tester 1" 1)))
      #7?(is = 0 (nth-value 1 (update-player-password-by-id (ub8 0) (ub8 0) 1)))
      #7?(is = 0 (nth-value 1 (update-player-activatedp-by-id t 1)))
      #8?(is = 0 (nth-value 1 (delete-player-by-id 1)))
      (let* ((query (sql-template '(:insert-into 'player :set
                                    :id 1 :login "gateway01"
                                    :email "ga1@te.way" :name "Test User 1"
                                    :creation-time $$ :last-edit-time $$)))
             (now (local-time:now))
             (day-before-now (local-time:timestamp- now 1 :day)))
        #9?(db-fail (pomo:query (funcall query now day-before-now)))))))
