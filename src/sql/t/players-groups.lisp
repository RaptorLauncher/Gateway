;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; GATEWAY
;;;; © Michał "phoe" Herda 2016-2019
;;;; sql/t/players-groups.lisp

(in-package #:gateway.sql/test)
(in-readtable protest/parachute)

(defparameter *players-groups-select-dummy-data*
  '((1 1 t) (2 1 nil) (3 1 nil) (4 1 nil)
    (3 2 t) (4 2 t)   (5 2 t)
    (2 3 t) (4 3 nil) (6 3 nil) (8 3 nil)))

(define-test-case players-groups-select-dummy
    (:documentation
     "Dummy select test suite for the table binding players to groups."
     :tags (:gateway :sql :suite :select-dummy :player :player-group
            :players-groups)))

(define-test players-groups-select-dummy
  :parent sql-select-dummy)

(define-test-case players-groups-select-dummy-select-player-owner-of-group-p
    (:documentation "Check if SELECT-PLAYER-OWNER-OF-GROUP-P works."
     :tags (:gateway :sql :suite :select-dummy :player :player-group
            :players-groups)))

(define-test players-groups-select-dummy-select-player-owner-of-group-p
  :parent players-groups-select-dummy
  (with-sql-test ()
    (dolist (player (iota 8 :start 1))
      (dolist (group (iota 3 :start 1))
        (flet ((ownerp (x) (and (eql (first x) player)
                                (eql (second x) group))))
          (let* ((result (select-player-owner-of-group-p player group))
                 (match (find-if #'ownerp *players-groups-select-dummy-data*))
                 (match (if match (third match) :null)))
            (is eq result match)))))))

(define-test-case players-groups-select-players-belonging-to-group
    (:documentation "Check if SELECT-PLAYERS-BELONGING-TO-GROUP works."
     :tags (:gateway :sql :suite :select-dummy :player :player-group
            :players-groups)))

(define-test players-groups-select-players-belonging-to-group
  :parent players-groups-select-dummy
  (with-sql-test ()
    (dolist (group (iota 3 :start 1))
      (let* ((result (select-players-belonging-to-group group))
             (result (mapcar (lambda (x) (list (first x) (tenth x))) result)))
        (dolist (cell result)
          (destructuring-bind (player ownerp) cell
            (flet ((find-data (x) (and (eql (first x) player)
                                       (eql (second x) group)
                                       (eql (third x) ownerp))))
              (true (find-if #'find-data
                             *players-groups-select-dummy-data*)))))))))

(define-test-case players-groups-select-groups-player-belongs-to
    (:documentation "Check if SELECT-GROUPS-PLAYER-BELONGS-TO works."
     :tags (:gateway :sql :suite :select-dummy :player :player-group
            :players-groups)))

(define-test players-groups-select-groups-player-belongs-to
  :parent players-groups-select-dummy
  (with-sql-test ()
    (dolist (player (iota 8 :start 1))
      (let* ((result (select-groups-player-belongs-to player))
             (result (mapcar (lambda (x) (list (first x) (fourth x))) result)))
        (dolist (cell result)
          (destructuring-bind (group ownerp) cell
            (flet ((find-data (x) (and (eql (first x) player)
                                       (eql (second x) group)
                                       (eql (third x) ownerp))))
              (true (find-if #'find-data
                             *players-groups-select-dummy-data*)))))))))

(define-test-case players-groups-positive
    (:documentation
     "Positive test suite for the table mapping players to player groups."
     :tags (:gateway :sql :suite :positive :player :player-group
            :players-groups)))

(define-test players-groups-positive
  :parent sql-positive
  (with-sql-test ()
    (let* ((player-1 '(:login "gateway-01-test"
                       :email "gateway01@te.st"
                       :name "Gateway 01 Test"))
           (player-2 '(:login "gateway-02-test"
                       :email "gateway02@te.st"
                       :name "Gateway 02 Test"))
           (player-3 '(:login "gateway-03-test"
                       :email "gateway03@te.st"
                       :name "Gateway 03 Test"))
           (group-1 '(:name "Test Player Group 01"))
           (group-2 '(:name "Test Player Group 02")))
      (destructuring-bind (pid1 pid2 pid3)
          (mapcar (curry #'apply #'insert-player
                         :hash "" :salt "" :activatedp t)
                  (list player-1 player-2 player-3))
        (destructuring-bind (gid1 gid2)
            (mapcar (curry #'apply #'insert-player-group :description "")
                    (list group-1 group-2))
          (is = 1 (nth-value 1 (add-player-into-player-group pid1 gid1 t)))
          (is = 1 (nth-value 1 (add-player-into-player-group pid2 gid1 nil)))
          (is = 1 (nth-value 1 (add-player-into-player-group pid1 gid2 t)))
          ;; TODO add IS = 1 NTH-VALUE 1 in all positive tests
          (is eq t (select-player-owner-of-group-p pid1 gid1))
          (is eq nil (select-player-owner-of-group-p pid2 gid1))
          (is eq :null (select-player-owner-of-group-p pid3 gid1))
          (is eq t (select-player-owner-of-group-p pid1 gid2))
          (is eq :null (select-player-owner-of-group-p pid2 gid2))
          (is eq :null (select-player-owner-of-group-p pid3 gid2))
          (flet ((verify (gid result)
                   (let* ((fn (lambda (x) (list (first x) (tenth x))))
                          (results (select-players-belonging-to-group gid))
                          (results (mapcar fn results)))
                     (true (set-equal results result :test #'equal)))))
            (verify gid1 `((,pid1 t) (,pid2 nil)))
            (verify gid2 `((,pid1 t))))
          (flet ((verify (pid result)
                   (let* ((fn (lambda (x) (list (first x) (fourth x))))
                          (results (select-groups-player-belongs-to pid))
                          (results (mapcar fn results)))
                     (true (set-equal results result :test #'equal)))))
            (verify pid1 `((,gid1 t) (,gid2 t)))
            (verify pid2 `((,gid1 nil)))
            (verify pid3 '()))
          (is = 1 (nth-value 1 (update-player-group-owner t pid2 gid1)))
          (is eq t (select-player-owner-of-group-p pid2 gid1))
          (is = 1 (nth-value 1 (update-player-group-owner nil pid2 gid1)))
          (is eq nil (select-player-owner-of-group-p pid2 gid1))
          (is = 0 (nth-value 1 (upsert-player-group-owner-when-owner
                                t pid3 gid1 pid2 gid1)))
          (is = 1 (nth-value 1 (upsert-player-group-owner-when-owner
                                t pid3 gid1 pid1 gid1)))
          (is eq t (select-player-owner-of-group-p pid3 gid1))
          (is = 1 (nth-value 1 (remove-player-from-player-group pid1 gid1)))
          (is eq :null (select-player-owner-of-group-p pid1 gid1))
          (is = 1 (nth-value 1 (remove-player-from-player-group pid3 gid1)))
          (is eq :null (select-player-owner-of-group-p pid3 gid1))
          (is = 1 (nth-value 1 (remove-player-from-player-group pid1 gid2)))
          (is eq :null (select-player-owner-of-group-p pid1 gid2))
          (true (table-empty-p "players_personas"))
          (mapc #'delete-player-by-id (list pid1 pid2 pid3))
          (mapc #'delete-player-group-by-id (list gid1 gid2)))))))

(define-test-case players-groups-negative
    (:documentation
     "Negative test suite for the table mapping players to player groups."
     :tags (:gateway :sql :suite :negative :player :player-group
            :players-groups))
  :assert
  1  "It is impossible to add a non-existent player to a group."
  2  "It is impossible to add a player to a non-existent group."
  3  "Trying to check if a nonexistent player is an owner of a group should ~
return null."
  4  "Trying to check if a player is an owner of a nonexistent group should ~
return null."
  5  "Selecting players belonging to a nonexistent group should return no rows."
  6  "Selecting groups a nonexistent player belongs go should return no rows."
  7  "Updating group ownership status of a nonexistent player should affect no ~
rows."
  8  "Updating group ownership status of a nonexistent group should affect no ~
rows."
  9  "Removing players from a nonexistent group should affect no rows."
  10 "Removing nonexistent players from a group should affect no rows."
  11 "It is impossible to upsert player group owner of a non-existent ~
to-be-updated player."
  12 "It is impossible to upsert player group owner of a non-existent ~
to-be-upserted group."
  13 "Upserting a player group owner should affect no rows if the permissioned ~
player does not exist."
  14 "Upserting a player group owner should affect no rows if the permissioned ~
group does not exist."
  15 "Upserting a player group owner should affect no rows if the player is ~
not an owner of the group.")

(define-test players-groups-negative
  :parent sql-negative
  (with-sql-test ()
    (flet ((ub8 (n) (make-array n :element-type '(unsigned-byte 8))))
      (let ((group (insert-player-group :name "Player Group" :description "")))
        #1?(db-fail (add-player-into-player-group 0 group t))

        #3?(is eq :null (select-player-owner-of-group-p 0 group))
        #7?(is = 0 (nth-value 1 (update-player-group-owner t 0 group)))
        #9?(is = 0 (nth-value 1 (remove-player-from-player-group 0 group)))
        (delete-player-group-by-id group))
      (let ((player (insert-player :login "gateway01" :email "ga1@te.way"
                                   :name "Player" :hash (ub8 0) :salt (ub8 0)
                                   :activatedp nil)))
        #2?(db-fail (add-player-into-player-group player 0 t))
        #4?(is eq :null (select-player-owner-of-group-p player 0))
        #8?(is = 0 (nth-value 1 (update-player-group-owner t player 0)))
        #10?(is = 0 (nth-value 1 (remove-player-from-player-group player 0)))
        (delete-player-by-id player))
      (let ((player (insert-player :login "gateway01" :email "ga1@te.way"
                                   :name "Player" :hash (ub8 0) :salt (ub8 0)
                                   :activatedp nil))
            (group (insert-player-group :name "Player Group" :description "")))
        (add-player-into-player-group player group t)
        #11?(db-fail (upsert-player-group-owner-when-owner
                      t 0 group player group))
        #12?(db-fail (upsert-player-group-owner-when-owner
                      t player 0 player group))
        #13?(is = 0 (nth-value 1 (upsert-player-group-owner-when-owner
                                  t player group 0 group)))
        #14?(is = 0 (nth-value 1 (upsert-player-group-owner-when-owner
                                  t player group player 0)))
        (update-player-group-owner nil player group)
        #15?(is = 0 (nth-value 1 (upsert-player-group-owner-when-owner
                                  t player group player group)))
        #15?(is = 0 (nth-value 1 (upsert-player-group-owner-when-owner
                                  nil player group player group)))
        (delete-player-by-id player)
        (delete-player-group-by-id group)))
    #5?(is = 0 (nth-value 1 (select-players-belonging-to-group 0)))
    #6?(is = 0 (nth-value 1 (select-groups-player-belongs-to 0)))))
