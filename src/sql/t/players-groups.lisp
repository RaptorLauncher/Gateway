;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; GATEWAY
;;;; © Michał "phoe" Herda 2016-2019
;;;; sql/t/players-groups.lisp

(in-package #:gateway.sql/test)
(in-readtable protest/parachute)

(define-test-case players-groups
    (:documentation
     "Positive est suite for the table mapping players to player groups."
     :tags (:gateway :sql :suite :positive :player :player-group
            :players-groups)))

(define-test players-groups
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
          (add-player-into-player-group pid1 gid1 t)
          (add-player-into-player-group pid2 gid1 nil)
          (add-player-into-player-group pid1 gid2 t)
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
          (update-player-group-owner t pid2 gid1)
          (is eq t (select-player-owner-of-group-p pid2 gid1))
          (update-player-group-owner nil pid2 gid1)
          (is eq nil (select-player-owner-of-group-p pid2 gid1))
          (remove-player-from-player-group pid1 gid1)
          (is eq :null (select-player-owner-of-group-p pid1 gid1))
          (remove-player-from-player-group pid3 gid1)
          (is eq :null (select-player-owner-of-group-p pid3 gid1))
          (remove-player-from-player-group pid1 gid2)
          (is eq :null (select-player-owner-of-group-p pid1 gid2))
          (true (table-empty-p "players_personas"))
          (mapc #'delete-player-by-id (list pid1 pid2 pid3))
          (mapc #'delete-player-group-by-id (list gid1 gid2)))))))
