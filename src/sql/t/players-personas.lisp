;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; GATEWAY
;;;; © Michał "phoe" Herda 2016-2019
;;;; sql/t/players-personas.lisp

(in-package #:gateway.sql/test)
(in-readtable protest/parachute)

(define-test-case players-personas
    (:documentation
     "Positive test suite for the table mapping players to player personas."
     :tags (:gateway :sql :suite :positive :player :player-persona
            :players-personas)))

(define-test players-personas
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
           (persona-1 '(:name "Test Persona 01"))
           (persona-2 '(:name "Test Persona 02")))
      (destructuring-bind (pid1 pid2 pid3)
          (mapcar (curry #'apply #'insert-player
                         :hash "" :salt "" :activatedp t)
                  (list player-1 player-2 player-3))
        (destructuring-bind (perid1 perid2)
            (mapcar (curry #'apply #'insert-persona :description "")
                    (list persona-1 persona-2))
          (add-persona-to-player pid1 perid1 t)
          (add-persona-to-player pid2 perid1 nil)
          (add-persona-to-player pid1 perid2 t)
          (is eq t (select-player-owner-of-persona-p pid1 perid1))
          (is eq nil (select-player-owner-of-persona-p pid2 perid1))
          (is eq :null (select-player-owner-of-persona-p pid3 perid1))
          (is eq t (select-player-owner-of-persona-p pid1 perid2))
          (is eq :null (select-player-owner-of-persona-p pid2 perid2))
          (is eq :null (select-player-owner-of-persona-p pid3 perid2))
          (flet ((verify (perid expected)
                   (let* ((fn (lambda (x) (list (first x) (tenth x))))
                          (actual (select-players-of-persona perid))
                          (actual (mapcar fn actual)))
                     (is (lambda (x y) (set-equal x y :test #'equal))
                         expected actual))))
            (verify perid1 `((,pid1 t) (,pid2 nil)))
            (verify perid2 `((,pid1 t))))
          (flet ((verify (pid expected)
                   (let* ((fn (lambda (x) (list (first x) (sixth x))))
                          (actual (select-personas-of-player pid))
                          (actual (mapcar fn actual)))
                     (is (lambda (x y) (set-equal x y :test #'equal))
                         expected actual))))
            (verify pid1 `((,perid1 t) (,perid2 t)))
            (verify pid2 `((,perid1 nil)))
            (verify pid3 '()))
          (fail (update-persona-owner t pid2 perid1))
          (update-persona-owner nil pid1 perid1)
          (is eq nil (select-player-owner-of-persona-p pid1 perid1))
          (update-persona-owner t pid2 perid1)
          (is eq t (select-player-owner-of-persona-p pid2 perid1))
          (remove-persona-from-player pid1 perid1)
          (is eq :null (select-player-owner-of-persona-p pid1 perid1))
          (remove-persona-from-player pid1 perid2)
          (is eq :null (select-player-owner-of-persona-p pid3 perid1))
          (remove-persona-from-player pid2 perid1)
          (is eq :null (select-player-owner-of-persona-p pid1 perid2))
          (true (table-empty-p "players_personas"))
          (mapc #'delete-player-by-id (list pid1 pid2 pid3))
          (mapc #'delete-persona-by-id (list perid1 perid2)))))))
