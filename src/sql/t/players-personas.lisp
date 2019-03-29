;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; GATEWAY
;;;; © Michał "phoe" Herda 2016-2019
;;;; sql/t/players-personas.lisp

(in-package #:gateway.sql/test)
(in-readtable protest/parachute)

(defparameter *players-personas-select-dummy-data*
  '((1 1 t)   (1 2 t)   (1 3 t) (1 4 nil)
    (2 4 t)   (2 5 t)   (2 6 t)
    (3 1 nil) (3 2 nil)
    (4 1 nil)))

(define-test-case players-personas-select-dummy
    (:documentation
     "Dummy select test suite for the table binding players to personas."
     :tags (:gateway :sql :suite :select-dummy :player :player-persona
            :players-personas)))

(define-test players-personas-select-dummy
  :parent sql-select-dummy)

(define-test-case players-personas-select-dummy-select-player-owner-of-persona-p
    (:documentation "Check if SELECT-PLAYER-OWNER-OF-PERSONA-P works."
     :tags (:gateway :sql :suite :select-dummy :player :player-persona
            :players-personas)))

(define-test players-personas-select-dummy-select-player-owner-of-persona-p
  :parent players-personas-select-dummy
  (with-sql-test ()
    (dolist (player (iota 8 :start 1))
      (dolist (persona (iota 8 :start 1))
        (flet ((ownerp (x) (and (eql (first x) player)
                                (eql (second x) persona))))
          (let* ((result (select-player-owner-of-persona-p player persona))
                 (match (find-if #'ownerp *players-personas-select-dummy-data*))
                 (match (if match (third match) :null)))
            (is eq result match)))))))

(define-test-case players-personas-select-players-of-persona
    (:documentation "Check if SELECT-PLAYERS-OF-PERSONA works."
     :tags (:gateway :sql :suite :select-dummy :player :player-persona
            :players-personas)))

(define-test players-personas-select-players-of-persona
  :parent players-personas-select-dummy
  (with-sql-test ()
    (dolist (persona (iota 3 :start 1))
      (let* ((result (select-players-of-persona persona))
             (result (mapcar (lambda (x) (list (first x) (tenth x))) result)))
        (dolist (cell result)
          (destructuring-bind (player ownerp) cell
            (flet ((find-data (x) (and (eql (first x) player)
                                       (eql (second x) persona)
                                       (eql (third x) ownerp))))
              (true (find-if #'find-data
                             *players-personas-select-dummy-data*)))))))))

(define-test-case players-personas-select-personas-of-player
    (:documentation "Check if SELECT-PERSONAS-OF-PLAYER works."
     :tags (:gateway :sql :suite :select-dummy :player :player-persona
            :players-personas)))

(define-test players-personas-select-personas-of-player
  :parent players-personas-select-dummy
  (with-sql-test ()
    (dolist (player (iota 8 :start 1))
      (let* ((result (select-personas-of-player player))
             (result (mapcar (lambda (x) (list (first x) (sixth x))) result)))
        (dolist (cell result)
          (destructuring-bind (persona ownerp) cell
            (flet ((find-data (x) (and (eql (first x) player)
                                       (eql (second x) persona)
                                       (eql (third x) ownerp))))
              (true (find-if #'find-data
                             *players-personas-select-dummy-data*)))))))))

(define-test-case players-personas-positive
    (:documentation
     "Positive test suite for the table mapping players to player personas."
     :tags (:gateway :sql :suite :positive :player :player-persona
            :players-personas)))

(define-test players-personas-positive
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

(define-test-case personas-negative
    (:documentation
     "Negative test suite for the table mapping players to player personas."
     :tags (:gateway :sql :suite :negative :player :player-persona
            :players-personas))
  :assert
  1  "It is impossible to add a non-existent player to a persona."
  2  "It is impossible to add a player to a non-existent persona."
  3  "Trying to check if a nonexistent player is an owner of a persona should ~
return null."
  4  "Trying to check if a player is an owner of a nonexistent persona should ~
return null."
  5  "Selecting owners of a nonexistent persona should return no rows."
  6  "Selecting personas a nonexistent player belongs go should return no rows."
  7  "Updating ownership status of a nonexistent persona should affect no rows."
  8  "Updating ownership status of a nonexistent player should affect no rows."
  9  "Removing owners of a nonexistent persona should affect no rows."
  10 "Removing nonexistent personas from a player should affect no rows.")

(define-test personas-negative
  :parent sql-negative
  (with-sql-test ()
    (flet ((ub8 (n) (make-array n :element-type '(unsigned-byte 8))))
      (let ((persona (insert-persona :name "Player Group" :description "")))
        #1?(db-fail (add-persona-to-player 0 persona t))

        #3?(is eq :null (select-player-owner-of-persona-p 0 persona))
        #7?(is = 0 (nth-value 1 (update-persona-owner t 0 persona)))
        #9?(is = 0 (nth-value 1 (remove-persona-from-player 0 persona)))
        (delete-persona-by-id persona))
      (let ((player (insert-player :login "gateway01" :email "ga1@te.way"
                                   :name "Player" :hash (ub8 0) :salt (ub8 0)
                                   :activatedp nil)))
        #2?(db-fail (add-persona-to-player player 0 t))
        #4?(is eq :null (select-player-owner-of-persona-p player 0))
        #8?(is = 0 (nth-value 1 (update-persona-owner t player 0)))
        #10?(is = 0 (nth-value 1 (remove-persona-from-player player 0)))
        (delete-player-by-id player)))
    #5?(is = 0 (nth-value 1 (select-players-of-persona 0)))
    #6?(is = 0 (nth-value 1 (select-personas-of-player 0)))))
