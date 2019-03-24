;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; GATEWAY
;;;; © Michał "phoe" Herda 2016-2019
;;;; sql/t/tests.lisp

(in-package #:gateway.sql/test)
(in-readtable protest/parachute)

;; TODO factor into multiple files

(defvar *checked-exports*)
(defvar *exports*)

(defparameter *warn-on-untested-symbols* t)

(defun compute-exports ()
  (let ((exports (loop with package = (find-package '#:gateway.sql)
                       for x being the external-symbols of package
                       collect x))
        (ignored '(install uninstall reinstall with-db with-test-db)))
    (set-difference exports ignored)))

;;; FIXME: Most of this hack should go away when Shinmera implements running
;;; code around tests in a test suite.

(defmacro define-test (name &body arguments-and-body)
  `(protest/parachute:define-test ,name
     ,@arguments-and-body
     (flet ((walk (x)
              (when (and (symbolp x)
                         (boundp '*checked-exports*)
                         (boundp '*exports*)
                         (member x *exports*))
                (pushnew x *checked-exports*))))
       (serapeum:walk-tree #'walk ',arguments-and-body))))

(defun test (&rest args)
  (let ((*checked-exports* '())
        (*exports* (compute-exports)))
    (apply #'protest/parachute:test args)
    (when *warn-on-untested-symbols*
      (when-let ((diff (set-difference *exports* *checked-exports*)))
        (warn "~D/~D symbols untested.~{~%~S~}"
              (length diff) (length *exports*) diff)))))

;;; TODO reader tests on dummy data
;;; TODO constraint and correctness tests

(defvar *test-tables-empty-query*
  "SELECT table_name,
          (xpath('/row/count/text()',
           query_to_xml('select count(*) from '||format('%I.%I', table_schema, table_name),
                        true, true, '')))[1]::text::int AS row_count
    FROM information_schema.tables
    WHERE table_schema = 'public';")

(defun test-tables-empty ()
  (loop with result = (pomo:query *test-tables-empty-query*)
        for (name count) in result
        unless (= 0 count)
          do (error "Cleanup failure: table ~S has ~D entries after the test.
~S" name count (pomo:query (uiop:strcat "SELECT * FROM " name)))))

(defun table-empty-p (name)
  (= 0 (pomo:query (uiop:strcat "SELECT COUNT(1) FROM " name) :single)))

(defmacro with-sql-test (() &body body)
  (with-gensyms (errorp e)
    `(let ((,errorp nil))
       (handler-bind ((error (lambda (,e) (setf ,errorp ,e))))
         (unwind-protect
              (with-test-db ()
                ,@body
                (test-tables-empty))
           (when ,errorp (with-test-db () (reinstall))))))))

;;; Positive test cases

(define-test-case sql-positive
    (:documentation "Test suite for positive SQL scenarios."
     :tags (:gateway :sql :suite :positive)))

(define-test sql-positive
  :parent sql)

;; TODO add negative tests
;; TODO add test case descriptions and protest readtable bindings

;;; Player tests

(define-test-case player
    (:documentation "Positive test suite for the player table."
     :tags (:gateway :sql :suite :positive :player)))

(define-test player
  :parent sql-positive
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

(define-test-case persona
    (:documentation "Positive test suite for the persona table."
     :tags (:gateway :sql :suite :positive :persona)))

(define-test persona
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

;;; Negative test cases

(define-test-case sql-negative
    (:documentation "Test suite for negative SQL scenarios."
     :tags (:gateway :sql :suite :negative)))

(define-test sql-negative
  :parent sql)
