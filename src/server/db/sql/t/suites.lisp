;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; GATEWAY
;;;; © Michał "phoe" Herda 2016-2019
;;;; sql/t/suites.lisp

(in-package #:gateway.sql/test)
(in-readtable protest/parachute)

;;; TODO add negative tests
;;; TODO add test case descriptions and protest readtable bindings
;;; TODO reader tests on dummy data
;;; TODO constraint and correctness tests

(define-test-case sql
    (:documentation "Test suite for Gateway SQL layer.
\
Each test in the test suite should use the WITH-SQL-TEST macro for ensuring that
the database is cleaned after each unhandled error in test body. The
WITH-SQL-TEST macro also connects to the test DB and ensures that, after each
test, all tables in the test database are empty or contain dummy data."
     :tags (:gateway :sql :suite)))

(define-test sql
  :parent (#:gateway.init #:gateway-full-test))

;;; SQL-SELECT-DUMMY

(define-test-case sql-select-dummy
    (:documentation
     "Test suite for SQL select functions, operating on dummy data.
\
Tests in this suite are forbidden from modifying the database state."
     :tags (:gateway :sql :suite :select-dummy)))

(define-test sql-select-dummy :parent sql)

(defmethod parachute:eval-in-context :around
    (context (test (eql (parachute:find-test 'sql-select-dummy))))
  (with-test-db () (install-dummy-data))
  (let ((*dummy-data* t))
    (call-next-method))
  (with-test-db () (uninstall) (install)))

;;; SQL-POSITIVE

(define-test-case sql-positive
    (:documentation "Test suite for positive SQL scenarios."
     :tags (:gateway :sql :suite :positive)))

(define-test sql-positive :parent sql)

;;; SQL-NEGATIVE

(define-test-case sql-negative
    (:documentation "Test suite for negative SQL scenarios."
     :tags (:gateway :sql :suite :negative)))

(define-test sql-negative :parent sql)
