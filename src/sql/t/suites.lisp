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
the database is reinstalled after each unhandled error in test body. The
WITH-SQL-TEST macro also connects to the test DB and ensures that, after each
test, all tables in the test database are empty."
     :tags (:gateway :sql :suite)))

(define-test sql)

(define-test-case sql-positive
    (:documentation "Test suite for positive SQL scenarios."
     :tags (:gateway :sql :suite :positive)))

(define-test sql-positive :parent sql)

(define-test-case sql-negative
    (:documentation "Test suite for negative SQL scenarios."
     :tags (:gateway :sql :suite :negative)))

(define-test sql-negative :parent sql)
