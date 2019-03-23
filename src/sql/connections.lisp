;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; GATEWAY
;;;; © Michał "phoe" Herda 2017
;;;; sql/connections.lisp

(in-package #:gateway/sql)

(defvar *sql-readtable* (cl-postgres:copy-sql-readtable))

(local-time:set-local-time-cl-postgres-readers *sql-readtable*)

(defmacro %with-db
    ((name-var user-var pass-var host-var port-var ssl-var) &body body)
  (with-gensyms (name user pass host port ssl)
    `(let* ((,name (config ,name-var)) (,user (config ,user-var))
            (,pass (config ,pass-var)) (,host (config ,host-var))
            (,port (config ,port-var)) (,ssl (config ,ssl-var))
            (cl-postgres:*sql-readtable* *sql-readtable*))
       (postmodern:with-connection (list ,name ,user ,pass ,host
                                         :port ,port :use-ssl ,ssl :pooled-p t)
         ,@body))))

(defmacro with-db (() &body body)
  "Evaluates forms with the database connection bound to the Gateway database."
  `(%with-db (:db-name :db-user :db-pass :db-host :db-port :db-use-ssl)
     ,@body))

(defmacro with-test-db (() &body body)
  "Evaluates forms with the database connection bound to the Gateway test
database."
  `(%with-db (:test-db-name :test-db-user :test-db-pass :test-db-host
              :test-db-port :test-db-use-ssl)
     ,@body))
