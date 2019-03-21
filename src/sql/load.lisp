;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Gateway
;;;; © Michał "phoe" Herda 2017
;;;; sql/load.lisp

;;; TODO unify ;;;; GATEWAY and ;;;; Gateway

(in-package #:gateway/sql)

(overlord:set-package-base "yesql/" :gateway.sql)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *sql-imports*
    '(;; (install . "install.sql")
      ;; (player . "player.sql")
      )))

(defun import-all ()
  (let ((*package* (find-package :gateway/sql)))
    (loop for (name . sql) in *sql-imports*
          with *package* = (find-package :gateway/sql)
          do (eval `(yesql:import ,name
                      :from ,sql
                      :as :cl-yesql/postmodern
                      :binding :all-as-functions
                      :export-bindings t)))
    (apply #'overlord:build (mapcar #'car *sql-imports*))
    t))

(import-all)

(defvar *sql-base-path*
  (asdf:system-relative-pathname :gateway.sql "yesql/"))

(defun execute-file-with-transaction (filename)
  (let* ((pathname (merge-pathnames filename *sql-base-path*)))
    (postmodern:with-transaction ()
      (execute-file pathname))))

(defun uninstall ()
  (execute-file-with-transaction "uninstall.sql"))

(defun install ()
  (execute-file-with-transaction "install.sql"))
