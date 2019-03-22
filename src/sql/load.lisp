;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; GATEWAY
;;;; © Michał "phoe" Herda 2017
;;;; sql/load.lisp

(in-package #:gateway/sql)

;;; Install / Uninstall

(defvar *install-sql-base-path*
  (asdf:system-relative-pathname :gateway.sql "install/"))

(defun execute-file-with-transaction (filename)
  (let* ((pathname (merge-pathnames filename *install-sql-base-path*)))
    (postmodern:with-transaction ()
      (execute-file pathname))))

(defun uninstall ()
  (execute-file-with-transaction "uninstall.sql"))

(defun install ()
  (execute-file-with-transaction "install.sql")
  (execute-file-with-transaction "install-triggers.sql"))

(defun reinstall ()
  (uninstall)
  (install))

;;; CL-YESQL functions

(overlord:set-package-base "yesql/" :gateway.sql)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *sql-imports*
    '((player . "player.sql")
      (player-group . "player-group.sql"))))

(defun rebuild ()
  (let ((*package* (find-package :gateway/sql)))
    (loop for (name . sql) in *sql-imports*
          do (eval `(yesql:import ,name
                      :from ,sql
                      :as :cl-yesql/postmodern
                      :binding :all-as-functions
                      :export-bindings t)))
    (funcall #'overlord:build (mapcar #'car *sql-imports*))
    t))

(rebuild)
