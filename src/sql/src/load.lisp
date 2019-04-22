;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; GATEWAY
;;;; © Michał "phoe" Herda 2017
;;;; sql/src/load.lisp

(in-package #:gateway.sql)

;;; Install / Uninstall

(defparameter *install-sql-base-path*
  (asdf:system-relative-pathname :gateway.sql "src/install/"))

(defun execute-file-with-transaction (filename)
  (let* ((pathname (merge-pathnames filename *install-sql-base-path*)))
    (postmodern:with-transaction ()
      (execute-file pathname))))

(defun uninstall ()
  (execute-file-with-transaction "uninstall.sql"))

(defun install ()
  (execute-file-with-transaction "install-utils.sql")
  (execute-file-with-transaction "install-schema.sql")
  (execute-file-with-transaction "install-errors.sql"))

(defun install-dummy-data ()
  (execute-file-with-transaction "install-dummy-data.sql"))

;;; CL-YESQL functions

;; TODO fix this for non-system SQL functions in the future
(let ((*package* (find-package '#:gateway.sql/system)))
  (overlord:set-package-base "src/yesql/" '#:gateway.sql))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *sql-imports*
    '(system/player
      system/player-group
      system/players-groups
      system/persona
      system/players-personas
      system/timeline)))

(defun rebuild ()
  (loop with *package* = (find-package '#:gateway.sql/system)
        for name in *sql-imports*
        for path = (string-downcase (symbol-name name))
        for sql = (uiop:strcat path ".sql")
        do (eval `(yesql:import ,name
                    :from ,sql
                    :as :cl-yesql/postmodern
                    :binding :all-as-functions
                    :export-bindings t)))
  (funcall (rcurry #'overlord:build) *sql-imports*)
  t)

(rebuild)
