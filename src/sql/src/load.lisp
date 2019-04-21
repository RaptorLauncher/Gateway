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
  (execute-file-with-transaction "install.sql")
  (execute-file-with-transaction "install-errors.sql"))

(defun install-dummy-data ()
  (execute-file-with-transaction "dummy-data.sql"))

;;; CL-YESQL functions

(overlord:set-package-base "src/yesql/" :gateway.sql)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *sql-imports*
    '(system/player
      system/player-group
      system/players-groups
      system/persona
      system/players-personas)))

(defun rebuild ()
  (let ((*package* (find-package :gateway.sql)))
    (loop for name in *sql-imports*
          for path = (string-downcase (symbol-name name))
          for sql = (uiop:strcat path ".sql")
          do (eval `(yesql:import ,name
                      :from ,sql
                      :as :cl-yesql/postmodern
                      :binding :all-as-functions
                      :export-bindings t)))
    (funcall (rcurry #'overlord:build) *sql-imports*)
    t))

(rebuild)

'(activate-player-by-id
  activated-player-p-by-id
  add-persona-to-player
  add-player-into-player-group
  deactivate-player-by-id
  delete-persona-by-id
  delete-player-by-id
  delete-player-group-by-id
  insert-persona
  insert-player
  insert-player-group
  install
  player-owner-of-group-p
  remove-persona-from-player
  remove-player-from-player-group
  select-groups-a-player-belongs-to
  select-groups-player-belongs-to
  select-persona-by-id
  select-personas-by-name
  select-personas-of-player
  select-player-by-display-name
  select-player-by-email
  select-player-by-id
  select-player-by-login
  select-player-group-by-id
  select-player-groups-by-name
  select-player-owner-of-group-p
  select-player-owner-of-persona-p
  select-players-belonging-to-group
  select-players-by-display-name
  select-players-by-name
  select-players-of-persona
  uninstall
  update-persona-description-by-id
  update-persona-name-by-id
  update-persona-owner
  update-player-activatedp-by-id
  update-player-display-name-by-id
  update-player-email-by-id
  update-player-group-description-by-id
  update-player-group-name-by-id
  update-player-group-owner
  update-player-login-by-id
  update-player-name-by-id
  update-player-password-by-id
  with-db
  with-test-db)
