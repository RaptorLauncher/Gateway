;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; GATEWAY
;;;; © Michał "phoe" Herda 2017
;;;; config.lisp

(in-package #:gateway/config)

(restore 'gateway)

(defun config (&rest path)
  "Fetch the configuration value for the given path. Returns two values. The
first is the retrieved value, or NIL if it was not found; the second is true if
the value was found in the configuration."
  (apply #'value 'gateway path))

(defun default-config (default &rest path)
  "Fetch the configuration value for the given path and return it. If the value
is not found, DEFAULT is set as the value and returned."
  (apply #'defaulted-value default 'gateway path))

(defun econfig (&rest path)
  "Returns the configuration value for the given path. Signals an error if the
value was not found."
  (multiple-value-bind (value foundp) (apply #'config path)
    (if foundp
        value
        (error "Failed to find a configuration value for path ~S." path))))

(defun (setf config) (new-value &rest path)
  "Set the configuration value of the given path."
  (apply #'(setf value) new-value 'gateway path))

(defun remconfig (&rest path)
  "Removes the configuration value of the given path."
  (apply #'remvalue 'gateway path))

(default-config "phoe" :db-user)
(default-config "gateway" :db-pass)
(default-config "localhost" :db-host)
(default-config 5432 :db-port)
(default-config "gateway" :db-name)
(default-config :no :db-use-ssl)

(default-config "phoe" :test-db-user)
(default-config "gateway" :test-db-pass)
(default-config "localhost" :test-db-host)
(default-config 5432 :test-db-port)
(default-config "gateway_test" :test-db-name)
(default-config :no :test-db-use-ssl)
