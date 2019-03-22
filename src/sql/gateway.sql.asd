;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; GATEWAY
;;;; © Michał "phoe" Herda 2016-2019
;;;; sql/gateway.sql.asd

(asdf:defsystem #:gateway.sql
  :description "SQL layer for Gateway"
  :author "Michał \"phoe\" Herda"
  :license "AGPL3"
  :depends-on (#:alexandria
               #:cl-yesql
               #:phoe-toolbox
               #:cl+ssl
               #:cl-yesql/postmodern
               #:postmodern-execute-file
               ;; #:gateway.utils
               ;; #:gateway.protocol
               #:gateway.config
               #:cl-postgres+local-time)
  :serial t
  :components ((:file "package")
               (:file "connections")
               (:file "load")
               ))
