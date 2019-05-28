;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; GATEWAY
;;;; © Michał "phoe" Herda 2016-2019
;;;; sql/gateway.sql.asd

(asdf:defsystem #:gateway.sql
  :description "SQL layer for Gateway"
  :author "Michał \"phoe\" Herda"
  :license "AGPL3"
  :depends-on (#:gateway.sql/src))

(asdf:defsystem #:gateway.sql/src
  :description "SQL layer for Gateway - source code"
  :author "Michał \"phoe\" Herda"
  :license "AGPL3"
  :depends-on (#:alexandria
               #:cl-yesql
               #:phoe-toolbox
               #:cl+ssl
               #:cl-yesql/postmodern
               #:gateway.config
               #:cl-postgres+local-time)
  :serial t
  :pathname "src/" ;; TODO implement everywhere
  :components ((:file "package")
               (:file "connections")
               (:file "load")))

(asdf:defsystem #:gateway.sql/test
  :description "SQL layer for Gateway - tests"
  :author "Michał \"phoe\" Herda"
  :license "AGPL3"
  :depends-on (#:gateway.sql
               #:gateway.init
               #:alexandria
               #:serapeum
               #:phoe-toolbox
               #:local-time
               #:ironclad
               #:serapeum
               #:named-readtables
               #:protest/test-case
               #:protest/parachute)
  :serial t
  :pathname "t/" ;; TODO implement everywhere
  :components ((:file "package")
               (:file "framework")
               (:file "suites")
               (:file "system/player")
               (:file "system/player-group")
               (:file "system/players-groups")
               (:file "system/persona")
               (:file "system/players-personas")
               (:file "system/timeline")))

(defmethod asdf:perform ((o asdf:test-op)
                         (c (eql (asdf:find-system '#:gateway.sql))))
  (asdf:operate 'asdf:load-op '#:gateway.sql/test)
  (uiop:symbol-call '#:gateway.sql/test '#:test))
