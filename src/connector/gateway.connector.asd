;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; GATEWAY
;;;; © Michał "phoe" Herda 2016-2018
;;;; connector/gateway.connector.asd

(asdf:defsystem #:gateway.connector/protocol
  :description "Protocols for Gateway connector"
  :author "Michał \"phoe\" Herda <phoe@disroot.org>"
  :license  "AGPL3"
  :version "0.0.1"
  :serial t
  :depends-on (#:gateway.cable
               #:protest/common/addressed
               #:protest/common/handling
               #:protest/common/killable
               #:protest/common/named
               #:protest/common/serializable)
  :pathname "protocol/"
  :components ((:file "package")
               (:file "connection")
               (:file "acceptor")
               ;; (:file "listener")
               ;; (:file "writer")
               ))

(asdf:defsystem #:gateway.connector
  :description
  "Part of Gateway responsible for routing data between users and game logic"
  :author "Michał \"phoe\" Herda <phoe@disroot.org>"
  :license  "AGPL3"
  :version "0.0.1"
  :serial t
  :depends-on (#:alexandria
               #:bordeaux-threads
               #:usocket
               #:verbose
               #:phoe-toolbox
               #:gateway.connector/protocol)
  :pathname "impl/"
  :components ((:file "package")
               (:file "utils")
               (:file "standard-connection")
               (:file "standard-acceptor")))

(asdf:defsystem #:gateway.connector/test
  :description "Tests for Gateway connector"
  :author "Michał \"phoe\" Herda <phoe@disroot.org>"
  :license  "AGPL3"
  :version "0.0.1"
  :serial t
  :depends-on (#:phoe-toolbox
               #:named-readtables
               #:protest/test
               #:protest/parachute
               #:gateway.connector/protocol
               #:gateway.connector)
  :pathname "t/"
  :components ((:file "package")
               (:file "standard-connection")))

(defmethod asdf:perform ((o asdf:test-op)
                         (c (eql (asdf:find-system ':gateway.connector))))
  (asdf:load-system :gateway.connector/test)
  (let ((*package* (find-package '#:gateway.connector/test)))
    (uiop:symbol-call :protest/parachute
                      :test (intern (symbol-name '#:connector)
                                    '#:gateway.connector/test)
                      :report (intern (symbol-name '#:interactive)
                                      '#:parachute))))
