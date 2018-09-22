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
  :depends-on (#:protest/common/addressed
               #:protest/common/handling
               #:protest/common/killable
               #:protest/common/named
               #:protest/common/serializable)
  :pathname "protocol/"
  :components ((:file "package")
               (:file "connection")
               ;; (:file "acceptor")
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
  :depends-on (#:usocket
               #:verbose
               #:phoe-toolbox
               #:gateway.connector/protocol)
  :pathname "impl/"
  :components ((:file "package")
               (:file "standard-socket")
               ;; (:file "standard-connection")
               ))