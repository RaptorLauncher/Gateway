;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; GATEWAY
;;;; © Michał "phoe" Herda 2016-2018
;;;; engine/gateway.engine.asd

(asdf:defsystem #:gateway.engine
  :description
  "Part of Gateway responsible for routing data between users and game logic"
  :author "Michał \"phoe\" Herda <phoe@disroot.org>"
  :license  "AGPL3"
  :version "0.0.1"
  :serial t
  :depends-on (#:gateway.engine/protocol
               #:gateway.engine/condition
               #:gateway.engine/message
               #:gateway.engine/impl)
  :components ((:file "package")))

(asdf:defsystem #:gateway.engine/protocol
  :description "Protocols for Gateway engine"
  :author "Michał \"phoe\" Herda <phoe@disroot.org>"
  :license  "AGPL3"
  :version "0.0.1"
  :serial t
  :depends-on (#:protest/protocol)
  :pathname "protocol/"
  :components ((:file "package")
               (:file "gateway-object")
               (:file "message")))

(asdf:defsystem #:gateway.engine/condition
  :description "Gateway game logic - conditions"
  :author "Michał \"phoe\" Herda <phoe@disroot.org>"
  :license  "AGPL3"
  :version "0.0.1"
  :serial t
  :depends-on (#:alexandria
               #:list-named-class
               #:moptilities
               #:destructuring-bind-star
               #:protest/base
               #:gateway.cable
               #:gateway.engine/protocol)
  :pathname "condition/"
  :components ((:file "package")
               (:file "read-error")))

(asdf:defsystem #:gateway.engine/message
  :description "Gateway game logic - messages"
  :author "Michał \"phoe\" Herda <phoe@disroot.org>"
  :license  "AGPL3"
  :version "0.0.1"
  :serial t
  :depends-on (#:alexandria
               #:phoe-toolbox
               #:list-named-class
               #:moptilities
               #:protest/base
               #:gateway.engine/protocol)
  :pathname "message/"
  :components ((:file "package")
               (:file "standard-message")))

(asdf:defsystem #:gateway.engine/impl
  :description "Gateway game logic"
  :author "Michał \"phoe\" Herda <phoe@disroot.org>"
  :license  "AGPL3"
  :version "0.0.1"
  :serial t
  :depends-on (#:alexandria
               #:phoe-toolbox
               #:moptilities
               #:protest/base
               #:gateway.cable
               #:gateway.engine/protocol
               #:gateway.engine/condition)
  :pathname "impl/"
  :components ((:file "package")
               (:file "data-object")))

(asdf:defsystem #:gateway.engine/test
  :description "Tests for Gateway engine"
  :author "Michał \"phoe\" Herda <phoe@disroot.org>"
  :license  "AGPL3"
  :version "0.0.1"
  :serial t
  :depends-on (#:phoe-toolbox
               #:list-named-class
               #:named-readtables
               #:moptilities
               #:protest/base
               #:protest/test
               #:protest/parachute
               #:gateway.cable
               #:gateway.engine)
  :pathname "t/"
  :components ((:file "package")
               (:file "data-object")))

(defmethod asdf:perform ((o asdf:test-op)
                         (c (eql (asdf:find-system '#:gateway.engine))))
  (asdf:load-system '#:gateway.engine/test)
  (let ((*package* (find-package '#:gateway.engine/test)))
    (uiop:symbol-call :protest/parachute
                      :test (intern (symbol-name '#:engine)
                                    '#:gateway.engine/test)
                      :report (intern (symbol-name '#:interactive)
                                      '#:parachute))))
