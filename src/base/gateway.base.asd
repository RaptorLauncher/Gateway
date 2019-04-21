;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; GATEWAY
;;;; © Michał "phoe" Herda 2016-2018
;;;; base/gateway.base.asd

(asdf:defsystem #:gateway.base
  :description "Gateway basic definitions and functions"
  :author "Michał \"phoe\" Herda <phoe@disroot.org>"
  :license  "AGPL3"
  :version "0.0.1"
  :serial t
  :depends-on (#:gateway.base/protocol
               #:gateway.base/impl)
  :components ((:file "package")))

(asdf:defsystem #:gateway.base/protocol
  :description "Protocols for Gateway base"
  :author "Michał \"phoe\" Herda <phoe@disroot.org>"
  :license  "AGPL3"
  :version "0.0.1"
  :serial t
  :depends-on (#:protest/protocol
               #:protest/common/date)
  :pathname "protocol/"
  :components ((:file "package")
               (:file "mixin/identifiable")
               ;; (:file "mixin/with-creation-time")
               (:file "mixin/activatable")
               (:file "gateway-object")
               (:file "message")))

(asdf:defsystem #:gateway.base/impl
  :description "Implementations for Gateway base"
  :author "Michał \"phoe\" Herda <phoe@disroot.org>"
  :license  "AGPL3"
  :version "0.0.1"
  :serial t
  :depends-on (#:alexandria
               #:list-named-class
               #:list-named-class/protest
               #:destructuring-bind-star
               #:phoe-toolbox
               #:moptilities
               #:protest/base
               #:gateway.cable
               #:gateway.base/protocol)
  :pathname "impl/"
  :components ((:file "package")
               (:file "read-error")
               (:file "data-object")
               (:file "standard-message")))

(asdf:defsystem #:gateway.base/test
  :description "Tests for Gateway base"
  :author "Michał \"phoe\" Herda <phoe@disroot.org>"
  :license  "AGPL3"
  :version "0.0.1"
  :serial t
  :depends-on (#:phoe-toolbox
               #:list-named-class
               #:named-readtables
               #:moptilities
               #:protest/base
               #:protest/test-case
               #:protest/parachute
               #:gateway.cable
               #:gateway.base
               #:gateway.init)
  :pathname "t/"
  :components ((:file "package")
               (:file "data-object")))

(defmethod asdf:perform ((o asdf:test-op)
                         (c (eql (asdf:find-system '#:gateway.base))))
  (asdf:load-system '#:gateway.base/test)
  (let* ((*package* (find-package '#:gateway.base/test)))
    (uiop:symbol-call '#:protest/parachute
                      '#:test (intern (symbol-name '#:base)
                                      '#:gateway.base/test))))












;; (asdf:defsystem #:gateway.base/condition
;;   :description "Gateway game logic - conditions"
;;   :author "Michał \"phoe\" Herda <phoe@disroot.org>"
;;   :license  "AGPL3"
;;   :version "0.0.1"
;;   :serial t
;;   :depends-on (#:alexandria
;;                #:list-named-class
;;                #:moptilities
;;                #:destructuring-bind-star
;;                #:protest/base
;;                #:gateway.cable
;;                #:gateway.base/protocol)
;;   :pathname "condition/"
;;   :components ((:file "package")
;;                (:file "read-error")))

;; (asdf:defsystem #:gateway.base/message
;;   :description "Gateway game logic - messages"
;;   :author "Michał \"phoe\" Herda <phoe@disroot.org>"
;;   :license  "AGPL3"
;;   :version "0.0.1"
;;   :serial t
;;   :depends-on (#:alexandria
;;                #:phoe-toolbox
;;                #:list-named-class
;;                #:list-named-class/protest
;;                #:moptilities
;;                #:protest/base
;;                #:gateway.base/protocol)
;;   :pathname "message/"
;;   :components ((:file "package")
;;                (:file "standard-message")))

;; (asdf:defsystem #:gateway.base/objects
;;   :description "Gateway game logic - objects"
;;   :author "Michał \"phoe\" Herda <phoe@disroot.org>"
;;   :license  "AGPL3"
;;   :version "0.0.1"
;;   :serial t
;;   :depends-on (#:alexandria
;;                #:phoe-toolbox
;;                #:list-named-class
;;                #:cl-ppcre
;;                #:ironclad
;;                #:flexi-streams
;;                #:moptilities
;;                #:local-time
;;                #:protest/base
;;                #:protest/common/date
;;                #:gateway.base/protocol)
;;   :pathname "objects/"
;;   :components ((:file "package")
;;                (:file "standard-player")
;;                (:file "standard-date")))

;; (asdf:defsystem #:gateway.base/impl-old
;;   :description "Gateway game logic"
;;   :author "Michał \"phoe\" Herda <phoe@disroot.org>"
;;   :license  "AGPL3"
;;   :version "0.0.1"
;;   :serial t
;;   :depends-on (#:alexandria
;;                #:phoe-toolbox
;;                #:moptilities
;;                #:protest/base
;;                #:gateway.cable
;;                #:gateway.base/protocol
;;                #:gateway.base/condition)
;;   :pathname "impl/"
;;   :components ((:file "package")
;;                (:file "data-object")))
