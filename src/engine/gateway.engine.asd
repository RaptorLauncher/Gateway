;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; GATEWAY
;;;; © Michał "phoe" Herda 2016-2018
;;;; engine/gateway.engine.asd

(asdf:defsystem #:gateway.engine/protocol
  :description "Protocols for Gateway engine"
  :author "Michał \"phoe\" Herda <phoe@disroot.org>"
  :license  "AGPL3"
  :version "0.0.1"
  :serial t
  :depends-on (#:protest/protocol)
  :pathname "protocol/"
  :components ((:file "package")
               (:file "message")))

(asdf:defsystem #:gateway.engine/impl
  :description "Gateway game logic"
  :author "Michał \"phoe\" Herda <phoe@disroot.org>"
  :license  "AGPL3"
  :version "0.0.1"
  :serial t
  :depends-on (#:alexandria
               #:phoe-toolbox)
  :pathname "impl/"
  :components ((:file "package")
               (:file "standard-message")))

;; (asdf:defsystem #:gateway.engine
;;   :description
;;   "Part of Gateway responsible for routing data between users and game logic"
;;   :author "Michał \"phoe\" Herda <phoe@disroot.org>"
;;   :license  "AGPL3"
;;   :version "0.0.1"
;;   :serial t
;;   :depends-on (#:alexandria
;;                #:bordeaux-threads
;;                #:lparallel
;;                #:usocket
;;                #:verbose
;;                #:phoe-toolbox
;;                #:gateway.engine/protocol)
;;   :pathname "impl/"
;;   :components ((:file "package")
;;                (:file "utils")
;;                (:file "standard-connection")
;;                (:file "standard-acceptor")
;;                (:file "standard-listener")
;;                (:file "standard-writer")
;;                (:file "standard-engine")))

;; (asdf:defsystem #:gateway.engine/test
;;   :description "Tests for Gateway engine"
;;   :author "Michał \"phoe\" Herda <phoe@disroot.org>"
;;   :license  "AGPL3"
;;   :version "0.0.1"
;;   :serial t
;;   :depends-on (#:phoe-toolbox
;;                #:named-readtables
;;                #:protest/test
;;                #:protest/parachute
;;                #:gateway.engine/protocol
;;                #:gateway.engine)
;;   :pathname "t/"
;;   :components ((:file "package")
;;                (:file "standard-connection")
;;                (:file "standard-acceptor")
;;                (:file "standard-listener")
;;                (:file "standard-writer")
;;                (:file "standard-engine")))

;; (defmethod asdf:perform ((o asdf:test-op)
;;                          (c (eql (asdf:find-system ':gateway.engine))))
;;   (asdf:load-system :gateway.engine/test)
;;   (let ((*package* (find-package '#:gateway.engine/test)))
;;     (uiop:symbol-call :protest/parachute
;;                       :test (intern (symbol-name '#:engine)
;;                                     '#:gateway.engine/test)
;;                       :report (intern (symbol-name '#:interactive)
;;                                       '#:parachute))))
