;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; GATEWAY
;;;; © Michał "phoe" Herda 2016-2020
;;;; client/gateway.client.asd

(asdf:defsystem #:gateway.client
  :description "Client for Gateway"
  :author "Michał \"phoe\" Herda"
  :license "AGPL3"
  :depends-on (#:gateway.client/src))

(asdf:defsystem #:gateway.client/src
  :description "Client for Gateway - source code"
  :author "Michał \"phoe\" Herda"
  :license "AGPL3"
  :depends-on (#:phoe-toolbox
               #:ironclad
               #:pzmq
               #:gateway.library
               #:gateway.cable)
  :serial t
  ;; :pathname "src/"
  :components ((:file "client")))
