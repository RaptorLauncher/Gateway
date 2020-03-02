;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; GATEWAY
;;;; © Michał "phoe" Herda 2016-2020
;;;; server/gateway.server.asd

(asdf:defsystem #:gateway.server
  :description "Server for Gateway"
  :author "Michał \"phoe\" Herda"
  :license "AGPL3"
  :depends-on (#:gateway.server/src))

(asdf:defsystem #:gateway.server/src
  :description "Server for Gateway - source code"
  :author "Michał \"phoe\" Herda"
  :license "AGPL3"
  :depends-on (#:alexandria
               #:phoe-toolbox
               #:ironclad
               #:bordeaux-threads
               #:pzmq
               #:local-time
               #:cffi
               #:gateway.library)
  :serial t
  ;; :pathname "src/"
  :components ((:file "server")))
