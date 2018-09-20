;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; GATEWAY-CONNECTOR
;;;; © Michał "phoe" Herda 2016-2018
;;;; gateway.connector.asd

(asdf:defsystem #:gateway.connector
  :description
  "Part of Gateway server - routes data between user connections and game logic"
  :author "Michał \"phoe\" Herda <phoe@disroot.org>"
  :license  "AGPL3"
  :version "0.0.1"
  :serial t
  :components ((:file "package")
               (:file "gateway.connector")))
