;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; GATEWAY
;;;; © Michał "phoe" Herda 2016-2020
;;;; library/gateway.library.asd

(asdf:defsystem #:gateway.library
  :description "Library for Gateway"
  :author "Michał \"phoe\" Herda"
  :license "AGPL3"
  :depends-on (#:gateway.library/src))

(asdf:defsystem #:gateway.library/src
  :description "Library for Gateway - source code"
  :author "Michał \"phoe\" Herda"
  :license "AGPL3"
  :depends-on (#:pzmq
               #:legit
               #:ironclad
               #:gateway.cable)
  :serial t
  ;; :pathname "src/"
  :components ((:file "library")))
