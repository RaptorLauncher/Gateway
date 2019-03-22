;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; GATEWAY
;;;; © Michał "phoe" Herda 2017
;;;; gateway.config.asd

(asdf:defsystem #:gateway.config
  :description "Configuration system for Gateway"
  :author "Michał \"phoe\" Herda <phoe@openmailbox.org>"
  :license "GPLv3"
  :serial t
  :depends-on (#:alexandria
               #:phoe-toolbox
               #:ubiquitous-concurrent)
  :components ((:file "package")
               (:file "config")))
