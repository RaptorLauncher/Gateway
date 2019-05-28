;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; GATEWAY
;;;; © Michał "phoe" Herda 2016-2018
;;;; gateway.client.ui.asd

(asdf:defsystem #:gateway.client.ui
  :description "Gateway UI"
  :author "Michał \"phoe\" Herda <phoe@disroot.org>"
  :license  "AGPL3"
  :version "0.0.1"
  :serial t
  :depends-on (#:qtgui
               #:qtools
               #:phoe-toolbox
               #:qtools-ui-imagetools)
  :components ((:file "package")
               (:file "definition")
               (:file "image-widget")
               ;; (:file "chat")
               ))
