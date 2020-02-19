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
               #:split-sequence
               #:local-time
               #:lorem-ipsum
               #:qtools-ui-imagetools
               #:qtools-ui-fixed-qtextedit
               #:qtools-ui-spellchecked-text-edit
               #:qtools-ui-placeholder-text-edit
               #:qtools-ui-dictionary)
  :defsystem-depends-on (:qtools)
  :build-operation "qt-program-op"
  :build-pathname "gateway"
  :entry-point "gateway.client.ui:main"
  :components ((:file "package")
               (:file "utils")
               (:file "classes") ;; TODO move out of UI
               (:file "image-widget")
               (:file "image-widget-holder")
               (:file "log-parser")
               (:file "example") ;; TODO move out of UI
               (:file "chat")
               (:file "main")
               ))
