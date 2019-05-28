;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; GATEWAY
;;;; © Michał "phoe" Herda 2016-2018
;;;; gateway.ui.asd

(asdf:defsystem #:gateway.ui
  :description "Gateway UI"
  :author "Michał \"phoe\" Herda <phoe@disroot.org>"
  :license  "AGPL3"
  :version "0.0.1"
  :serial t
  :depends-on (#:qtgui
               #:qtools
               #:qtools-ui-fixed-qtextedit
               #:spell/simple
               #:wordnet
               #:split-sequence
               #:phoe-toolbox
               #:trivial-indent
               #:qtools-commons)
  :components ((:file "package")
               (:file "definition")
               (:file "spellchecked-text-edit")
               (:file "dictionary")
               ;; (:file "image-widget")
               ;; (:file "chat")
               ))
