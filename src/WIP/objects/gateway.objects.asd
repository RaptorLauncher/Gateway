;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; GATEWAY
;;;; © Michał "phoe" Herda 2016-2018
;;;; objects/gateway.objects.asd

(asdf:defsystem #:gateway.objects
  :description "Gateway base protocols and classes"
  :author "Michał \"phoe\" Herda <phoe@disroot.org>"
  :license  "AGPL3"
  :version "0.0.1"
  :serial t
  :depends-on (#:gateway.objects/protocol
               #:gateway.objects/impl)
  :components ((:file "package")))

(asdf:defsystem #:gateway.objects/protocol
  :description "Protocols for Gateway objects"
  :author "Michał \"phoe\" Herda <phoe@disroot.org>"
  :license  "AGPL3"
  :version "0.0.1"
  :serial t
  :depends-on (#:protest/protocol
               #:protest/common/date
               #:gateway.base)
  :pathname "protocol/"
  :components ((:file "package")
               (:file "player")))

(asdf:defsystem #:gateway.objects/impl
  :description "Concrete classes for Gateway objects"
  :author "Michał \"phoe\" Herda <phoe@disroot.org>"
  :license  "AGPL3"
  :version "0.0.1"
  :serial t
  :depends-on (#:alexandria
               ;; #:bordeaux-threads
               ;; #:lparallel
               ;; #:usocket
               ;; #:verbose ;; TODO logging
               #:local-time
               #:cl-ppcre
               #:ironclad
               #:flexi-streams
               #:phoe-toolbox
               #:gateway.objects/protocol)
  :pathname "impl/"
  :components ((:file "package")
               ;; (:file "standard-date")
               (:file "standard-player")))

(asdf:defsystem #:gateway.objects/test
  :description "Tests for Gateway objects"
  :author "Michał \"phoe\" Herda <phoe@disroot.org>"
  :license  "AGPL3"
  :version "0.0.1"
  :serial t
  :depends-on (#:phoe-toolbox
               #:named-readtables
               #:protest/test-case
               #:protest/parachute
               #:gateway.objects)
  :pathname "t/"
  :components ((:file "package")
               (:file "standard-player")))

(defmethod asdf:perform ((o asdf:test-op)
                         (c (eql (asdf:find-system '#:gateway.objects))))
  (asdf:load-system '#:gateway.objects/test)
  (let ((*package* (find-package '#:gateway.objects/test)))
    (uiop:symbol-call :protest/parachute
                      :test (intern (symbol-name '#:objects)
                                    '#:gateway.objects/test)
                      :report (intern (symbol-name '#:interactive)
                                      '#:parachute))))
