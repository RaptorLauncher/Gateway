;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; GATEWAY
;;;; © Michał "phoe" Herda 2016-2018
;;;; cable/gateway.cable.asd

(asdf:defsystem #:gateway.cable
  :description "Cable network protocol for Gateway"
  :author "Michał \"phoe\" Herda <phoe@disroot.org>"
  :license  "Artistic"
  :version "0.1"
  :serial t
  :depends-on (#:trivial-garbage
               #:bordeaux-threads
               #:phoe-toolbox
               #:cl-plumbing)
  :components ((:file "package")
               (:file "buffer")
               (:file "equal")
               (:file "output")
               (:file "input")))

(asdf:defsystem #:gateway.cable/test
  :description "Test for Gateway cable network protocol"
  :author "Michał \"phoe\" Herda <phoe@disroot.org>"
  :license  "Artistic"
  :version "0.1"
  :serial t
  :depends-on (#:gateway.cable
               #:gateway.init
               #:protest/test-case
               #:protest/parachute)
  :components ((:file "test")))

(defmethod asdf:perform ((o asdf:test-op)
                         (c (eql (asdf:find-system '#:gateway.cable))))
  (asdf:operate 'asdf:load-op '#:gateway.cable/test)
  (uiop:symbol-call '#:parachute '#:test '#:gateway.cable/test))
