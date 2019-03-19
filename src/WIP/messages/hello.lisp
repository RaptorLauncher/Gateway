;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; GATEWAY
;;;; © Michał "phoe" Herda 2016-2018
;;;; messages/hello.lisp

(uiop:define-package #:gateway.message
  (:mix #:list-named-class
        #:cl
        #:list-named-class))

(in-package #:gateway.message)

(define-protocol-class (:hello) () ())

(defclass (:request :hello) ((:request) (:hello))
  ((%client :accessor client
            :initarg :client)
   (%version :accessor version
             :initarg :version))
  (:default-initargs :client (error "Must provide CLIENT.")
                     :version (error "Must provide VERSION.") ;; TODO *version*
   ))

(defclass (:response :hello) ((:response) (:hello))
  ((%server :accessor server
            :initarg :server)
   (%version :accessor version
             :initarg :version)
   (%domain :accessor domain
            :initarg :domain))
  (:default-initargs
   :client (error "Must provide CLIENT.")
   :version (error "Must provide VERSION.") ;; TODO *version*
   :domain #+sbcl (sb-bsd-sockets:host-ent-name
                   (sb-bsd-sockets:get-host-by-name "localhost"))
           #-sbcl (machine-instance)))

(defclass (:error :hello) ((:error) (:hello)) ())
