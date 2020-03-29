;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; GATEWAY
;;;; © Michał "phoe" Herda 2016-2020
;;;; library/library.lisp

(defpackage #:gateway.library
  (:use #:cl)
  (:local-nicknames (#:a #:alexandria)
                    (#:z #:pzmq)
                    (#:i #:ironclad)
                    (#:g #:legit))
  (:export
   ;; Variables
   #:*context*
   ;; Classes
   #:zmq-socketed
   #:socket
   #:encrypting
   #:private-key
   #:public-key
   #:cipher
   #:kill ;; TODO protest/common:kill
   ;; Functions
   #:kill-socket
   #:kill-context
   #:set-socket-options
   #:gateway-version))

(in-package #:gateway.library)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Variables

(defvar *context* (pzmq:ctx-new)
  "Gateway ZMQ context.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Functions

;; TODO remove
(defmacro kill-socket (place)
  `(when ,place
     (z:close ,place)
     (setf ,place nil)))

(defun set-socket-options (socket &rest args)
  (loop for (key value) on args by #'cddr
        do (z:setsockopt socket key value)))

(defun gateway-version (&optional (system :gateway.library))
  (or (ignore-errors
       (format nil "git-~A" (g:current-commit
                             (asdf:system-source-directory system) :short t)))
      (ignore-errors (asdf:system-version system))
      "unknown"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Classes

(defclass zmq-socketed () ;; TODO define-protocol-class
  ((%socket :accessor socket)))

(defmethod initialize-instance :after
    ((object zmq-socketed) &key socket-type socket-options)
  (unless socket-type (a:required-argument :socket-type))
  (setf (socket object) (z:socket *context* socket-type))
  (when socket-options
    (apply #'set-socket-options (socket object) socket-options)))

(defmethod kill ((object zmq-socketed)) ;; TODO protest/common:killable
  (z:close (socket object)))

(defclass encrypting () ;; TODO define-protocol-class
  ((%private-key :accessor private-key :initarg :private-key)
   (%public-key :accessor public-key :initarg :public-key)
   (%cipher :accessor cipher :initarg :cipher))
  (:default-initargs
   :cipher nil))

(defmethod initialize-instance :after
    ((object encrypting) &key (key-type :curve25519))
  (unless (and (slot-boundp object '%private-key)
               (slot-boundp object '%public-key))
    (multiple-value-bind (private public) (i:generate-key-pair key-type)
      (setf (private-key object) private
            (public-key object) public))))
