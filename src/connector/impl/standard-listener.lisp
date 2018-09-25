;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; GATEWAY
;;;; © Michał "phoe" Herda 2016-2018
;;;; connector/impl/standard-listener.lisp

(in-package #:gateway.connector)

(defclass standard-listener (listener)
  ((%name :accessor name
          :initform "Gateway - Listener")
   (%handler :accessor handler
             :initarg :handler
             :initform (error "Must define a handler function."))
   (%control-input-connection :accessor control-input-connection)
   (%control-output-connection :accessor control-output-connection)
   (%connections :accessor connections)
   (%lock :accessor lock
          :initform (bt:make-recursive-lock "Gateway - Listener lock"))
   (%thread :accessor thread))
  (:documentation #.(format nil "A standard implementation of Gateway protocol ~
class LISTENER.
\
The STANDARD-LISTENER spawns a thread which monitors the CONNECTIONs on the ~
connection list by means of READY-CONNECTION. It then attempts ~
to read a command by CONNECTION-RECEIVE and, if it is read, call its handler ~
function with the connection and the command as arguments.")))

(define-print (standard-listener stream)
  (format stream "(~:[ALIVE, ~D connections~;DEAD~])"
          (deadp standard-listener)
          (connection-count standard-listener)))

(define-constructor (standard-listener)
  (destructuring-bind (conn-1 conn-2) (make-connection-pair)
    (bt:with-lock-held ((lock standard-listener))
      (let ((fn (curry #'listener-loop standard-listener)))
        (setf (control-input-connection standard-listener) conn-1
              (control-output-connection standard-listener) conn-2
              (connections standard-listener) (list conn-2)
              (thread standard-listener)
              (bt:make-thread fn :name (name standard-listener))))))
  (v:trace '(:gateway :listener) "~A: starting." standard-listener))

(defmethod add-connection ((listener standard-listener) (connection connection))
  (bt:with-lock-held ((lock listener))
    (push connection (connections listener))
    (connection-send (control-input-connection listener) '())))

(defmethod connection-count ((listener standard-listener))
  (bt:with-lock-held ((lock listener))
    (1- (length (connections listener)))))

(defun listener-ready-connection (listener)
  (loop for conns = (bt:with-lock-held ((lock listener)) (connections listener))
        for conn = (ready-connection conns)
        when conn do (v:trace '(:gateway :listener)
                              "Receiving from ~A."
                              (socket-peer-address conn))
        and return conn))

(defun remove-connection (listener connection)
  ;; TODO define disconnection handler
  ;; TODO call disconnection handler here
  (bt:with-lock-held ((lock listener))
    (removef (connections listener) connection :count 1)))

(defun listener-loop (listener)
  (with-restartability ()
    (let ((control-connection (control-output-connection listener)))
      (loop
        (handler-case
            (let ((connection (listener-ready-connection listener)))
              (multiple-value-bind (command alivep)
                  (connection-receive connection)
                (cond ((not alivep)
                       (remove-connection listener connection))
                      ((null command))
                      ((and (eq connection control-connection)
                            (cable-equal command '(#:goodbye)))
                       (v:trace '(:gateway :listener)
                                "~A: quitting." listener)
                       (kill (control-output-connection listener))
                       (kill (control-input-connection listener))
                       (mapc #'kill (connections listener))
                       (return-from listener-loop))
                      (t (funcall (handler listener) connection command)))))
          (stream-error (e)
            (bt:with-lock-held ((lock listener))
              (listener-stream-error listener e))))))))

(defun listener-stream-error (listener condition)
  (when-let* ((connections (connections listener))
              (stream (stream-error-stream condition))
              (predicate (lambda (x) (eq stream (usocket:socket-stream x))))
              (connection (find-if predicate connections)))
    (v:debug :gateway "Removing dead ~A."
             (address connection))
    (kill connection)
    (remove-connection listener connection)))

(defmethod deadp ((listener standard-listener))
  (not (bt:thread-alive-p (thread listener))))

(defmethod kill ((listener standard-listener))
  (handler-case
      (progn (v:trace '(:gateway :listener) "~A: killed." listener)
             (connection-send (control-input-connection listener) '(#:goodbye)))
    (stream-error (e)
      (v:debug '(:gateway :listener) "Stream error while killing ~A: ~A"
               listener e)))
  (values))

;; Oh goodness, I remember the days when I've had no idea what a closure was
;; and how a function can be an object.
;; ~phoe, 28 Dec 2017

;; Oh goodness, I remember the days when I wrote the above comment. I've learned
;; so much since then.
;; ~phoe, 03 Aug 2017

;; Oh goodness, I remember the days when I walked into a project that I have not
;; been in for some time and immediately going "wtf, who wrote this." These days
;; are over now.
;; ~phoe, 30 Mar 2018

;; Oh goodness, I remember the days when I actually began working on Gateway.
;; I was a soloist back then, without any kind of other people backing the
;; project up. There's a whole community around it right now.
;; ~phoe, 23 Sep 2018
