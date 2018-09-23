;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; GATEWAY
;;;; © Michał "phoe" Herda 2016-2018
;;;; connector/impl/standard-listener.lisp

(in-package #:gateway.connector)

(defclass standard-listener (listener)
  ((%lock :accessor lock
          :initform (bt:make-lock "Gateway - Listener lock"))
   (%connections :accessor connections)
   (%notifier-connection :accessor notifier-connection)
   (%thread :accessor thread)
   (%name :accessor name
          :initform "Gateway - Listener")
   (%handler :accessor handler
             :initarg :handler
             :initform (error "Must define a handler function.")))
  (:documentation #.(format nil "A standard implementation of Gateway protocol ~
class LISTENER.

The STANDARD-LISTENER spawns a thread which monitors the CONNECTIONs on the ~
connection list by means of READY-CONNECTION. It then attempts ~
to read a command by CONNECTION-RECEIVE and, if it is read, call its handler ~
function with the connection and the command as arguments.

The handler is expected to push a message in form (CONNECTION COMMAND) into ~
a designated place.")))

(define-print (standard-listener stream)
  (if (deadp standard-listener)
      (format stream "(DEAD)")
      (let ((connections (connections standard-listener)))
        (format stream "(~D connections, ALIVE)" (length connections)))))

(define-constructor (standard-listener)
  (v:trace '(:gateway :acceptor) "Standard listener starting.")
  (destructuring-bind (connection-1 connection-2) (make-connection-pair)
    (let ((fn (curry #'listener-loop standard-listener)))
      (setf (notifier-connection standard-listener) connection-1
            (connections standard-listener) (list connection-2)
            (thread standard-listener)
            (bt:make-thread fn :name (name standard-listener))))))

(defmethod add-connection ((listener standard-listener) (connection connection))
  (bt:with-lock-held ((lock listener))
    (push connection (connections listener))
    (connection-send (notifier-connection listener) '())))

(defmethod connection-count ((listener standard-listener))
  (length (connections listener)))

(defun listener-ready-connection (listener)
  (let ((conns (bt:with-lock-held ((lock listener)) (connections listener))))
    (tagbody :start
       (assert (not (null conns)) () "Listener: connection list is empty.")
       (let ((conn (ready-connection conns)))
         (unless conn (go :start))
         (v:trace :gateway "Standard listener: receiving from ~A."
                  (socket-peer-address conn))
         (return-from listener-ready-connection conn)))))

(defun listener-loop (listener)
  (with-restartability (listener)
    (loop
      (handler-case
          (let ((connection (listener-ready-connection listener)))
            (multiple-value-bind (command alivep)
                (connection-receive connection)
              (unless alivep
                (bt:with-lock-held ((lock listener))
                  (removef (connections listener) connection :count 1)))
              (when command
                (funcall (handler listener) connection command))))
        (stream-error (e) (listener-error listener e))))))

(defun listener-error (listener condition)
  (let* ((connections (bt:with-lock-held ((lock listener))
                        (connections listener)))
         (stream (stream-error-stream condition))
         (predicate (lambda (x) (eq stream (usocket:socket-stream x))))
         (connection (find-if predicate connections)))
    (when connection
      (v:debug :gateway "Standard listener: removing dead connection ~A."
               (address connection))
      (kill connection)
      (bt:with-lock-held ((lock listener))
        (removef (connections listener) connection :count 1)))))

(defmethod deadp ((listener standard-listener))
  (not (bt:thread-alive-p (thread listener))))

(defmethod kill ((listener standard-listener))
  (v:trace :gateway "Standard listener was killed.")
  (unless (eq (thread listener) (bt:current-thread))
    (bt:destroy-thread (thread listener)))
  (kill (notifier-connection listener))
  (bt:with-lock-held ((lock listener))
    (mapc #'kill (connections listener))
    (setf (connections listener) '()))
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

;;; TESTS
