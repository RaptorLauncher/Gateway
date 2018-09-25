;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; GATEWAY
;;;; © Michał "phoe" Herda 2016-2018
;;;; connector/impl/standard-writer.lisp

(in-package #:gateway.connector)

(defclass standard-writer (writer)
  ((%name :reader name
          :initform "Gateway - Writer")
   (%queue :reader queue
           :initform (lparallel.queue:make-queue))
   (%thread :accessor thread)))

(define-print (standard-writer stream)
  (format stream "(~:[ALIVE~;DEAD~])" (deadp standard-writer)))

(define-constructor (standard-writer)
  (let ((fn (curry #'writer-loop standard-writer)))
    (setf (thread standard-writer)
          (bt:make-thread fn :name (name standard-writer))))
  (v:trace '(:gateway :writer) "~A: starting." standard-writer))

(defun writer-loop (writer)
  (with-restartability ()
    (loop
      (destructuring-bind (command . rest)
          (lparallel.queue:pop-queue (queue writer))
        (case command
          (:goodbye
           (v:trace '(:gateway :writer) "~A: quitting." writer)
           (return nil))
          (:write
           (destructuring-bind (connection data) rest
             (v:trace '(:gateway :writer) "Standard writer: writing ~A to ~A."
                      data connection)
             (connection-send connection data)))
          (t (v:error '(:gateway :writer) "Unknown command ~S with data ~S."
                      command rest)))))))

(defmethod write-data ((writer standard-writer) (connection connection) data)
  (lparallel.queue:push-queue `(:write ,connection ,data) (queue writer)))

(defmethod deadp ((writer standard-writer))
  (not (bt:thread-alive-p (thread writer))))

(defmethod kill ((writer standard-writer))
  (v:trace '(:gateway :writer) "~A: killed." writer)
  (lparallel.queue:push-queue `(:goodbye) (queue writer))
  (values))
