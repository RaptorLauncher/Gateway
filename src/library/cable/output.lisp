;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; GATEWAY
;;;; © Michał "phoe" Herda 2016-2018
;;;; cable/output.lisp

;;;; This software is based on the wire protocol from lichat-protocol by
;;;; Nicolas "Shinmera" Hafner, released under the Artistic license.
;;;; https://github.com/Shirakumo/lichat-protocol/

(in-package #:gateway.cable)

(defgeneric to-cable (object stream)
  (:documentation
   "Converts the provided object to the cable format and outputs it to the
provided stream."))

(defmethod to-cable ((object list) (stream stream))
  (write-char #\( stream)
  (unwind-protect
       (loop for (item . rest) on object
             do (to-cable item stream)
                (when rest (write-char #\  stream)))
    (write-char #\) stream)))

(defmethod to-cable ((object string) (stream stream))
  (write-char #\" stream)
  (unwind-protect
       (loop for char across object
             unless (char= char (code-char 0))
               do (when (or (char= char #\") (char= char #\\))
                    (write-char #\\ stream))
                  (write-char char stream))
    (write-char #\" stream)))

(defmethod to-cable ((object integer) (stream stream))
  (format stream "~D" object))

(defmethod to-cable ((object float) (stream stream))
  (format stream "~F" object))

(defmethod to-cable ((object symbol) (stream stream))
  (format stream "~A" (symbol-name object)))

(defmethod to-cable ((object null) (stream stream))
  (format stream "()"))
