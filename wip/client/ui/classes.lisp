;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; GATEWAY
;;;; © Michał "phoe" Herda 2017
;;;; classes.lisp

(in-package :gateway.client.ui)
(in-readtable :qtools)

;;; IMAGE

(defclass image ()
  ((%foreground-path :accessor foreground-path :initarg :foreground-path)
   (%background-path :accessor background-path :initarg :background-path)
   (%eye-level :accessor eye-level :initarg :eye-level)
   (%optimal-width :accessor optimal-width :initarg :optimal-width)
   (%background-hue :accessor background-hue :initarg :background-hue))
  (:default-initargs :foreground-path nil :background-path nil
                     :eye-level nil :optimal-width nil
                     :background-hue 0.0))

;;; OUTFIT

(defclass outfit ()
  ((%description :accessor description :initarg :description)
   (%images :accessor images :initarg :images)
   (%color-light :accessor color-light :initarg :color-light)
   (%color-dark :accessor color-dark :initarg :color-dark))
  (:default-initargs :description nil :images '()
                     :color-light "#111111" :color-dark "#EEEEEE"))

;;; PERSONA

(defclass persona ()
  ((%name :accessor name :initarg :name)
   (%outfits :accessor outfits :initarg :outfits))
  (:default-initargs :name (required-argument :name)
                     :outfits '()))

(defmethod print-object ((persona persona) stream)
  (print-unreadable-object (persona stream :type t)
    (format stream "~S (~D images)" (name persona) (length (images persona)))))

;;; POST

(defclass post ()
  ((%persona :accessor persona :initarg :persona)
   (%timestamp :accessor timestamp :initarg :timestamp)
   (%contents :accessor contents :initarg :contents))
  (:default-initargs :persona (required-argument :persona)
                     :contents (required-argument :contents)
                     :timestamp (local-time:now)))

(defmethod print-object ((post post) stream)
  (with-accessors ((persona persona) (timestamp timestamp) (contents contents))
      post
    (print-unreadable-object (post stream :type t)
      (format stream "(~A, ~A) \"~A\""
              (when persona (name persona))
              (local-time:format-timestring nil timestamp
                                            :format local-time:+asctime-format+)
              (if (<= (length contents) 20)
                  contents
                  (format nil "~A..." (subseq contents 0 20)))))))

;;; STREAM

(defclass stream ()
  ((%name :accessor name :initarg :name)
   (%posts :accessor posts :initarg :posts))
  (:default-initargs :name (required-argument :name)
                     :posts '()))
