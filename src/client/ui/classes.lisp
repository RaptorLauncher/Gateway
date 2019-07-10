;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; RAPTOR-LAUNCHER
;;;; © Michał "phoe" Herda 2017
;;;; classes.lisp

(in-package :gateway.client.ui)
(in-readtable :qtools)

;;; PERSONA

(defclass persona ()
  ((%name :accessor name :initarg :name)
   (%image-path :accessor image-path :initarg :image-path)
   (%color-light :accessor color-light :initarg :color-light)
   (%color-dark :accessor color-dark :initarg :color-dark)
   ;; TODO stuff all required image-widget data here
   (%foreground-path :accessor foreground-path :initarg :foreground-path)
   (%background-path :accessor background-path :initarg :background-path)
   (%foreground :accessor foreground :initarg :foreground)
   (%background :accessor background :initarg :background)
   (%eye-level :accessor eye-level :initarg :eye-level)
   (%min-width :accessor min-width :initarg :min-width)
   (%shadow-level :accessor shadow-level :initarg :shadow-level)
   (%shadow-height :accessor shadow-height :initarg :shadow-height)
   (%background-hue :accessor background-hue :initarg :background-hue))
  (:default-initargs :name (required-argument :name)
                     :color-light nil :color-dark nil
                     :foreground-path nil :foreground nil
                     :background-path nil :background nil
                     :eye-level nil :min-width nil
                     :shadow-level 0.5 :shadow-height 1000
                     :background-hue 0.0))

(defun make-persona (name image color-light color-dark)
  (make-instance 'persona :name name :image-path image
                          :color-light color-light :color-dark color-dark))

;;; POST

(defclass post ()
  ((persona :accessor persona :initarg :persona)
   (timestamp :accessor timestamp :initarg :timestamp)
   (contents :accessor contents :initarg :contents))
  (:default-initargs :persona (required-argument :persona)
                     :contents (required-argument :contents)
                     :timestamp (local-time:now)))

(defmethod print-object ((post post) stream)
  (with-slots (persona timestamp contents) post
    (print-unreadable-object (post stream :type t)
      (format stream "(~A, ~A) \"~A\""
              (typecase persona (persona (name persona)) (t persona))
              (local-time:format-timestring nil timestamp
                                            :format local-time:+asctime-format+)
              (if (<= (length contents) 20)
                  contents
                  (format nil "~A..." (subseq contents 0 20)))))))

(defun make-dummy-personas (&key (name-1 "Erchembod") (name-2 "Scaletail"))
  (let* ((image-1 (homepath "erchembod.png"))
         (persona-1 (make-persona name-1 image-1 "#CC33FF" "#33CC00"))
         (image-2 (homepath "scaletail.png"))
         (persona-2 (make-persona name-2 image-2 "#EEFF88" "#110077")))
    (list persona-1 persona-2)))

(defun make-lorem-ipsum-posts (n personas)
  (let ((personas (copy-list personas)))
    (setf (cdr (last personas)) personas)
    (loop repeat n
          for persona in personas
          for contents = (lorem-ipsum:paragraph :prologue nil)
          collect (make-instance 'post :persona persona
                                       :contents contents))))
