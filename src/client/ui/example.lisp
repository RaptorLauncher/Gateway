;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; GATEWAY
;;;; © Michał "phoe" Herda 2017
;;;; example.lisp

(in-package :gateway.client.ui)
(in-readtable :qtools)

;;; IMAGES

(defparameter *image-bahtami*
  (make-instance 'image :foreground-path (homepath "bahta.png")
                        :background-path (homepath "tile3.png")
                        :optimal-width 300
                        :eye-level 0.17
                        :background-hue 0.16))

(defparameter *image-kimya*
  (make-instance 'image :foreground-path (homepath "kimya-cropped.png")
                        :background-path (homepath "tile4.png")
                        :optimal-width 300
                        :eye-level 0.05
                        :background-hue 0.05))

;;; PERSONAS

(defparameter *persona-bahtami*
  (make-instance 'persona :name "Bahtami Quele"
                          :images (list *image-bahtami*)
                          :color-light "#BCAAA4"
                          :color-dark "#4E342E"))

(defparameter *persona-kimya*
  (make-instance 'persona :name "Kimya Kivuli"
                          :images (list *image-kimya*)
                          :color-light "#81C784"
                          :color-dark "#1B5E20"))

(defparameter *persona-druta*
  (make-instance 'persona :name "Druta Pani"
                          :images '()
                          :color-light "#DCBA74"
                          :color-dark "#6E440E"))

;;; STREAMS

(defparameter *posts*
  (load-file (homepath "bahtami.txt")
             (list *persona-bahtami* *persona-kimya* *persona-druta*)))

(defparameter *stream*
  (make-instance 'stream :name "Bahtami's Journey"
                         :posts *posts*))
