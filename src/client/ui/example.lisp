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

;;; OUTFITS

(defparameter *outfit-bahtami*
  (make-instance
   'outfit
   :description (read-file-into-string (homepath "desc-bahta.txt"))
   :images (list *image-bahtami*)
   :color-dark "#BCAAA4"
   :color-light "#4E342E"))

(defparameter *outfit-kimya*
  (make-instance
   'outfit
   :description (read-file-into-string (homepath "desc-kimya.txt"))
   :images (list *image-kimya*)
   :color-dark "#81C784"
   :color-light "#1B5E20"))

(defparameter *outfit-druta*
  (make-instance
   'outfit
   :description (read-file-into-string (homepath "desc-druta.txt"))
   :images '()
   :color-dark "#DCBA74"
   :color-light "#6E440E"))

;;; PERSONAS

(defparameter *persona-bahtami*
  (make-instance 'persona :name "Bahtami Quele"
                          :outfits (list *outfit-bahtami*)))

(defparameter *persona-kimya*
  (make-instance 'persona :name "Kimya Kivuli"
                          :outfits (list *outfit-kimya*)))

(defparameter *persona-druta*
  (make-instance 'persona :name "Druta Pani"
                          :outfits (list *outfit-druta*)))

;;; STREAMS

(defparameter *posts*
  (load-file (homepath "bahtami.txt")
             (list *persona-bahtami* *persona-kimya* *persona-druta*)))

(defparameter *stream*
  (make-instance 'stream :name "Bahtami's Journey"
                         :posts *posts*))

;;; EXAMPLES

;; TODO deprecate or add DESCRIPTION arg
(defun make-persona (name description images color-light color-dark)
  (let ((outfits (list (make-instance 'outfit
                                      :description description
                                      :images images
                                      :color-light color-light
                                      :color-dark color-dark))))
    (make-instance 'persona :name name :outfits outfits)))

(defun make-dummy-personas (;; &key (name-1 "Erchembod") (name-2 "Scaletail")
                            )
  ;; (let* ((image-1 (homepath "erchembod.png"))
  ;;        (image-2 (homepath "scaletail.png"))
  ;;        ;; TODO add proper image classes here
  ;;        (persona-1 (make-persona name-1 name-1 '() "#CC33FF" "#33CC00"))
  ;;        (persona-2 (make-persona name-2 name-2 '() "#EEFF88" "#110077")))
  ;;   (list persona-1 persona-2))
  (list *persona-kimya* *persona-bahtami* *persona-druta*))

(defun make-lorem-ipsum-posts (n personas)
  (let ((personas (copy-list personas)))
    (setf (cdr (last personas)) personas)
    (loop repeat n
          for persona in personas
          for contents = (lorem-ipsum:paragraph :prologue nil)
          collect (make-instance 'post :persona persona
                                       :contents contents))))
