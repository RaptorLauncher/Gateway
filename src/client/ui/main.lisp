;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; GATEWAY
;;;; © Michał "phoe" Herda 2016-2020
;;;; package.lisp

(in-package :gateway.client.ui)
(in-readtable :qtools)

(defun main ()
  (dolist (lib '("smokebase" "smokeqtcore" "smokeqtgui" "smokeqimageblitz"))
    (cffi:load-foreign-library (format nil
                                       #+win32 "~A.dll"
                                       #+darwin "qtlibs!~A.dylib"
                                       #+linux "qtlibs!~A.so"
                                       lib)))
  (with-main-window (window (q+:make-qlabel "Hello world!"))
    (setf (q+:minimum-height window) 300
          (q+:minimum-width window) 400)))
