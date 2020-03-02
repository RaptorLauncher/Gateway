;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; GATEWAY
;;;; © Michał "phoe" Herda 2016-2020
;;;; library/library.lisp

(defpackage #:gateway.library
  (:use #:cl)
  (:local-nicknames (#:z #:pzmq)
                    (#:g #:legit))
  (:export #:kill-socket
           #:kill-context
           #:set-socket-options
           #:gateway-version))

(in-package #:gateway.library)

(defmacro kill-socket (place)
  `(when ,place
     (z:close ,place)
     (setf ,place nil)))

(defmacro kill-context (place)
  `(when ,place
     (z:ctx-term ,place)
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
