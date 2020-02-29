;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; GATEWAY
;;;; © Michał "phoe" Herda 2016-2020
;;;; package.lisp

(in-package :gateway.client.ui)
(in-readtable :qtools)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; PZMQ stuff

(cffi:define-foreign-library libsodium
  (:darwin "libsodium.dylib")
  (:unix "libsodium.so")
  (:windows "libsodium.dll")
  (t (:default "libsodium")))

(cffi:load-foreign-library 'libsodium)

(defmacro kill-socket (place)
  `(when ,place
     (pzmq:close ,place)
     (setf ,place nil)))

(defmacro kill-context (place)
  `(when ,place
     (pzmq:ctx-term ,place)
     (setf ,place nil)))

(defun set-socket-options (socket &rest args)
  (loop for (key value) on args by #'cddr
        do (pzmq:setsockopt socket key value)))

(defparameter *client-context* nil)
(defparameter *client-socket* nil)
(defparameter *client-socket-options*
  '(:linger 100
    :probe-router 1))

(defun start-client (&optional (server-address "tcp://localhost:6500"))
  (setf *client-context* (pzmq:ctx-new))
  (setf *client-socket* (pzmq:socket *client-context* :dealer))
  (apply #'set-socket-options *client-socket* *client-socket-options*)
  (pzmq:connect *client-socket* server-address))

(defun stop-client ()
  (kill-socket *client-socket*)
  (kill-context *client-context*))

(defun client-send (data)
  (fformat t "~&Sending data ~S." data)
  (pzmq:send *client-socket*
             (with-output-to-string (stream) (gateway.cable:to-cable data stream))))

(defun client-receive ()
  (loop for message = (handler-case (pzmq:recv-string *client-socket* :dontwait t)
                        (pzmq:eagain () nil))
        while message
        do (fformat t "~&Received data ~S." message)
        collect (with-input-from-string (stream message)
                  (gateway.cable:from-cable stream))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Widget definition

(define-widget main-window (qwidget) ())

(define-subwidget (main-window layout) (q+:make-qgridlayout main-window))

(define-subwidget (main-window view) (q+:make-qtextbrowser)
  (q+:add-widget layout view 0 0 1 2))

(define-subwidget (main-window input) (q+:make-qlineedit)
  (q+:add-widget layout input 1 0))

(define-subwidget (main-window send-button) (q+:make-qpushbutton "Emit")
  (q+:add-widget layout send-button 1 1))

(define-slot (main-window send-message) ()
  (declare (connected input (return-pressed))
           (connected send-button (pressed)))
  (let ((message (q+:text input)))
    (setf (q+:text input) "")
    (client-send `(message ,message))))

(define-subwidget (main-window timer) (q+:make-qtimer)
  (setf (q+:interval timer) 100))

(define-slot (main-window timer-fired) ()
  (declare (connected timer (timeout)))
  (when-let ((messages (client-receive)))
    (dolist (message messages)
      (when (and (consp message) (gateway.cable:cable-equal (first message) :message))
        (q+:append view (second message))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Main function

(defun main-window ()
  (with-main-window (window (make-instance 'main-window))
    (with-slots-bound (window main-window)
      (q+:start timer))))

(defun main ()
  (dolist (lib '("smokebase" "smokeqtcore" "smokeqtgui" "smokeqimageblitz"))
    (cffi:load-foreign-library (format nil
                                       #+win32 "~A.dll"
                                       #+darwin "qtlibs!~A.dylib"
                                       #+linux "qtlibs!~A.so"
                                       lib)))
  (start-client "tcp://raptor.systems:6500")
  (main-window)
  (stop-client))
