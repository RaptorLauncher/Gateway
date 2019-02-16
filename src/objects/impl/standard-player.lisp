;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; GATEWAY
;;;; © Michał "phoe" Herda 2016-2018
;;;; objects/impl/standard-player.lisp

(in-package #:gateway.objects/impl)

;; TODO tests
(defclass standard-player (player activatable)
  ((%username :accessor username :initarg :username)
   (%display-name :accessor display-name :initarg :display-name)
   (%id :reader id :initarg :id)
   (%email :accessor email :initarg :email)
   (%pass-hash :accessor pass-hash :initarg :pass-hash)
   (%pass-salt :accessor pass-salt :initarg :pass-salt)
   (%activatedp :accessor activatedp :initarg :activatedp)
   (%creation-time :reader creation-time :initarg :creation-time))
  (:default-initargs :id (error "Must provide ID.")
                     :username (error "Must provide username.")
                     :email (error "Must provide email.")
                     :display-name nil
                     :pass-hash nil
                     :pass-salt nil
                     :creation-time (now)
                     :activatedp nil)
  (:documentation #.(format nil "A standard implementation of Gateway protocol ~
class PLAYER.")))

;; TODO shouldn't we actually define those until we have a database?
;; The behavior of DATA-OBJECT-USING-CLASS is going to differ based on how
;; the characters are fetched by their username.

(defmethod object-data ((player standard-player))
  `(:player ,(username player) ,@(when (display-name player)
                                   `(:display-name ,(display-name player)))))

(defmethod data-object-using-class
    ((class (eql (find-class 'standard-player))) data &key)
  )

;; TODO data-object and object-data

(defvar *login-scanner*
  (cl-ppcre:create-scanner
   "^[a-zA-Z0-9]{3,64}$"))

(defvar *email-scanner*
  (cl-ppcre:create-scanner
   "^[a-z0-9\\-._]+@[a-z0-9]+[a-z0-9\\-._]*\\.[a-z0-9\\-_]+$"))

(defparameter *iteration-count* 1024)
(defparameter *key-length* 512)
(defparameter *salt-length* 64)

(define-constructor (standard-player
                     id login email display-name
                     password pass-hash pass-salt
                     creation-time)
  (assert (typep id 'unsigned-byte) ()
          "ID should be a non-negative number, not ~S." id)
  (assert (funcall *login-scanner* login 0 (length login)) ()
          "Invalid login: ~S" login)
  (assert (funcall *email-scanner* email 0 (length email)) ()
          "Invalid email: ~S" email)
  (assert (typep display-name '(or null string)) ()
          "Display name should be NIL or a string, not ~S." display-name)
  (cond (password
         (assert (typep password 'string) ()
                 "PASSWORD should be a string, not ~S." password)
         (setf (password standard-player) password))
        ((and pass-hash pass-salt)
         (assert (typep pass-hash '(array (unsigned-byte 8) (*))) ()
                 "PASS-HASH should be a byte vector, not ~S." pass-hash)
         (assert (typep pass-salt '(array (unsigned-byte 8) (*))) ()
                 "PASS-SALT should be a byte vector, not ~S." pass-salt)
         (setf (pass-hash standard-player) pass-hash
               (pass-salt standard-player) pass-salt)))
  (when creation-time
    (assert (typep creation-time 'date) ()
            "Invalid creation time: ~S." creation-time)))

(defun derive-key (password &key salt)
  (let* ((kdf (ironclad:make-kdf 'ironclad:scrypt-kdf :digest :sha1))
         (password (flex:string-to-octets password :external-format :utf-8))
         (salt (or salt (ironclad:make-random-salt *salt-length*)))
         (key (ironclad:derive-key kdf password salt
                                   *iteration-count* *key-length*)))
    (values key salt)))

(defmethod password-matches-p ((player standard-player) (password string))
  (declare (optimize speed))
  (when (pass-hash player)
    (let ((actual (pass-hash player))
          (expected (derive-key password :salt (pass-salt player))))
      (declare (type (simple-array (unsigned-byte 8) (*)) actual expected))
      (and (= (length actual) (length expected))
           (loop for i across actual
                 for j across expected
                 always (= i j))))))

(defmethod (setf password) ((new-value string) (player standard-player))
  (setf (values (pass-hash player) (pass-salt player))
        (derive-key new-value)))
