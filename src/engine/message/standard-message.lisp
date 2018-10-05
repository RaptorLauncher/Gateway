;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; GATEWAY
;;;; © Michał "phoe" Herda 2016-2018
;;;; connector/message/standard-message.lisp

(in-package #:gateway.engine/message)

(define-protocol-class standard-message (message)
  ((%id :reader id
        :initarg :id
        :initform nil)
   (%error-reason :reader error-reason
                  :initarg :error-reason
                  :initform nil)
   (%status :reader status
            :initarg :status
            :initform :request)))

(define-constructor (standard-message id status)
  (assert (typep id '(cons (member :client :server) (cons unsigned-byte null))))
  (assert (typep status '(member nil :request :ok :error))))

(define-print (standard-message stream :type nil)
  (let ((name (class-name (class-of standard-message))))
    (if (and (slot-boundp standard-message '%id)
             (slot-boundp standard-message '%status))
        (let ((id (id standard-message))
              (status (status standard-message)))
          (if (eq status :request)
              (prin1 name stream)
              (format stream "(~A ~S)" status name))
          (destructuring-bind (owner number) id
            (let ((owner (ecase owner (:server :s) (:client :c))))
              (format stream " (~A ~A)" owner number))))
        (prin1 name stream))))

(defclass message-object () ())

(defmethod print-object ((object message-object) stream)
  (print-unreadable-object (object stream :type nil :identity t)
    (handler-case (prin1 (class-name (class-of object)) stream)
      (error () (princ "<error>" stream)))))

(defclass message-class (message-object standard-class) ())

(defmethod c2mop:validate-superclass ((c message-class) (s standard-class)) t)

(defmethod c2mop:validate-superclass ((c standard-class) (s message-class)) t)

(defvar *message-classes* (make-hash-table :test #'equal))

(defun find-message-class (name)
  (assert (and (listp name) (every #'symbolp name)) (name)
          "NAME must be a list of symbols, not ~S." name)
  (values (gethash name *message-classes*)))

(defun (setf find-message-class) (new-value name)
  (assert (and (listp name) (every #'symbolp name)) (name)
          "NAME must be a list of symbols, not ~S." name)
  (assert (typep new-value '(or class null)) (new-value)
          "NEW-VALUE must be a class metaobject or NIL, not ~S." new-value)
  (if new-value
      (setf (gethash name *message-classes*) new-value)
      (remhash name *message-classes*)))

(defmacro define-message-class
    (name direct-superclasses direct-slots &rest options)
  (let ((gensym (gensym)))
    `(unwind-protect
          (progn (setf (find-class ',gensym) (find-message-class ',name))
                 (defclass ,gensym (,@direct-superclasses message-object)
                   ,direct-slots
                   (:metaclass message-class)
                   ,@options)
                 (setf (find-message-class ',name) (find-class ',gensym))
                 (find-class ',gensym))
       (let ((class (find-class ',gensym)))
         (setf (find-class ',gensym nil) nil)
         (setf (class-name class) ',name)))))

(defmacro define-message-method (name &rest args)
  (let* ((args (copy-list args))
         (specializers (loop for arg = (car args)
                             until (listp arg)
                             collect (pop args) into result
                             finally (return (nreverse result)))))
    ;; TODO more work
    `'(list ,name ,specializers ,args)))

#|
14:20 < beach> phoe: What problem are you trying to solve?
14:27 < phoe> beach: extending CLOS to add an alternate class type. Classes are usually named by symbols; I've found an use case for objects that are named by lists
containing two symbols.
14:28 < beach> phoe: I think the presentation types in McCLIM are represented by classes named that way.
14:29 < beach> Apparently, it is allowed.
14:29 < beach> http://metamodular.com/CLOS-MOP/initialization-of-class-metaobjects2.html
14:30 < beach> "The :name argument is an object."
14:30 < phoe> allowed?
14:30 < phoe> ...
14:30 < phoe> http://metamodular.com/CLOS-MOP/class-name.html
14:30 < phoe> This is different than http://clhs.lisp.se/Body/f_class_.htm
14:31 < beach> Yes.
14:31 < beach> You can't find such a class using FIND-CLASS, but the intrinsic name of the class can be any object according to the MOP.
14:32 < phoe> But I could write my own function that is named FIND-FOO.
14:32 < phoe> That will find a FOO with the name (BAR BAZ).
14:32 < beach> Yes, you can have your classes in your own database.
14:33 < phoe> And I'll need a DEFINE-FOO that calls ENSURE-FOO that calls ENSURE-FOO-USING-CLASS.
14:36 < beach> You could "cheat".
14:36 < phoe> How?
14:36 < phoe> By defining methods on ENSURE-CLASS?
14:36 < phoe> ...-USING-CLASS?
14:36 < beach> Name your class with (gensym), then, once it is in your database, (reinitialize-instance (find-class <that-gensym>) :name <your-name>)
14:37 < beach> Then you can use the existing machinery.
14:37 < phoe> Oh.
14:37 < phoe> How would subsequent DEFCLASS calls work?
14:37 < phoe> I mean, would this make it possible to reinitialize the class using DEFCLASS?
14:37 < beach> You can't use DEFCLASS with anything other than a symbol.
14:37 < phoe> Oh, wait, that's correct.
14:38 < phoe> But, wait.
14:38 < phoe> It is in my database now, so I can call (find-foo '(bar baz)).
14:38 < phoe> And I can call REINITIALIZE-INSTANCE manually.
14:38 < phoe> Which means that I'll nonetheless need to write that piece of the machinery.
14:38 < beach> Not necessarily.
14:39 < phoe> ...do I first rename the class back to the gensym, then call ENSURE-CLASS-USING-CLASS, then rename the class back to the list?
14:39 < beach> Something like that.
14:40 < phoe> This is a hack.
14:40 < phoe> ........I think I'll do that.
14:40 < beach> You do (setf (find-class (gensym)) (find-my-class <your-name>))
14:40 < beach> Then (ensure-class-using-class...)
14:40 < beach> Then (setf (find-class <that-gensym>) nil)
14:40 < beach> You don't have to use the original gensym.
14:41 < phoe> Oh, so the gensym only has lexical extent.
14:41 < beach> If you setf the find-class to NIL, then it will be gone after that.
14:41 < beach> So, yes.
14:41 < beach> You don't have to remember it.

14:44 < phoe> So, basically, I can specialize CLASS-NAME on my custom subclass of STANDARD-CLASS.
14:45 < phoe> And it'll return whatever I tell it to return.
14:49 < beach> You don't need to do that.
14:49 < beach> CLASS-NAME returns the intrinsic name of the class.
14:49 < beach> Not the name used as key in the database.
14:50 < phoe> Oh. Right.
14:50 < phoe> So I can already set arbitrary data there and retrieve it.
14:50 < beach> Yes.
14:50 < phoe> That's even better.
14:50 < beach> You just need to write your own FIND-CLASS and (SETF FIND-CLASS)
14:51 < beach> ... and your own version of DEFCLASS if you want to be able to define classes that way.
14:51 < phoe> Yes, and in the trivial case it's just GETHASH, (SETF GETHASH), and REMHASH.
14:51 < beach> But your macro could expand to DEFCLASS with a GENSYM.
14:51 < phoe> And DEFINE-FOO that wraps around DEFCLASS---
14:51 < phoe> Yes, exactly this.
14:52 < beach> Make sure you stick some UNWIND-PROTECT in there so that you don't accumulate gensym-ed class names in find-class.

|#
