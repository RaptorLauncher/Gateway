;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; GATEWAY
;;;; © Michał "phoe" Herda 2017-2019
;;;; chat.lisp

(in-package :gateway.client.ui)
(in-readtable :qtools)
;;; Main widget

(define-widget chat-window (qwidget)
  ((personas :accessor personas :initarg :personas)
   (ic-posts :accessor ic-posts :initarg :ic-posts)
   (ooc-posts :accessor ooc-posts :initarg :ooc-posts))
  (:default-initargs :personas '() :ic-posts '() :ooc-posts '()))

(define-subwidget (chat-window layout) (q+:make-qgridlayout chat-window))

;;; Bottom row

(define-subwidget (chat-window image-left) (q+:make-qlabel)
  (q+:add-widget layout image-left 1 0)
  ;; (setf (q+:pixmap image-left)
  ;;       (q+:make-qpixmap (homepath "sha.png")))
  )

(define-subwidget (chat-window splitter) (q+:make-qsplitter (q+:qt.horizontal))
  (q+:add-widget layout splitter 1 1))

(define-subwidget (chat-window image-right) (q+:make-qlabel)
  (q+:add-widget layout image-right 1 2)
  ;; (setf (q+:pixmap image-right)
  ;;       (q+:make-qpixmap (homepath "undies.png")))
  )

;;; IC/OOC/WIDGETS

(define-subwidget (chat-window ic) (q+:make-qsplitter (q+:qt.vertical))
  (q+:add-widget splitter ic)
  (setf (q+:minimum-width ic) 200
        (q+:children-collapsible ic) nil
        (q+:stretch-factor splitter 0) 2))

(define-subwidget (chat-window ooc) (q+:make-qsplitter (q+:qt.vertical))
  (q+:add-widget splitter ooc)
  (setf (q+:minimum-width ooc) 200
        (q+:children-collapsible ooc) nil
        (q+:stretch-factor splitter 1) 1))

(define-subwidget (chat-window ic-output)
    (make-placeholder-text-edit "(No IC chat yet.)")
  (q+:add-widget ic ic-output)
  (setf (q+:read-only ic-output) t
        (q+:minimum-height ic-output) 100
        (q+:stretch-factor ic 0) 3)
  (setf (q+:html ic-output)
        (read-file-into-string
         "~/Projects/Raptor Chat/ic.txt")))

(define-subwidget (chat-window ic-input)
    (make-input-text-edit "Type your IC here!")
  (q+:add-widget ic ic-input)
  (setf (q+:minimum-height ic-input) 100
        (q+:stretch-factor ic 1) 1
        (q+:text ic-input)
        "Hello world! This, as yyou can see, jjkhdssdfs ain't no joke."))

(define-subwidget (chat-window ooc-output)
    (make-placeholder-text-edit "[No OOC chat yet.]")
  (q+:add-widget ooc ooc-output)
  (setf (q+:read-only ooc-output) t
        (q+:minimum-height ooc-output) 100
        (q+:stretch-factor ooc 0) 3)
  (setf (q+:html ooc-output)
        (read-file-into-string
         "~/Projects/Raptor Chat/ooc.txt")))

(define-subwidget (chat-window ooc-input)
    (make-input-text-edit "[Type your OOC here!]")
  (q+:add-widget ooc ooc-input)
  (setf (q+:minimum-height ooc-input) 100
        (q+:stretch-factor ooc 1) 1))

(define-subwidget (chat-window dictionary) (make-instance 'qui:dictionary)
  (q+:add-widget splitter dictionary)
  (setf (q+:stretch-factor splitter 1) 1)
  (q+:hide dictionary))

(define-subwidget (chat-window description-left)
    (q+:make-qtextbrowser)
  (q+:add-widget splitter description-left)
  (setf (q+:stretch-factor splitter 2) 1)
  (q+:hide description-left)
  (setf (q+:html description-left)
        (format nil "<h3>Sashasa</h3>~%~A"
                (read-file-into-string "~/Projects/Raptor Chat/sha.txt"))
        (q+:open-external-links description-left) t))

(define-subwidget (chat-window description-right)
    (q+:make-qtextbrowser)
  (q+:add-widget splitter description-right)
  (setf (q+:stretch-factor splitter 3) 1
        (q+:stretch-factor splitter 4) 1)
  (q+:hide description-right)
  (setf (q+:html description-right)
        (format nil "<h3>Undies</h3>~%~A"
                (read-file-into-string "~/Projects/Raptor Chat/undies.txt"))
        (q+:open-external-links description-right) t))

;;; BUTTONS

;;; Upper row

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun forms-subwidgets (forms)
    (loop for tail in (mapcar #'cddr forms)
          for subwidget = (getf tail :subwidget)
          when subwidget collect it))
  (defun make-button (widget name string
                      &key checkable (layout 'buttons-layout) default init-forms
                      &allow-other-keys)
    `(define-subwidget (,widget ,name) (make-chat-button ,string)
       ,@(when layout `((q+:add-widget ,layout ,name)))
       ,@(when checkable `((setf (q+:checkable ,name) t)))
       ,@(when default `((setf (q+:checked ,name) t)))
       ,@init-forms))
  (defun make-slot (widget name string &key subwidget &allow-other-keys)
    (declare (ignore string))
    (let ((slot-name (symbolicate name "-CLICKED"))
          (state (gensym)) (symbol (gensym)))
      `(define-slot (,widget ,slot-name) ()
         (declare (connected ,name (clicked)))
         (let ((,state (q+:save-state splitter)))
           (dolist (,symbol *right-chat-widgets*)
             (q+:hide (slot-value ,widget ,symbol)))
           (q+:show ,subwidget)
           (q+:insert-widget splitter 1 ,subwidget)
           (dolist (,symbol *right-chat-buttons*)
             (setf (q+:checked (slot-value ,widget ,symbol)) nil))
           (setf (q+:checked ,name) t)
           (q+:restore-state splitter ,state))))))

(define-subwidget (chat-window buttons-layout) (q+:make-qhboxlayout)
  (q+:add-layout layout buttons-layout 0 1))

(defun make-text-qtoolbutton (text)
  (let ((button (q+:make-qtoolbutton)))
    (setf (q+:text button) text
          (q+:tool-button-style button)
          (q+:qt.tool-button-text-only))
    button))

(defun make-chat-button (string)
  (let ((button (make-text-qtoolbutton string))
        (font (q+:make-qfont)))
    (setf (q+:point-size font) 8
          (q+:font button) font)
    button))

;; (define-subwidget (chat-window description-button-left)
;;     (make-chat-button "Description")
;;   (q+:add-widget layout description-button-left 0 0))

;; (define-subwidget (chat-window description-button-right)
;;     (make-chat-button "Description")
;;   (q+:add-widget layout description-button-right 0 2))

(trivial-indent:define-indentation define-chat-buttons (4 2 2))

(defmacro define-chat-buttons
    (widget
     (far-left-button)
     left-buttons middle-buttons right-buttons
     (far-right-button))
  `(progn
     ,@(mapcar (curry #'apply #'make-button widget) left-buttons)
     (define-subwidget (,widget buttons-stretch-left)
         (q+:add-stretch buttons-layout 9001))
     ,@(mapcar (curry #'apply #'make-button widget) middle-buttons)
     (define-subwidget (,widget buttons-stretch-right)
         (q+:add-stretch buttons-layout 9001))
     ,@(mapcar (curry #'apply #'make-button widget) right-buttons)
     (defparameter *right-chat-buttons*
       ',(list* (car far-left-button)
                (car far-right-button)
                (mapcar #'car right-buttons)))
     (defparameter *right-chat-widgets*
       ',(list* 'description-left
                'description-right
                (forms-subwidgets right-buttons)))
     ,@(mapcar (curry #'apply #'make-slot widget) right-buttons)
     ,(apply #'make-button widget far-left-button)
     ,(apply #'make-slot widget far-left-button)
     ,(apply #'make-button widget far-right-button)
     ,(apply #'make-slot widget far-right-button)))

(define-chat-buttons chat-window
  ((description-button-left
    "Description"
    :layout nil :checkable t
    :subwidget description-left
    :init-forms
    ((q+:add-widget layout description-button-left 0 0))))
  ((timestamps-button "Timestamps" :checkable t)
   (names-button "Names" :checkable t)
   (justify-button "Justify" :checkable t))
  ((spellchecker-button "Spelling")
   (mark-read-button "Mark Read")
   (logs-button "Logs"))
  ((ooc-button "OOC" :checkable t :subwidget ooc :default t)
   (dictionary-button "Dictionary" :checkable t :subwidget dictionary))
  ((description-button-right
    "Description"
    :layout nil :checkable t
    :subwidget description-right
    :init-forms
    ((q+:add-widget layout description-button-right 0 2 (q+:qt.align-right))))))

(define-signal (chat-window ic-output-modified) ())

(define-signal (chat-window ooc-output-modified) ())

(define-slot (chat-window update-ic-output) ()
  (declare (connected chat-window (ic-output-modified)))
  (update-ic-output chat-window))

(define-slot (chat-window update-ooc-output) ()
  (declare (connected chat-window (ooc-output-modified)))
  (update-ooc-output chat-window))

(define-slot (chat-window buttons-update-outputs) ()
  (declare (connected timestamps-button (clicked))
           (connected names-button (clicked))
           (connected justify-button (clicked)))
  (update-outputs chat-window))

(define-slot (chat-window spellchecker-button-clicked) ()
  (declare (connected spellchecker-button (clicked)))
  (qui:spellcheck ic-input))

(define-slot (chat-window dictionary-text-selection) ()
  (declare (connected ic-input (selection-changed)))
  (with-finalizing ((cursor (q+:text-cursor ic-input)))
    (when (q+:is-visible dictionary)
      (let ((selection (q+:selected-text cursor)))
        (when (string/= selection "")
          (setf (q+:text (slot-value dictionary 'qui:dictionary-input))
                selection))))))

;; TODO hyperlink style
;; TODO descriptions

;;; Random post generation

;; (defun make-posts-from-file (&optional (filename (homepath "posts.txt")))
;;   (with-open-file (stream filename)
;;     (loop for line = (read-line stream nil stream)
;;           until (eq line stream)
;;           collect (make-instance
;;                    'cl-furcadia/clos:standard-post
;;                    :shortname
;;                    (if (eql #\J (aref line 0)) "jacula" "xiaohui")
;;                    :contents line))))

(defun generate-html ;; TODO s/posts/post and mapcar on top of it
    (posts &key print-names-p print-times-p justifyp)
  (with-output-to-string (*standard-output*)
    (loop
      for post in posts
      for persona = (persona post)
      for name = (name persona)
      for date = (timestamp post)
      for outfit = (first (outfits persona))
      for color = (color-dark outfit)
      for contents = (contents post)
      do (format t "<p style=\"color: ~A;\"~A>" color
                 (if justifyp " align=justify" ""))
         (when print-times-p
           (multiple-value-bind (ns ss mm hh day month year dow dstp tzo tza)
               (local-time:decode-timestamp date)
             (declare (ignore ns day month year dow dstp tzo tza))
             (format t "<b>(~2,'0D:~2,'0D:~2,'0D)</b> " hh mm ss)))
         (if print-names-p
             (format t "<b>~A:</b> " (string-capitalize name))
             (princ "<b>&gt;</b> "))
         (princ contents)
         (format t "</p>~%"))))

(defmethod (setf ic-posts) :after (new-value (chat-window chat-window))
  (signal! chat-window (ic-output-modified)))

(defmethod (setf ooc-posts) :after (new-value (chat-window chat-window))
  (signal! chat-window (ooc-output-modified)))

(defmethod add-ic-post ((chat-window chat-window) (post post))
  (appendf (slot-value chat-window 'ic-posts) post)
  (with-slots-bound (chat-window chat-window)
    (q+:append ic-output (posts-html chat-window (list post)))))

(defmethod add-ooc-post ((chat-window chat-window) (post post))
  (appendf (slot-value chat-window 'ooc-posts) post)
  (with-slots-bound (chat-window chat-window)
    (q+:append ooc-output (posts-html chat-window (list post)))))

(defun posts-html (chat-window posts)
  (with-slots-bound (chat-window chat-window)
    (generate-html posts
                   :print-names-p (q+:is-checked names-button)
                   :print-times-p (q+:is-checked timestamps-button)
                   :justifyp (q+:is-checked justify-button))))

(defun update-output (chat-window output posts)
  (let* ((scrollbar (q+:vertical-scroll-bar output))
         (value (q+:value scrollbar))
         (maximum (q+:maximum scrollbar)))
    (setf (q+:html output) (posts-html chat-window posts))
    (unless (= maximum 0)
      (let ((percentage (/ value maximum)))
        (if (= value maximum)
            (setf (q+:value scrollbar) (q+:maximum scrollbar))
            (setf (q+:value scrollbar)
                  (round (* percentage maximum))))))))

(defun update-outputs (chat-window)
  (with-slots-bound (chat-window chat-window)
    (update-output chat-window ic-output ic-posts)
    (update-output chat-window ooc-output ooc-posts)))

(defun update-ic-output (chat-window)
  (with-slots-bound (chat-window chat-window)
    (update-output chat-window ic-output ic-posts)))

(defun update-ooc-output (chat-window)
  (with-slots-bound (chat-window chat-window)
    (update-output chat-window ooc-output ooc-posts)))

;;; MAIN

(defun chat ()
  (with-main-window (chat-window 'chat-window)
    (with-slots-bound (chat-window chat-window)
      (flet ((split-posts (post)
               (let ((contents (contents post)))
                 (or (eql #\[ (char contents 0))
                     (eql #\] (char contents (1- (length contents))))))))
        (setf personas (make-dummy-personas)
              (values ooc-posts ic-posts) (phoe-toolbox:split #'split-posts *posts*))
        (update-outputs chat-window)))))
