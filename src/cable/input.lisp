;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; GATEWAY
;;;; © Michał "phoe" Herda 2016-2018
;;;; cable/input.lisp

;;;; This software is based on the wire protocol from lichat-protocol by
;;;; Nicolas "Shinmera" Hafner, released under the Artistic license.
;;;; https://github.com/Shirakumo/lichat-protocol/

(in-package #:gateway.cable)

;;; VARS AND UTILS

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *whitespace*
    (map 'vector #'code-char '(#x0009 #x000A #x000B #x000C #x000D #x0020))))

(defvar *read-limit* nil)
(defvar *read-counter*)
(defvar *backup-string* nil)
;; TODO depth limit

(defun make-backup-string ()
  (make-array 0 :element-type 'character :adjustable t :fill-pointer 0))

(define-condition incomplete-input () ())

(define-condition read-limit-hit (error) ())

(define-condition cable-error (simple-condition error) ())

(defun make-cable-error (&optional format-control &rest format-arguments)
  "Creates a CABLE-ERROR with the provided format control and arguments."
  (make-instance 'cable-error :format-control format-control
                              :format-arguments format-arguments))

(defun whitespace-p (char)
  (find char *whitespace*))

;;; LREAD/LPEEK/LUNREAD


(defun lread (stream &optional eof)
  (when *read-limit*
    (when (< *read-limit* *read-counter*)
      (error 'read-limit-hit))
    (incf *read-counter*))
  (let ((char (read-char-no-hang stream (not eof) eof)))
    (cond ((null char) (signal 'incomplete-input))
          ((and *backup-string* (not (eql char eof)))
           (vector-push-extend char *backup-string*)))
    char))

(defun lpeek (stream)
  (let ((char (phoe-toolbox:peek-char-no-hang stream)))
    (unless char (signal 'incomplete-input))
    (when *read-limit*
      (when (<= *read-limit* *read-counter*)
        (error 'read-limit-hit)))
    char))

(defun lpeek* (stream &key eof)
  (let ((char (phoe-toolbox:peek-char-no-hang stream (not eof) eof)))
    (when (and (characterp char) *read-limit*)
      (when (<= *read-limit* *read-counter*)
        (error 'read-limit-hit)))
    char))

(defun lunread (char stream)
  (when *read-limit*
    (decf *read-counter*))
  (when *backup-string*
    (decf (fill-pointer *backup-string*)))
  (unread-char char stream))

(defun skip-whitespace (stream)
  (loop for char = (lread stream)
        unless char
          do (signal 'incomplete-input)
        while (find char *whitespace*)
        when *read-limit*
          do (when (< *read-limit* *read-counter*)
               (error 'read-limit-hit))
        finally (lunread char stream)))

;;; READ-SEXPR

(defun read-sexpr-string (stream)
  (with-output-to-string (out)
    (loop for char = (lread stream)
          do (case char
               (#\Nul (error "Stray NULL found in input."))
               (#\\ (write-char (lread stream) out))
               (#\" (return))
               (t (write-char char out))))))

(defun read-sexpr-number (stream)
  (let ((out (make-string-output-stream))
        (point nil))
    (loop for i from 0
          for char = (lread stream :eof)
          do (case char
               (:eof (return))
               (#\. (cond (point (lunread char stream) (return))
                          (t (setf point i))))
               ((#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)
                (write-char char out))
               (t (lunread char stream) (return))))
    (let* ((number-string (get-output-stream-string out))
           (number (if (string= number-string "")
                       0
                       (parse-integer number-string))))
      (if point
          (let ((decimal (- (length number-string) point)))
            (if (= 0 decimal)
                (coerce number 'double-float)
                (coerce (/ number (expt 10 decimal)) 'double-float)))
          number))))

(defun read-sexpr-token (stream)
  (lpeek stream)
  (with-output-to-string (out)
    (loop for char = (lread stream :eof)
          do (case char
               (#\\ (write-char (lread stream) out))
               (#.(list* #\" #\( #\) #\0 #\1 #\2 #\3
                         #\4 #\5 #\6 #\7 #\8 #\9 #\. #\Nul
                         (coerce *whitespace* 'list))
                (lunread char stream) (return))
               (:eof (return))
               (t (write-char (char-upcase char) out))))))

(defun read-sexpr-symbol (stream)
  (let ((token (read-sexpr-token stream)))
    (make-symbol token)))

(defun read-sexpr-list (stream)
  (prog1 (loop do (skip-whitespace stream)
               until (eql #\) (lpeek stream))
               collect (read-sexpr stream))
    (lread stream)))

(defun read-sexpr (stream)
  (skip-whitespace stream)
  (let* ((char (lread stream))
         (result (case char
                   (#\Nul (error (make-cable-error "Stray null found.")))
                   (#\( (read-sexpr-list stream))
                   (#\) (error (make-cable-error "Stray closing paren found.")))
                   (#\" (read-sexpr-string stream))
                   ((#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9 #\.)
                    (lunread char stream)
                    (read-sexpr-number stream))
                   (t (lunread char stream)
                    (read-sexpr-symbol stream)))))
    (loop for char = (lpeek* stream :eof stream)
          while (whitespace-p char)
          do (lread stream))
    result))

;;; FROM-CABLE

(defun from-cable (stream)
  "Reads from the provided cable stream and outputs the read object as its
primary value. In case there is not enough data on the stream to form a complete
object, returns NIL as its primary value and the read data as its secondary
value."
  (let ((*backup-string* (make-backup-string))
        (*read-counter* 0))
    (handler-case (values (read-sexpr stream) nil)
      (incomplete-input ()
        (return-from from-cable (values nil *backup-string*))))))

;;; FROM-CABLE-BUFFERED

(defun %from-cable-buffered-no-buffer (stream)
  (multiple-value-bind (result new-buffer) (from-cable stream)
    (setf (buffer-of stream) new-buffer)
    result))

(defun %from-cable-buffered-buffer (stream buffer)
  (let* ((buffer-stream (make-string-input-stream buffer))
         (final-stream (make-concatenated-stream buffer-stream stream)))
    (multiple-value-bind (result new-buffer) (from-cable final-stream)
      (setf (buffer-of stream) new-buffer)
      result)))

(defun from-cable-buffered (stream)
  "Reads from the provided cable stream and outputs the read object. In case
there is not enough data on the stream to form a complete object, returns NIL
and saves the read data into an internal buffer to allow subsequent calls to
FROM-CABLE-BUFFERED to read a complete object. In case a cable error is
signaled, the buffer is reset to an empty state."
  (handler-bind
      ((cable-error
         (lambda (e) (declare (ignore e)) (setf (buffer-of stream) nil))))
    (let* ((buffer (buffer-of stream)))
      (if buffer
          (%from-cable-buffered-buffer stream buffer)
          (%from-cable-buffered-no-buffer stream)))))
