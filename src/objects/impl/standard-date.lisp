;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; GATEWAY
;;;; © Michał "phoe" Herda 2016-2018
;;;; engine/objects/standard-date.lisp

(in-package #:gateway.engine/objects)

(defclass standard-date (date gateway-object local-time:timestamp) ()
  (:documentation #.(format nil "A standard implementation of Gateway protocol ~
class DATE.")))

(defmethod data-object-using-class
    ((class (eql (find-class 'standard-date))) data &key)
  (check-type data cons)
  (assert (= (length data) 2))
  (destructuring-bind (symbol timestamp) data
    (check-type symbol symbol)
    (assert (string= symbol :date))
    (check-type timestamp string)
    (change-class (local-time:parse-timestring timestamp) 'standard-date)))

(defmethod object-data ((object standard-date))
  `(:date ,(format nil "~A" object)))

(defmethod date-timestamp ((date standard-date))
  (local-time:timestamp-to-unix date))

(defmethod timestamp-date-using-class
    ((class (eql (find-class 'standard-date))) (timestamp integer))
  (change-class (local-time:unix-to-timestamp timestamp) 'standard-date))

(defmethod date-ustimestamp ((date standard-date))
  (let ((timestamp (date-timestamp date))
        (nsec (truncate (local-time:nsec-of date) 1000)))
    (+ (* 1000000 timestamp) nsec)))

(defmethod ustimestamp-date-using-class
    ((class (eql (find-class 'standard-date))) (nstimestamp integer))
  (multiple-value-bind (timestamp msec) (truncate nstimestamp 1000000)
    (let ((date (timestamp-date-using-class class timestamp)))
      (setf (local-time:nsec-of date) (* 1000 msec))
      date)))

(defmethod date=
    ((date-1 standard-date) (date-2 standard-date) &key (unit :microsecond))
  (assert (member unit *date-granularity-units*))
  (if (eq unit :microsecond)
      (local-time:timestamp= date-1 date-2)
      (every #'= (date-elts date-1 unit) (date-elts date-2 unit))))

(defun date-elts (date unit)
  (let ((decoded (nreverse (multiple-value-list
                            (local-time:decode-timestamp
                             date :timezone local-time:+utc-zone+)))))
    (subseq decoded 4 (+ 5 (position unit *date-granularity-units*)))))

(defmethod date/=
    ((date-1 standard-date) (date-2 standard-date) &key (unit :microsecond))
  (not (date= date-1 date-2 :unit unit)))

(defmethod date<=
    ((date-1 standard-date) (date-2 standard-date) &key (unit :microsecond))
  (or (date= date-1 date-2 :unit unit) (local-time:timestamp< date-1 date-2)))

(defmethod date>=
    ((date-1 standard-date) (date-2 standard-date) &key (unit :microsecond))
  (or (date= date-1 date-2 :unit unit) (local-time:timestamp> date-1 date-2)))

(defmethod date>
    ((date-1 standard-date) (date-2 standard-date) &key (unit :microsecond))
  (not (date<= date-1 date-2 :unit unit)))

(defmethod date<
    ((date-1 standard-date) (date-2 standard-date) &key (unit :microsecond))
  (not (date>= date-1 date-2 :unit unit)))

(defmethod date-min (date &rest other-dates)
  (apply #'local-time:timestamp-minimum date other-dates))

(defmethod date-max (date &rest other-dates)
  (apply #'local-time:timestamp-maximum date other-dates))

(defmethod now-using-class ((class (eql (find-class 'standard-date))))
  (change-class (local-time:now) 'standard-date))

(defun now () (now-using-class (find-class 'standard-date)))
