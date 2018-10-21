;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; GATEWAY
;;;; © Michał "phoe" Herda 2016-2018
;;;; engine/t/standard-date.lisp

(in-package #:gateway.engine/test)
(in-readtable protest/parachute)

;;; Test suite

(define-test-case standard-date
    (:documentation "Test suite for STANDARD-DATE."
     :tags (:gateway :engine :object :standard-date :suite)))

(define-test standard-date
  :parent engine)

(define-test-case standard-date-unit
    (:documentation "Unit tests for STANDARD-DATE."
     :tags (:unit :date :standard-date :unit)))

(define-test standard-date-unit
  :parent standard-date
  (flet ((make (d s n) (make-instance 'standard-date :day d :sec s :nsec n)))
    (let* ((d-orig (make 0 0 0)) (d-same (make 0 0 0)) (d-usec (make 0 0 1000))
           (d-sec (make 0 1 0)) (d-min (make 0 60 0)) (d-hour (make 0 3600 0))
           (d-day (make 1 0 0)) (d-month (make 31 0 0)) (d-year (make 365 0 0))
           (vars (list d-orig d-usec d-sec d-min d-hour d-day d-year)))
      (true (listp (object-data d-orig)))
      (true (date= d-orig (data-object-using-class
                           (find-class 'standard-date)
                           (object-data d-orig))))
      (true (integerp (date-timestamp d-orig)))
      (true (date= d-orig
                   (timestamp-date-using-class (find-class 'standard-date)
                                               (date-timestamp d-orig))))
      (true (integerp (date-ustimestamp d-orig)))
      (true (date= d-orig
                   (ustimestamp-date-using-class (find-class 'standard-date)
                                                 (date-ustimestamp d-orig))))
      (true (date= d-orig d-same))
      (true (date= d-orig d-same :unit :microsecond))
      (true (date= d-orig d-usec :unit :second))
      (true (date= d-orig d-sec :unit :minute))
      (true (date= d-orig d-min :unit :hour))
      (true (date= d-orig d-hour :unit :day))
      (true (date= d-orig d-day :unit :month))
      (true (date= d-orig d-month :unit :year))
      (true (date/= d-orig d-usec))
      (true (date/= d-orig d-sec))
      (true (date/= d-orig d-min))
      (true (date/= d-orig d-hour))
      (true (date/= d-orig d-day))
      (true (date/= d-orig d-month))
      (true (date/= d-orig d-year))
      (true (date/= d-orig d-sec :unit :microsecond))
      (true (date/= d-orig d-min :unit :second))
      (true (date/= d-orig d-hour :unit :minute))
      (true (date/= d-orig d-day :unit :hour))
      (true (date/= d-orig d-month :unit :day))
      (true (date/= d-orig d-year :unit :month))
      (true (date< d-orig d-usec))
      (true (date< d-usec d-sec))
      (true (date< d-sec d-min))
      (true (date< d-min d-hour))
      (true (date< d-hour d-day))
      (true (date< d-day d-month))
      (true (date< d-month d-year))
      (true (date> d-usec d-orig))
      (true (date> d-sec d-usec))
      (true (date> d-min d-sec))
      (true (date> d-hour d-min))
      (true (date> d-day d-hour))
      (true (date> d-month d-day))
      (true (date> d-year d-month))
      (true (eq d-orig (apply #'date-min vars)))
      (true (eq d-year (apply #'date-max vars)))
      (true (eql (class-of (now-using-class (find-class 'standard-date)))
                 (find-class 'standard-date)))
      (true (eql (class-of (now)) (find-class 'standard-date))))))
