;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; RAPTOR-LAUNCHER
;;;; © Michał "phoe" Herda 2017
;;;; classes.lisp

(in-package :gateway.client.ui)
(in-readtable :qtools)

(defparameter *load-regex* "(\\d+)/(\\d+)/(\\d+) (\\d+):(\\d+):(\\d+) (.*)")

(defun load-file (path expected-personas)
  (flet ((scan (x) (nth-value 1 (cl-ppcre:scan-to-strings *load-regex* x))))
    (let* ((file (read-file-into-string path))
           (splits (split-sequence:split-sequence #\Newline file
                                                  :remove-empty-subseqs t))
           (scanned (mapcar #'scan splits))
           (scanned (mapcar (rcurry #'coerce 'list) scanned))
           (scanned (delete nil scanned)))
      (loop for (month day year h m s contents) in scanned
            for args = (mapcar #'parse-integer (list s m h day month))
            for fixed-year = (parse-integer year)
            for fixed-year2 = (+ fixed-year (if (< 80 fixed-year) 1900 2000))
            for timestamp = (apply #'local-time:encode-timestamp 0
                                   (append args (list fixed-year2)))
            for persona = (detect-persona contents expected-personas)
            for processed-contents = (process-contents contents persona)
            collect (make-instance 'post :contents processed-contents
                                         :timestamp timestamp
                                         :persona persona)))))

(defun detect-persona (contents personas)
  (loop for persona in personas
        for regex = (format nil "^(\\* |)~A" persona)
        for match = (cl-ppcre:scan regex contents)
        when match return persona))

(defun process-contents (contents &optional persona)
  (multiple-value-bind (foundp strings)
      (cl-ppcre:scan-to-strings "^(\\* |)(.*)" contents)
    (declare (ignore foundp))
    (let ((third-person-p (string/= (aref strings 0) ""))
          (post-contents (aref strings 1)))
      (if third-person-p
          post-contents
          (let* ((format-control (if persona "^~A: (.*)" "^.*?: (.*)"))
                 (regex (format nil format-control persona))
                 (strings (nth-value 1 (cl-ppcre:scan-to-strings
                                        regex post-contents))))
            (aref strings 0))))))
