(defun ascii-color (color)
  (ecase color
    (:red 31)
    (:green 32)
    (:blue 34)))

(defun colored-text (text color &key bold)
  (format nil "~c[~a~:[~;;1~]m~a~c[0m"
          #\Esc
          (ascii-color color)
          bold
          text #\Esc))

(defun run-test (doc-string test-and-forms)
  (if (null test-and-forms)
    (progn 
      (format t "  ~A~%" (colored-text doc-string :green))
      t)
    (destructuring-bind (test form) (car test-and-forms)
      (if test
        (run-test doc-string (cdr test-and-forms))
        (progn 
          (format t "  ~A~%" (colored-text doc-string :red))
          (format t "    Failing form~%     ~A~%" form)
          nil)))))

(defmacro spec (doc-string &body forms)
  "Run each expression in 'forms' as a test case."
  `(run-test ,doc-string 
    (list ,@(mapcar (lambda (f) `(list ,f ',f)) forms))))

(defmacro info (feature &rest specs)
  `(progn 
    (format t "~A~%" (colored-text ,feature :blue))
    (and ,@specs)))





