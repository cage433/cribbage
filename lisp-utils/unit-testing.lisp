(in-package :cage433-lisp-utils)

(defvar *test-name* nil)

(defun === (expected actual &key (test #'eql))
  (if (funcall test expected actual)
    t
    (progn
      (format t "Expected ~a but got ~a~%" expected actual))))

(defmacro deftest (name parameters &body body)
  "Define a test function. Within a test function we can call
   other test functions or use 'check' to run individual test
   cases."
  (with-gensyms (result)
    `(defun ,name ,parameters
      (let ((,result (let ((*test-name* (append *test-name* (list ',name))))
            ,@body)))
        (if ,result
          (format t ".")
          (format t "~%Failed ~%"))
        ,result))))

(defmacro check (&body forms)
  "Run each expression in 'forms' as a test case."
  `(combine-results
     ,@(loop for f in forms collect `(report-result ,f ',f))))

(defmacro combine-results (&body forms)
  "Combine the results (as booleans) of evaluating 'forms' in order."
  (with-gensyms (result)
    `(let ((,result t))
      ,@(loop for f in forms collect `(unless ,f (setf ,result nil)))
      ,result)))

(defun report-result (result form)
  "Report the results of a single test case. Called by 'check'."
  (if (not result)
      (format t "~:[FAIL~;pass~] ... ~a: ~a~%" result *test-name* form))
  result)

(deftest test=== ()
  (check (=== 3 3) 
         (=== "4" "4" :test #'equal)
         (=== 1 1)))


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
