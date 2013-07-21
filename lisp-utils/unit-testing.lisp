(defvar *test-name* nil)

(defparameter +test-names+ nil)

(defun === (expected actual &key (test #'eql))
  (if (funcall test expected actual)
    t
    (progn
      (format t "Expected ~a but got ~a~%" expected actual))))

(defmacro deftest (name parameters &body body)
  "Define a test function. Within a test function we can call
   other test functions or use 'check' to run individual test
   cases."
   (setf +test-names+ (cons name +test-names+))
  `(defun ,name ,parameters
     (let ((result (let ((*test-name* (append *test-name* (list ',name))))
		      ,@body)))
       (if result
	   (format t "Passed ~%")
	   (format t "Failed ~%"))
       result)))

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

