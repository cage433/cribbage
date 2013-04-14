(load "utils")
(defvar *test-name* nil)


(defmacro deftest (name parameters &body body)
  "Define a test function. Within a test function we can call
   other test functions or use `check' to run individual test
   cases."
  `(defun ,name ,parameters
    (let ((*test-name* (append *test-name* (list ',name))))
      ,@body)))

(defun check-approximately-equals (value expected-value tolerance &key text (double-format "~,3f"))
  (or (< (abs (- value expected-value)) tolerance)
	  (progn
		(when *test-name*
		  (format t "Test failed: ~a~%" *test-name*))
		(when text
		  (format t "~a~%" text))
		(format t "Expected ~?, but got ~?~%" double-format (list expected-value) double-format (list value))
		nil)))

(defun check-within-%age-tolerance (value expected-value tolerance &key text (double-format "~,3f"))
  (check-approximately-equals value expected-value
							  (* tolerance (max (abs value) (abs expected-value)))
							  :text text :double-format double-format))


(defmacro combine-results (&body forms)
  "Combine the results (as booleans) of evaluating `forms' in order."
  (with-gensyms (result)
	`(let ((,result t))
	  ,@(loop for f in forms collect `(unless ,f (setf ,result nil)))
	  ,result)))
(defun report-result (result form)
  "Report the results of a single test case. Called by `check'."
  (unless result
;; 	  (format t "... passed: ~a~%" *test-name*)
	  (format t "... failed: ~a ~a~%" *test-name* form))
  result)
(defmacro check (&body forms)
  "Run each expression in `forms' as a test case."
  `(combine-results
    ,@(loop for f in forms collect `(report-result ,f ',f))))


