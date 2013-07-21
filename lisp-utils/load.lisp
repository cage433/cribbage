(proclaim '(optimize (debug 3)))
(load "utils0")
(load "unit-testing")
(load "utils")
(load "readers")

(defun run-lisp-utilities-tests (&key (exit-on-termination t))
  (let ((result 
          (combine-results 
            (test-span)
            (test-anon-functions)
            (test-curry)
            (test-compose)
            (test===))))
    (if exit-on-termination
      (sb-ext:exit :code (if result 0 1))
      result)))

(defun run-tests (&key (exit-on-termination t))
  (run-lisp-utilities-tests :exit-on-termination exit-on-termination))
