(load "load")

(defun run-tests (&key (exit-on-termination t))
  (cage433-lisp-utils::run-tests :exit-on-termination exit-on-termination))