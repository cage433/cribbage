(defun run-tests (&key (exit-on-termination t))
  (let ((result (for-all #'funcall +test-names+)))
    (if exit-on-termination
      (sb-ext:exit :code (if result 0 1))
      result)))
