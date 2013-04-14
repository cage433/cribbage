(defun load-and-compile-if-necessary (name)
  (labels ((file-date (ext) 
             (let ((file-name (concatenate 'string name ext)))
               (and (probe-file file-name)
                    (file-write-date file-name)))))
    (let ((src-time (file-date ".lisp"))
          (fasl-time (file-date ".fasl")))
      (if (or (null fasl-time) 
              (> src-time fasl-time))
        (progn
          (format *standard-output* "Compiling ~a~%" name)
          (compile-file name))
        (format *standard-output* "No need to compile ~a~%" name))
      (load name))))

(load-and-compile-if-necessary "utils")
(load-and-compile-if-necessary "unit")
(load-and-compile-if-necessary "cards")

(defun run-tests (&key (exit-on-termination t))
  (let ((result 
          (combine-results (test-cards))))
    (if exit-on-termination
      (sb-ext:exit :code (if result 0 1))
      result)))
