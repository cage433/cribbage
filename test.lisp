(defun load-and-compile-if-necessary (name)
  (labels ((file-name (ext) (concatenate 'string name ext))
           (file-date (ext) 
              (and (probe-file (file-name ext))
                  (file-write-date (file-name ext)))))
    (let ((src-time (file-date ".lisp"))
          (fasl-time (file-date ".fasl")))
      (if (or (null fasl-time) 
              (> src-time fasl-time))
        (progn
          (format *standard-output* "Compiling ~A~%" name)
          (compile-file name)))
      (load (file-name ".fasl") :verbose t))))

(load-and-compile-if-necessary "cl-utilities/package")
(load-and-compile-if-necessary "cl-utilities/split-sequence")
(load-and-compile-if-necessary "utils")
(load-and-compile-if-necessary "unit")
(load-and-compile-if-necessary "cards")

(defun run-tests (&key (exit-on-termination t))
  (let ((result 
          (combine-results (test-cards))))
    (if exit-on-termination
      (sb-ext:exit :code (if result 0 1))
      result)))
