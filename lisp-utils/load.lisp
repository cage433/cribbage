(load "package.lisp")

(in-package :cage433-lisp-utils)


(proclaim '(optimize (debug 3)))

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
          (compile-file name :verbose nil :print nil)))
      (load (file-name ".fasl") :verbose nil))))


(load-and-compile-if-necessary "package")
(load-and-compile-if-necessary "utils0")
(load-and-compile-if-necessary "unit-testing")
(load-and-compile-if-necessary "utils")
(load-and-compile-if-necessary "readers")
(load-and-compile-if-necessary "readers-tests")

(defun run-tests (&key (exit-on-termination t))
  (let ((result 
          (combine-results 
            (test-span)
            (test-take-while)
            (test-anon-functions)
            (test-curry)
            (test-compose)
            (test-def-rstruct)
            (test===))))
    (if exit-on-termination
      (sb-ext:exit :code (if result 0 1))
      result)))
