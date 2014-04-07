(load "package.lisp")

(in-package :cage433-lisp-utils)


(proclaim '(optimize (debug 3)))

(defun load-and-compile-if-necessary (name)
  (labels ((file-name (ext) (concatenate 'string name ext))
           (file-date (ext) 
              (and (probe-file (file-name ext))
                  (file-write-date (file-name ext))))
           (compile-source() 
            (format *standard-output* "Compiling ~A~%" name)
            (multiple-value-bind (output-file warnings-p failure-p) 
                                 (compile-file name :verbose nil :print nil)
              (let ((success (and (null warnings-p) (null failure-p))))
                (unless success
                  (delete-file output-file))
                success)))
           (fasl-out-of-date()
              (let ((src-time (file-date ".lisp"))
                        (fasl-time (file-date ".fasl")))
                    (or (null fasl-time) 
                        (> src-time fasl-time))))
           (load-fasl() 
             (load (file-name ".fasl") :verbose nil))
           )
    (if (fasl-out-of-date)
      (and (compile-source) (load-fasl))
      (progn (load-fasl) t))))

(and
  (load-and-compile-if-necessary "package")
  (load-and-compile-if-necessary "utils0")
  (load-and-compile-if-necessary "unit-testing")
  (load-and-compile-if-necessary "utils")
  (load-and-compile-if-necessary "readers")
  (load-and-compile-if-necessary "readers-tests"))

(defun run-tests()
  (and
    (test-span)
    (test-take-while)
    (test-anon-functions)
    (test-curry)
    (test-compose)
    (test-def-rstruct)
;    (test===)
    ))

