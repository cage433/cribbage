(in-package :common-lisp-user)

(let ((*default-pathname-defaults* (merge-pathnames #p"lisp-utils/" *default-pathname-defaults*)))
  (load "load"))

(cage433-lisp-utils:load-and-compile-if-necessary "cl-utilities/package")
(cage433-lisp-utils:load-and-compile-if-necessary "cl-utilities/split-sequence")

(cage433-lisp-utils:load-and-compile-if-necessary "package")

(in-package :cage433-cribbage)

(defun load-and-compile-source()
  (format t "Loading source ~%")
  (load-and-compile-if-necessary "utils")
  (load-and-compile-if-necessary "cards")
  ;(load-and-compile-if-necessary "game")
  (load-and-compile-if-necessary "game2"))

(defun compile-and-run-tests()
  (declare #+sbcl(sb-ext:muffle-conditions style-warning))
  (ignore-errors
  (progn
      (load-and-compile-source)
      (let ((result 
              (combine-results 
                (test-cards) 
                (test-game)
                ;(test-game2)
                (cage433-lisp-utils::run-tests :exit-on-termination nil)
                )))
        (sb-ext:exit :code (if result 0 1)))))
  (format t "Badness happened~%~%")
  (sb-ext:exit :code 1))

    
