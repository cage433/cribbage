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
  (load-and-compile-if-necessary "play/game"))

(defun compile-and-run-tests()
  (declare #+sbcl(sb-ext:muffle-conditions style-warning))
  (and
    (load-and-compile-source)
    (cage433-lisp-utils::run-tests)
    (test-cards) 
    (test-game)))

(defun compile-and-run-tests-and-exit()
  (declare #+sbcl(sb-ext:muffle-conditions style-warning))
  (multiple-value-bind (success error-condition)
    (ignore-errors
      (compile-and-run-tests))
    (if success
      (progn
        (format t (colored-text "Tests passed~%" :green))
        (sb-ext:exit :code 0))
      (progn
        (if error-condition
          (format t (colored-text "~a~%" :red) error-condition)
          (format t (colored-text "Tests failed ~%" :red)))
        (sb-ext:exit :code 1)))))

    
