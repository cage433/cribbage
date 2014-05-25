(in-package :common-lisp-user)

(let ((*default-pathname-defaults* (merge-pathnames #p"lisp-utils/" *default-pathname-defaults*)))
  (load "load"))

(cage433-lisp-utils:load-and-compile-if-necessary "cl-utilities/package")
(cage433-lisp-utils:load-and-compile-if-necessary "cl-match_0.1.8/load")
(cage433-lisp-utils:load-and-compile-if-necessary "cl-utilities/split-sequence")

(cage433-lisp-utils:load-and-compile-if-necessary "package")

(in-package :cage433-cribbage)

(defun load-and-compile-source()
  (format t "Loading source ~%")
  (and
    (load-and-compile-if-necessary "utils")
    (load-and-compile-if-necessary "cards")
    (load-and-compile-if-necessary "crib")
    (load-and-compile-if-necessary "play/game-state")
    (load-and-compile-if-necessary "play/discard")
    (load-and-compile-if-necessary "play/play")
    ))

(defun compile-and-run-tests()
  (declare #+sbcl(sb-ext:muffle-conditions style-warning))
  (if (load-and-compile-source)
    (and
      (cage433-lisp-utils::run-tests)
      (test-cards) 
      (test-game-state)
      (test-discard-cards)
      (test-play-single-round)
      (test-play-rounds)
      )))

(defun load-scratch()
  (declare #+sbcl(sb-ext:muffle-conditions style-warning))
  (load-and-compile-if-necessary "foo"))

(defun run-ci-function(ci-fun)
  (declare #+sbcl(sb-ext:muffle-conditions style-warning))
  (multiple-value-bind (success error-condition)
    (ignore-errors
      (funcall ci-fun)
      )
    (if success
      (progn
        (format t (colored-text "Tests passed~%" :green))
        (sb-ext:exit :code 0))
      (progn
        (if error-condition
          (format t (colored-text "~a~%" :red) error-condition)
          (format t (colored-text "Tests failed ~%" :red)))
        (sb-ext:exit :code 1)))))

    
(defun ci()
    (run-ci-function #'compile-and-run-tests)
;    (progn
;      (load-and-compile-source)
;      (run-ci-function #'load-scratch))
    )
