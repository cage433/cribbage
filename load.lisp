(in-package :common-lisp-user)
(require :split-sequence)
(require :cl-match)
(require :cage433-ci)

(let ((*default-pathname-defaults* (merge-pathnames #p"lisp-utils/" *default-pathname-defaults*)))
  (load "load"))

(cage433-ci:load-and-compile-if-necessary "package")

(in-package :cage433-cribbage)

(defun load-and-compile-source()
  (format t "Loading source ~%")
  (and
    (cage433-ci:load-and-compile-if-necessary "utils")
    (cage433-ci:load-and-compile-if-necessary "cards")
    (cage433-ci:load-and-compile-if-necessary "play/game-state")
    (cage433-ci:load-and-compile-if-necessary "play/discard")
    (cage433-ci:load-and-compile-if-necessary "play/play")
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

    
(in-package :common-lisp-user)
(defun ci()
  (cage433-cribbage::run-ci-function #'cage433-cribbage::compile-and-run-tests)
  )
