(in-package :common-lisp-user)
(require :split-sequence)
(require :cl-match)
(require :cage433-ci)
(require :cage433-lisp-utils)

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
      (test-cards) 
      (test-game-state)
      (test-discard-cards)
      (test-play-single-round)
      (test-play-rounds)
      )))

    
(in-package :common-lisp-user)
(defun ci()
  (cage433-ci:run-ci-function #'cage433-cribbage::compile-and-run-tests)
  )
