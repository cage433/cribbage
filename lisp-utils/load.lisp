(load "package.lisp")

(in-package :cage433-lisp-utils)
(require :cage433-ci)

(proclaim '(optimize (debug 3)))

(and
  (cage433-ci:load-and-compile-if-necessary "package")
  (cage433-ci:load-and-compile-if-necessary "utils0")
  (cage433-ci:load-and-compile-if-necessary "unit-testing")
  (cage433-ci:load-and-compile-if-necessary "utils")
  (cage433-ci:load-and-compile-if-necessary "utils-tests")
  (cage433-ci:load-and-compile-if-necessary "readers")
  (cage433-ci:load-and-compile-if-necessary "readers-tests")
  (cage433-ci:load-and-compile-if-necessary "anaphors")
  )

(defun run-tests()
  (and
    (test-span)
    (test-take-while)
    (test-anon-functions)
    (test-curry)
    (test-compose)
    (test-def-rstruct)
    (test-cross-product)
;    (test===)
    ))

