(let ((*default-pathname-defaults* (merge-pathnames #p"lisp-utils/" *default-pathname-defaults*)))
  (load "load"))

(load-and-compile-if-necessary "cl-utilities/package")
(load-and-compile-if-necessary "cl-utilities/split-sequence")
(load-and-compile-if-necessary "utils")
(load-and-compile-if-necessary "cards")
(load-and-compile-if-necessary "game")

(defun run-cribbage-tests(&key (exit-on-termination t))
  (let ((result 
          (combine-results (test-cards) 
                           (test-game)
                           (test-game2)
                           (run-lisp-utilities-tests :exit-on-termination nil)
                           )))
    (if exit-on-termination
      (sb-ext:exit :code (if result 0 1))
      result)))

(defun run-tests (&key (exit-on-termination t))
  (run-cribbage-tests :exit-on-termination exit-on-termination))
