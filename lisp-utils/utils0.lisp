(in-package :cage433-lisp-utils)

;; From Graham's On Lisp
(defmacro with-gensyms (syms &body body)
  `(let ,(mapcar #'(lambda (s) `(,s (gensym))) syms) ,@body))
