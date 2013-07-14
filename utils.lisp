(defun mappend (fn &rest lsts)
  "maps elements in list and finally appends all resulted lists."
  (apply #'append (apply #'mapcar fn lsts)))

(defmacro with-gensyms (syms &body body)
  "From Paul Graham's 'On Lisp'"
  `(let ,(mapcar #'(lambda (s)
                     `(,s (gensym)))
                 syms)
     ,@body))

(defmacro while (expr &body body)
  (with-gensyms (x)
        `(do ((,x ,expr ,expr))
                ((not ,x))
             ,@body)))
(defmacro dbind (&body body)
  `(destructuring-bind ,@body))
