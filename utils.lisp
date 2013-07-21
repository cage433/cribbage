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


(defun clear-screen()
  (format t "~c~a~c~a" #\Escape "[H" #\Escape "[2J"))

(defun in-colour (colour str)
  (format t "~c~a~a~c~a" #\Escape colour str #\Escape "[0m"))

(defun in-red (str)
  (in-colour "[1;31m" str))
(defun in-green (str)
  (in-colour "[1;32m" str))
(defun in-blue (str)
  (in-colour "[1;34m" str))
 
 

