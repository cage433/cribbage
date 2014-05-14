(in-package :cage433-cribbage)

(defun mappend (fn &rest lsts)
  "maps elements in list and finally appends all resulted lists."
  (apply #'append (apply #'mapcar fn lsts)))

(defmacro while (expr &body body)
  (with-gensyms (x)
        `(do ((,x ,expr ,expr))
                ((not ,x))
             ,@body)))


(defun clear-screen()
  (format t "~c~a~c~a" #\Escape "[H" #\Escape "[2J"))

(defun newline()
  (format t "~%"))

(defun in-colour (colour str)
  (format t "~c~a~a~c~a" #\Escape colour str #\Escape "[0m"))

(defun in-red (str)
  (in-colour "[1;31m" str))
(defun in-green (str)
  (in-colour "[1;32m" str))
(defun in-blue (str)
  (in-colour "[1;34m" str))
 
 

