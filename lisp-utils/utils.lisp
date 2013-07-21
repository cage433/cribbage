(in-package :cage433-lisp-utils)

(defun foldr (fn init args)
	(reduce fn args :initial-value init :from-end t))
(defun foldl (fn init args)
	(reduce fn args :initial-value init :from-end nil))

(defmacro dbind (&rest stuff)
	`(destructuring-bind ,@stuff))

(defmacro mbind (&rest stuff)
	`(multiple-values-bind  ,@stuff))

(defmacro until (pred &body body)
	(let ((result (gensym)))
		`(do ((,result))
				 (,pred ,result)
				 (setq ,result (progn ,@body)))))

(defun span (pred xs)
	"Returns the longest left sublist for which pred is true, consed with the remainder"
	(labels ((rec (trues rest)
							(if (and (consp rest) (funcall pred (car rest)))
								(rec (cons (car rest) trues) (cdr rest))
								(cons (reverse trues) rest))))
		(rec '() xs)))

(defun for-all (pred list)
  (cond ((null list) t)
        ((funcall pred (car list)) (for-all pred (cdr list)))
        (t nil)))

(deftest test-span()
	(check (equalp '((1 2 3) 4 5 6) (span (lambda (i) (< i 3.5)) '(1 2 3 4 5 6)))))

