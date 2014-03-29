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

(deftest test-span()
	(check (equalp '((1 2 3) 4 5 6) (span (lambda (i) (< i 3.5)) '(1 2 3 4 5 6)))))

(defun for-all (pred list)
  (cond ((null list) t)
        ((funcall pred (car list)) (for-all pred (cdr list)))
        (t nil)))


(defun take-while (pred seq)
  (labels ((recurse (seq acc)
                (if (or (null seq) (not (funcall pred (car seq))))
                  (reverse acc)
                  (recurse (cdr seq) (cons (car seq) acc)))))
    (recurse seq nil)))

(deftest test-take-while()
  (check (=== '(1 2 3) (take-while (lambda (i) (< i 4)) '(1 2 3 4 0)) 
              :test #'equal))
  (check (=== nil (take-while (lambda (i) (> i 2)) '(1 2 3 4 5)) 
              :test #'equal))
  )


(defun concat-syms (&rest syms)
    (intern (apply #'concatenate 'string (mapcar #'symbol-name syms))))

(defmacro def-rstruct (name &rest its-slots)
  `(progn
    (defstruct ,name ,@its-slots)
    (defmacro ,(concat-syms 'with- name) (thing-name &body body)
                `(with-slots (,@',its-slots) ,thing-name
        ,@body))
    (defmacro ,(concat-syms 'with-named- name) (thing-name &body body)
      `(with-slots ,(mapcar (lambda (s) (list (concat-syms thing-name '/ s) s)) ',its-slots) ,thing-name
        ,@body))))
