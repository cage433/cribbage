(in-package :cage433-lisp-utils)

;;; Taken from Paul Graham's 'On Lisp'
;;; Chapter 18 - Anaphoric Macros

(defmacro aif (test-form then-form &optional else-form)
        `(let ((it ,test-form))
             (if it ,then-form ,else-form)))

 (defmacro awhen (test-form &body body)
        `(aif ,test-form
                 (progn ,@body)))

 (defmacro awhile (expr &body body)
        `(do ((it ,expr ,expr))
                ((not it))
             ,@body))

 (defmacro aand (&rest args)
        (cond ((null args) t)
                 ((null (cdr args)) (car args))
                 (t `(aif , (car args) (aand ,@(cdr args))))))

 (defmacro acond (&rest clauses)
        (if (null clauses)
              nil
              (let ((cl1 (car clauses))
                       (sym (gensym)))
                 `(let ((,sym , (car cl1)))
                      (if ,sym
                            (let ((it ,sym)) ,@(cdr cl1))
                            (acond ,@(cdr clauses)))))))

