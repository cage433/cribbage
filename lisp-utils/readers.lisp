(in-package :cage433-lisp-utils)

(defun mkstr (&rest args)
	"Concatenates a list of arguments into a string"
	(with-output-to-string (s)
		(dolist (a args) (princ a s))))

(defun symb (&rest args)
	"Creates a new symbol using an arbitrary list of terms to generate the name."
	(values (intern (apply #'mkstr args))))


(defun add-anon-function-syntax()
	"Used to allow cleaner syntax for representing functions in curry and compose reader macros.
	Symbols in a function position (any term for compose, the first only for curry) will be prepended
	with #' unless they begin with an underscore. This is intended for variables with function values.
	Lambda expressions are unchanged, so for example"

	(set-dispatch-macro-character
		#\# #\_ (lambda (stream sub-char numarg)
							(declare (ignore sub-char))
							(unless numarg (setq numarg 1))
				(let ((syms (if (> numarg 1)
												 (loop for i from 1 to numarg
															 collect (symb '_ i))
												 (list (symb '_)))))
								`(lambda ,syms
									 (declare (ignorable ,@syms))
									 ,(read stream t nil t))))))

(add-anon-function-syntax)





(defun to-function-symbol (function-exp)
	"Used to allow cleaner syntax for representing functions in curry and compose reader macros.
	Symbols in a function position (any term for compose, the first only for curry) will be prepended
	with #' unless they begin with an underscore. This is intended for variables with function values.
	Lambda expressions are unchanged, so for example"
	(if (symbolp function-exp)
				(if (string= "_" (symbol-name function-exp) :start2 0 :end2 1)
					(symb (subseq (symbol-name function-exp) 1))
					`#',function-exp)
				function-exp))


(defun add-curry-syntax ()
"Reader macro for currying functions. Works like, e.g.
			(macroexpand '[* 10 15]) =>
							#'(LAMBDA (&REST MORE-ARGS) (APPLY #'* (APPEND (LIST 10 15) MORE-ARGS)))
"
	(set-macro-character #\] (get-macro-character #\)))
	(set-macro-character
				#\[
				(lambda (stream char)
					(declare (ignore char))
					(dbind (fn . args) (read-delimited-list #\] stream t)
												 (let ((genargs) (allargs))
													 (dolist (s args)
																 (when (and (symbolp s) (string= "_" (symbol-name s)))
																	 (setq s (gensym))
																	 (push s genargs))
																 (push s allargs))
													 `(lambda (,@(reverse genargs) &rest more-args)
																	(apply ,(to-function-symbol fn) (append (list ,@(reverse allargs)) more-args))))))))


(add-curry-syntax)


(defun add-compose-syntax ()
	"Reader macro for composing functions. E.g.

	* (macroexpand '(let ((foo #'1+ )) {cos _foo (lambda (x y) (+ x y))}))
	(LET ((FOO #'1+))
			 (LAMBDA (&REST #:G2729)
							 (FUNCALL #'COS (FUNCALL FOO (APPLY (LAMBDA (X Y) (+ X Y)) #:G2729)))))
	"
	(set-macro-character #\} (get-macro-character #\)))
	(set-macro-character
		#\{
		(lambda (stream char)
			(declare (ignore char))
			(let ((fns  (mapcar #'to-function-symbol (read-delimited-list #\} stream t))))
				(if fns
					(let ((args (gensym)))
						`(lambda (&rest ,args) ,(foldr (lambda (x y) `(funcall ,x ,y))
																					 `(apply ,(car (last fns)) ,args)
																					 (butlast fns))))
					#'identity)

				))))
(add-compose-syntax)

