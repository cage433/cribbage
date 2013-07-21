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

(deftest test-anon-functions()
	(check (= 9 (#_(* _ _) 3)))
	(check (= 8 (#2_(* _1 _2) 2 4)))
	(check (= 32 (#2_(* _1 _2 _2) 2 4)))
	(labels ((plus (x y) (+ x y)))
		 (check (= 6 (#2_(plus _1 _2) 2 4))))
	(let ((_plus #'+))
		(check (= 6 (#2_ (funcall _plus _1 _2) 2 4)))))




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

(deftest test-curry()
	
	(check (= 6 ([+ 4] 2)))

	(check (= 6 ([#'+ 4] 2)))

	(labels ((plus (x y) (+ x y)))
		(check (= 6 ([plus 4] 2))))

	(flet ((minus (x y) (- x y)))
		(check (= 6 ([minus 10] 4))))

	(check (= 6 ([(lambda (x y) (+ x y)) 3] 3)))

	(let ((plus (lambda (x y) (+ x y))))
		(check (= 6 ([ _plus 3] 3))))

	(labels ((foo() (lambda (x y) (+ x y))))
		(check (= 6 ([(foo) 4] 2))))			
	)

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

(deftest test-compose()
	
	(check (= 6 ( { #'1+ + } 2 3)))

	(check (= 6 ( { 1+ + } 2 3)))

	(check (= 7 ( { 1+ 1+ + } 2 3)))

	(labels ((one-plus (n) (1+ n)))
		(check (= 6 ( { one-plus + } 2 3))))

	(check (= 6 ( { (lambda (n) (1+ n)) + } 3 2)))

	(let ((one-plus (lambda (n) (1+ n))))
		(check (= 6 ( { _one-plus + } 2 3))))

	(labels ((foo() (lambda (n) (1+ n))))
		(check (= 6 ( { (foo) + } 2 3))))
)

