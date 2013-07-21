(in-package :cage433-lisp-utils)

(deftest test-anon-functions()
	(check (= 9 (#_(* _ _) 3)))
	(check (= 8 (#2_(* _1 _2) 2 4)))
	(check (= 32 (#2_(* _1 _2 _2) 2 4)))
	(labels ((plus (x y) (+ x y)))
		 (check (= 6 (#2_(plus _1 _2) 2 4))))
	(let ((_plus #'+))
		(check (= 6 (#2_ (funcall _plus _1 _2) 2 4)))))


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

