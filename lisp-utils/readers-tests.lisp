(in-package :cage433-lisp-utils)

(deftest test-anon-functions()
  (combine-results
    (check (= 9 (#_(* _ _) 3)))
    (check (= 27 (#_(* 3 (+ _ 2)) 7)))
    (check (= 8 (#2_(* _1 _2) 2 4)))
    (check (= 32 (#2_(* _1 _2 _2) 2 4)))
    (labels ((plus (x y) (+ x y)))
      (check (= 6 (#2_(plus _1 _2) 2 4))))
    (let ((_plus #'+))
      (check (= 6 (#2_ (funcall _plus _1 _2) 2 4))))))


(deftest test-curry()
  (combine-results	
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
      (check (= 6 ([(foo) 4] 2))))))

(deftest test-compose()
  (combine-results	
    (check (= 6 ( { #'1+ + } 2 3)))

    (check (= 6 ( { 1+ + } 2 3)))

    (check (= 7 ( { 1+ 1+ + } 2 3)))

    (labels ((one-plus (n) (1+ n)))
      (check (= 6 ( { one-plus + } 2 3))))

    (check (= 6 ( { (lambda (n) (1+ n)) + } 3 2)))

    (let ((one-plus (lambda (n) (1+ n))))
      (check (= 6 ( { _one-plus + } 2 3))))

    (labels ((foo() (lambda (n) (1+ n))))
      (check (= 6 ( { (foo) + } 2 3))))))

(def-rstruct test-struct x y)
(deftest test-def-rstruct()
  (combine-results
    (let ((a (make-test-struct :x 1 :y 2))
          (b (make-test-struct :x 3 :y 4)))
      (with-test-struct a (check (= 1 x)))
      (with-test-struct b (check (= 3 x)))
      (with-named-test-struct a
        (check (= 1 a/x)))
      (with-named-test-struct a
        (with-named-test-struct b
              (check (= 1 a/x))
              (check (= 3 b/x)))
      ))))
