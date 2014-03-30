(in-package :cage433-lisp-utils)

(defun test-anon-functions()
  (info "anon function read macro"
    (spec "Works for sundry calls"
      (= 9 (#_(* _ _) 3))
      (= 27 (#_(* 3 (+ _ 2)) 7))
      (= 8 (#2_(* _1 _2) 2 4))
      (= 32 (#2_(* _1 _2 _2) 2 4)))
    (spec "Works with labels"
      (labels ((plus (x y) (+ x y)))
        (= 6 (#2_(plus _1 _2) 2 4))))
    (spec "Works with sharp signed function"
      (let ((_plus #'+))
        (= 6 (#2_ (funcall _plus _1 _2) 2 4))))))


(defun test-curry()
  (info "curry read macro"
    (spec "with '+' special form"
      (= 6 ([+ 4] 2)))

    (spec "with #'+"
      (= 6 ([#'+ 4] 2)))

    (spec "labels"
      (labels ((plus (x y) (+ x y)))
        (= 6 ([plus 4] 2))))

    (spec "flet"
      (flet ((minus (x y) (- x y)))
        (= 6 ([minus 10] 4))))

    (spec "lambda"
      (= 6 ([(lambda (x y) (+ x y)) 3] 3)))

    (spec "function variable works with _ syntax"
      (let ((plus (lambda (x y) (+ x y))))
        (= 6 ([ _plus 3] 3))))

    (spec "labels with lambda"
      (labels ((foo() (lambda (x y) (+ x y))))
        (check (= 6 ([(foo) 4] 2)))))))

(defun test-compose()
  (info	"compose read macro"
    (spec "With sharp sign"
      (= 6 ( { #'1+ + } 2 3)))
    
    (spec "With special forms"
      (= 6 ( { 1+ + } 2 3))
      (= 7 ( { 1+ 1+ + } 2 3)))

    (spec "With lambda"
      (= 6 ( { (lambda (n) (1+ n)) + } 3 2)))

    (spec "With labels"
      (labels ((one-plus (n) (1+ n)))
        (= 6 ( { one-plus + } 2 3))))

    (spec "With function variable"
      (let ((one-plus (lambda (n) (1+ n))))
        (= 6 ( { _one-plus + } 2 3))))

    (spec "With function variable and lambda"
      (labels ((foo() (lambda (n) (1+ n))))
        (= 6 ( { (foo) + } 2 3))))))

(def-rstruct test-struct x y)
(defun test-def-rstruct()
  (let ((a (make-test-struct :x 1 :y 2))
        (b (make-test-struct :x 3 :y 4)))
    (info "def-rstruct"
    (spec "with- macro assigns slot values"
      (with-test-struct a (= 1 x))
      (with-test-struct b (= 3 x)))
    (spec "with-named- macro assigns slot values"
      (with-named-test-struct a
        (= 1 a/x)))
    (spec "with-named- macro can be nested"
      (with-named-test-struct a
        (with-named-test-struct b
              (and (= 1 a/x)
                   (= 3 b/x))))))))
