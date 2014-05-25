(in-package :cage433-lisp-utils)

(defun foldr (fn init args)
	(reduce fn args :initial-value init :from-end t))
(defun foldl (fn init args)
	(reduce fn args :initial-value init :from-end nil))

(defmacro abbrev (short long)
       `(defmacro ,short (&rest args)
           `(,',long ,@args)))

(abbrev dbind destructuring-bind)
(abbrev mbind multiple-value-bind)

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

(defun test-span()
  (info "Span function"
    (spec "Returns nested nil when passed nil as an argument"
      (equalp '(()) (span #'evenp nil)))
    (spec "Works when predicate fails on first element"
      (equalp '(() 1 2 3) (span #'evenp '(1 2 3))))
    (spec "Works when predicate never fails"
      (equalp '((1 2 3)) (span #'numberp '(1 2 3))))
    (spec "Works when predicate fails in the middle of a list"
      (equalp '((1 2 3) 4 5 6) (span (lambda (i) (< i 3.5)) '(1 2 3 4 5 6))))))

(defun for-all (pred list)
  (cond ((null list) t)
        ((funcall pred (car list)) (for-all pred (cdr list)))
        (t nil)))


(defun take-while (pred seq)
	"Returns the longest left sublist for which pred is true"
  (labels ((recurse (seq acc)
                (if (or (null seq) (not (funcall pred (car seq))))
                  (reverse acc)
                  (recurse (cdr seq) (cons (car seq) acc)))))
    (recurse seq nil)))

(defun test-take-while()
  (info "take-while"
    (spec "Returns nil when passed an empty list"
      (equalp nil (take-while #'evenp nil)))
    (spec "Returns nil when predicate failes on the first element"
      (equalp nil (take-while #'evenp '(1 2 4 6 8))))
    (spec "Returns whole list if predicate never fails"
      (equalp '(1 2 4 6 8) (take-while #'numberp '(1 2 4 6 8))))
    (spec "Returns a proper sublist at the point predicate fails"
      (=== '(1 2 3) (take-while (lambda (i) (< i 4)) '(1 2 3 4 0)) 
              :test #'equal))))


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

(defun group-by (fn xs) 
  (let ((groups (make-hash-table :test #'eql)))
    (mapc 
      (lambda (x) 
        (let* ((y (funcall fn x))
               (grp (gethash y groups nil)))
            (setf (gethash y groups) (cons x grp))))
      xs)
    groups))

(defun hash-to-list(hash)
  (let ((kvs nil))
    (maphash (lambda (k v) (setf kvs (cons (list k v) kvs))) hash)
    kvs))

(defun hash-values (hash)
  (mapcar #'cadr (hash-to-list hash)))

(defun mappend (fn &rest lsts)
    "maps elements in list and finally appends all resulted lists."
      (apply #'append (apply #'mapcar fn lsts)))

(defun cross-product (&rest sets)
  "Takes a collection of sets and returns 
  the cross product of these. e.g. given
    '( (1 2 3) '(X Y) )
  returns
    '( (1 X) (1 Y) 
       (2 X) (2 Y) 
       (3 X) (3 y) )
    "
  (cond ((null sets) sets)
        ((= 1 (length sets)) (mapcar #'list (car sets)))
        (t (mappend 
             (lambda (y)
                  (mapcar (lambda (x) (cons x y))
                          (car sets)))
             (apply #'cross-product (cdr sets))))))
