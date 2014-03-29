
(defun concat-syms (&rest syms)
    (intern (apply #'concatenate 'string (mapcar #'symbol-name syms))))

(defmacro def-rstruct (name &rest its-slots)
  (let ((macro-name (concat-syms 'with- 'fred)))
    `(progn
      (defstruct ,name ,@its-slots)
      (defmacro ,macro-name (a-fred &body body)
        `(with-slots (,@'(,@its-slots)) ,a-fred
          ,@body)))))

(def-rstruct fred x)

(let ((f (make-fred :x 1)))
  (with-fred f (format t "x = ~a~%" x)))
