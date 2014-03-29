
(defun concat-syms (&rest syms)
    (intern (apply #'concatenate 'string (mapcar #'symbol-name syms))))

(defmacro def-rstruct (name &rest its-slots)
  `(progn
    (defstruct ,name ,@its-slots)
    (defmacro ,(concat-syms 'with- name) (X &body body)
      `(with-slots (,@',its-slots) ,X
        ,@body))))

(def-rstruct fred x y)

(let ((f (make-fred :x 1)))
  (with-fred f (format t "x = ~a~%" x)))
