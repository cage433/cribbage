
(defun concat-syms (&rest syms)
    (intern (apply #'concatenate 'string (mapcar #'symbol-name syms))))

(defmacro def-rstruct (name &rest its-slots)
  `(progn
    (defstruct ,name ,@its-slots)
    (defmacro ,(concat-syms 'with- name) (thing-name &body body)
      `(with-slots ,(mapcar (lambda (s) (list (concat-syms thing-name '/ s) s)) ',its-slots) ,thing-name
        ,@body))))

;(format t "~a~% " (macroexpand '(def-rstruct fred x y)))
(def-rstruct fred x y)

;(format t "~a~% " (macroexpand '(with-fred f 33)))
(let ((f (make-fred :x 1)))
  (with-fred f (format t "x = ~a~%" (list f/x f/y))))
