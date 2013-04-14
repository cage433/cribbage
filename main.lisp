(load "unit")

(defun mappend (fn &rest lsts)
  "maps elements in list and finally appends all resulted lists."
  (apply #'append (apply #'mapcar fn lsts)))

(defparameter CARDS
  (mappend
    (lambda (suit) 
      (mapcar (lambda (rank) (list rank suit)) 
              '(A 2 3 4 5 6 7 8 9 10 J Q K)))
     '(C D H S)))

(defun run-tests (&key (exit-on-termination t))
  (let ((result (check (= 52 (length CARDS)))))
    (if exit-on-termination
      (sb-ext:exit :code (if result 0 1))
      result)))



