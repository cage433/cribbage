(load "unit")

(defparameter CARDS
  (mappend
    (lambda (suit) 
      (mapcar (lambda (rank) (list rank suit)) 
              '(A 2 3 4 5 6 7 8 9 10 J Q K)))
     '(C D H S)))

(deftest test-cards()
  (check (= 52 (length CARDS))))



