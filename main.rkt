#lang racket

(define cards 
  (for*/list ([suit '(C D H S)]
              [rank '(A 2 3 4 5 6 7 8 9 10 J Q K)])
    (list rank suit)))

(module+ test
  (require rackunit)
  (check-eq? 52 (length cards)))

             

