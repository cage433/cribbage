(module cards racket 
  (require math)
  (require racket/set)
  (provide shuffle)

  (random-seed 1234)

  (define (new-deck)
    (for*/list ([suit '(C D H S)]
                [rank '(A 2 3 4 5 6 7 8 9 10 J Q K)])
      (list rank suit)))

  (define (shuffle xs)
    (let ([vec (list->vector xs)])
      (for ([i (- (length xs) 1)]) 
        (let ([j (random-integer i (- (length xs) 1))])
;          (display (list i j))
;          (display "\n")
;          (display vec)
;          (display "\n")
          (let ([tmp (vector-ref vec i)])
            (vector-set! vec i (vector-ref vec j))
            (vector-set! vec j tmp))))
      (vector->list vec)))

  (define (shuffled-deck)
    (shuffle (new-deck)))

  (module+ test
    (require rackunit)
    (check-eq? 52 (length (new-deck)))
    (check-eq? 52 (length (shuffled-deck)))
    (check-eq? 52 (set-count (list->set (shuffled-deck))))
    )
)
             

