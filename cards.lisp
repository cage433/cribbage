(load "unit")

(defstruct card
  rank rank-index rank-value suit)


(defparameter *SUITS* (list "H" "C" "D" "S"))
(defparameter *RANK-INDEXES* (list 0 1 2 3 4 5 6 7 8 9 10 11 12))
(defparameter *RANK-VALUES* (list 2 3 4 5 6 7 8 9 10 10 10 10 1))
(defparameter *RANKS* (list "2" "3" "4" "5" "6" "7" "8" "9" "10" "J" "Q" "K" "A"))

(defparameter *CARDS*
  (make-array 52
              :element-type 'card 
              :initial-contents 
                (mappend
                  (lambda (suit) 
                    (mapcar (lambda (rank rank-index rank-value) (make-card :rank rank :rank-index rank-index :rank-value rank-value :suit suit) )
                            *RANKS*
                            *RANK-INDEXES*
                            *RANK-VALUES*
                            ))
                    *SUITS*)))

(defun shuffle (vec)
  (dotimes (i (length vec))
    (let ((j (+ i (random (- (length vec) i)))))
      (rotatef (aref vec i) (aref vec j))))
  vec)

(defun shuffled-deck() 
  (shuffle (copy-seq *CARDS*)))


(defun hand-value (starter hand)
  (let ((value 0)
        (hand-suits (remove-duplicates (mapcar #'card-suit hand)))
        (starter-suit (card-suit starter)))
    (if ( = (length hand-suits) 1)
      (progn
        (incf value 4)
        (if (eq starter-suit (car hand-suits))
          (incf value))))
    value))


(defun card-from-name (name)
  (let ((r (position (subseq name 0 (1- (length name))) *RANKS* :test #'string-equal))
        (s (position (subseq name (1- (length name))) *SUITS* :test #'string-equal)))
    (or (and r s 
          (make-card :rank (nth r *RANKS*)
                    :rank-index r
                    :rank-value (nth r *RANK-VALUES*)
                    :suit (nth s *SUITS*)))
        (error (format nil "Can't make a card from ~A" name)))))

    
(deftest test-cards()
  (check (= 52 (length *CARDS*)))
  (check (= 52 (length (remove-duplicates (shuffled-deck)))))
  (check (= 10 (card-rank-value (card-from-name "10H"))))
  (check (= 10 (card-rank-value (card-from-name "QH"))))
  (check (= 1 (card-rank-value (card-from-name "AH"))))
  (check (= 5 (card-rank-value (card-from-name "5D"))))
  )



