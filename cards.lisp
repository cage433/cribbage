(load "unit")

(defstruct card
  rank rank-index rank-value suit)


(defparameter *SUITS* (list "H" "C" "D" "S"))
(defparameter *RANK-INDEXES* (list 0 1 2 3 4 5 6 7 8 9 10 11 12))
(defparameter *RANK-VALUES* (list 1 2 3 4 5 6 7 8 9 10 10 10 10))
(defparameter *RANKS* (list "A" "2" "3" "4" "5" "6" "7" "8" "9" "10" "J" "Q" "K" "A"))

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


(defun group-by (fn xs) 
  (let ((groups (make-hash-table :test #'eql)))
    (mapc 
      (lambda (x) 
        (let* ((y (funcall fn x))
               (grp (gethash y groups nil)))
            (setf (gethash y groups) (cons y grp))))
      xs)
    groups))

(defun hash-values (hash)
  (let ((vs nil))
    (maphash (lambda (k v) (declare (ignore k)) (setf vs (cons v vs))) hash)
    vs))

(defun flush-value (cards)
  (let* ((suit-counts (mapcar #'length (hash-values (group-by #'card-suit cards))))
         (longest-suit (apply #'max suit-counts)))
      (if (>= longest-suit 4)
        longest-suit
        0)))

(defun pairs-and-higher-value (cards)
  (let ((rank-counts (mapcar #'length (hash-values (group-by #'card-rank cards))))
        (value 0))
    (mapc (lambda (count)
            (cond ((= 4 count) (incf value 12))
                  ((= 3 count) (incf value 6))
                  ((= 2 count) (incf value 2))))
          rank-counts)
    value))

(defun run-value (cards)
  (let* ((by-indices (group-by #'card-rank-index cards))
         (contiguous-groups (remove-if-not (lambda (g) (>= (length g) 3))
                                       (cl-utilities:split-sequence-if (lambda (i) (null (gethash i by-indices))) *RANK-INDEXES* :remove-empty-subseqs t)))
         (value 0))
    (mapc (lambda (group) 
            (incf value (apply #'* (length group) (mapcar (lambda (i) (length (gethash i by-indices))) group))))
          contiguous-groups)
    value))

    



(defun hand-value (cards)
  (+
    (flush-value cards)
    (pairs-and-higher-value cards)
    (run-value cards)
    ))


(defun card-from-name (name)
  (let ((r (position (subseq name 0 (1- (length name))) *RANKS* :test #'string-equal))
        (s (position (subseq name (1- (length name))) *SUITS* :test #'string-equal)))
    (if (and r s)
        (make-card :rank (nth r *RANKS*)
                  :rank-index r
                  :rank-value (nth r *RANK-VALUES*)
                  :suit (nth s *SUITS*))
        (error (format nil "Can't make a card from ~A" name)))))

(defun hand-from-names (names)
  (mapcar #'card-from-name names))

(defun hand-from-string (hand-as-string)
  (mapcar #'card-from-name
          (cl-utilities:split-sequence #\Space hand-as-string :remove-empty-subseqs t)))
    
(deftest test-cards()
  (check 
    (= 52 (length *CARDS*))
    (= 52 (length (remove-duplicates (shuffled-deck))))
    (= 10 (card-rank-value (card-from-name "10H")))
    (= 10 (card-rank-value (card-from-name "QH")))
    (= 1 (card-rank-value (card-from-name "AH")))
    (= 5 (card-rank-value (card-from-name "5D")))
    (= 4 (flush-value (hand-from-string "2H 4H 6H QH")))
    (= 5 (flush-value (hand-from-string "2H 4H 6H QH AH 2C")))
    (= 0 (flush-value (hand-from-string "2D 4S 6H QH AH 2C")))
    (= 0 (pairs-and-higher-value (hand-from-string "2D 3C 4S 8C 10H QH")))
    (= 2 (pairs-and-higher-value (hand-from-string "2D 2C 3S 4C 10H QH")))
    (= 6 (pairs-and-higher-value (hand-from-string "2D 2C 2S 4C 10H QH")))
    (= 4 (pairs-and-higher-value (hand-from-string "2D 2C AS AC 10H QH")))
    (= 12 (pairs-and-higher-value (hand-from-string "2D 2C 2S 2H 10H QH")))
    (= 0 (run-value (hand-from-string "2D 3C 5S 7C 8C 10D")))
    (= 3 (run-value (hand-from-string "2D 3C 4S")))
    (= 3 (run-value (hand-from-string "2D 3C 4S 6C 7D 9D")))
    (= 6 (run-value (hand-from-string "2D 3C 4S 6C 7D 8D")))
    (= 4 (run-value (hand-from-string "2D 3C 4S 5C 7D 8D")))
    (= 5 (run-value (hand-from-string "2D 3C 4S 5C 6D 8D")))
    (= 12 (run-value (hand-from-string "2C 2D 3C 4S 4C 8D")))
    (= 16 (run-value (hand-from-string "2C 2D 3C 4S 4C 5D")))
  ))



