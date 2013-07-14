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


(defun shuffle (vec &optional (random-state (make-random-state t)))
  (dotimes (i (length vec))
    (let ((j (+ i (random (- (length vec) i) random-state))))
      (rotatef (aref vec i) (aref vec j))))
  vec)

(defun shuffled-deck (&optional (random-state (make-random-state t))) 
  (coerce (shuffle (copy-seq *CARDS*) random-state) 'list))


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

(defun flush-value (cards hand-or-crib)
  (let* ((suit-counts (mapcar #'length (hash-values (group-by #'card-suit cards))))
         (longest-suit (if suit-counts (apply #'max suit-counts) 0))
         (minimal-flush-size (ecase hand-or-crib
                                (:hand 4)
                                (:crib 5))))
      (if (>= longest-suit minimal-flush-size)
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

(defun fifteens-value (cards)
  (let ((ranks (sort (mapcar #'card-rank-value cards)
                     #'<))
        (value-per-fifteen 2))
    (labels ((ways-to-make (n ns)
                (cond ((null ns) 0)
                      ((= (car ns) n) (+ 1 (ways-to-make n (cdr ns))))
                      ((< (car ns) n) (+ (ways-to-make (- n (car ns)) (cdr ns))
                                        (ways-to-make n (cdr ns))))
                      (t 0))))
      (* value-per-fifteen 
         (ways-to-make 15 ranks)))))

(defun jack-value (starter hand)
  (if (find (card-suit starter) 
            (mapcar #'card-suit
                    (remove-if-not (lambda (card) (string-equal (card-rank card) "J")) hand))
            :test #'string-equal)
    1
    0))


(defun hand-value (starter hand hand-or-crib)
  (let ((cards (cons starter hand)))
    (+
      (flush-value cards hand-or-crib)
      (pairs-and-higher-value cards)
      (run-value cards)
      (fifteens-value cards)
      (jack-value starter hand)
      )))

(defun play-fifteen-value (cards)
  (if (= 15 (apply #'+ (mapcar #'card-rank-value cards)))
    2
    0))

(defun play-value (cards)
  (+
    (play-fifteen-value cards)
    (play-pairs-and-higher-value cards)
    (play-run-value cards)
    (play-thierty-one-value cards)))

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
    (=== 2 (play-fifteen-value (hand-from-string "10C 5H")))
    (=== 2 (play-fifteen-value (hand-from-string "10C 3H 2H")))
    (=== 0 (play-fifteen-value (hand-from-string "")))
    (=== 0 (play-fifteen-value (hand-from-string "8D")))
    (=== 0 (play-fifteen-value (hand-from-string "8D JC")))
    (= 52 (length *CARDS*))
    (= 52 (length (remove-duplicates (shuffled-deck))))
    (= 10 (card-rank-value (card-from-name "10H")))
    (= 10 (card-rank-value (card-from-name "QH")))
    (= 1 (card-rank-value (card-from-name "AH")))
    (= 5 (card-rank-value (card-from-name "5D")))
    (= 0 (flush-value (hand-from-string "") :crib))
    (= 4 (flush-value (hand-from-string "2H 4H 6H QH") :hand))
    (= 0 (flush-value (hand-from-string "2H 4H 6H QH KD") :crib))
    (= 5 (flush-value (hand-from-string "2H 4H 6H QH AH 2C") :hand))
    (= 5 (flush-value (hand-from-string "2H 4H 6H QH AH 2C") :crib))
    (= 0 (flush-value (hand-from-string "2D 4S 6H QH AH 2C") :hand))
    (= 0 (pairs-and-higher-value (hand-from-string "")))
    (= 0 (pairs-and-higher-value (hand-from-string "2D 3C 4S 8C 10H QH")))
    (= 2 (pairs-and-higher-value (hand-from-string "2D 2C 3S 4C 10H QH")))
    (= 6 (pairs-and-higher-value (hand-from-string "2D 2C 2S 4C 10H QH")))
    (= 4 (pairs-and-higher-value (hand-from-string "2D 2C AS AC 10H QH")))
    (= 12 (pairs-and-higher-value (hand-from-string "2D 2C 2S 2H 10H QH")))
    (= 0 (run-value (hand-from-string "")))
    (= 0 (run-value (hand-from-string "2D 3C 5S 7C 8C 10D")))
    (= 3 (run-value (hand-from-string "2D 3C 4S")))
    (= 3 (run-value (hand-from-string "2D 3C 4S 6C 7D 9D")))
    (= 6 (run-value (hand-from-string "2D 3C 4S 6C 7D 8D")))
    (= 4 (run-value (hand-from-string "2D 3C 4S 5C 7D 8D")))
    (= 5 (run-value (hand-from-string "2D 3C 4S 5C 6D 8D")))
    (= 12 (run-value (hand-from-string "2C 2D 3C 4S 4C 8D")))
    (= 16 (run-value (hand-from-string "2C 2D 3C 4S 4C 5D")))
    (= 0 (fifteens-value (hand-from-string "")))
    (= 0 (fifteens-value (hand-from-string "2D 4C 4S 8C 10H QH")))
    (= 2 (fifteens-value (hand-from-string "10C 5D")))
    (= 2 (fifteens-value (hand-from-string "10C 5D 6H 8D 3S 3C")))
    (= 6 (fifteens-value (hand-from-string "10C 5D 6H 9D 3S 3C")))
    (= 16 (fifteens-value (hand-from-string "4C 4D 5S 5D 6H 6D")))
    (= 0 (jack-value (card-from-name "JD") (hand-from-string "")))
    (= 1 (jack-value (card-from-name "2H") (hand-from-string "4C 4D 5S 5D 6H JH")))
    (= 1 (jack-value (card-from-name "JD") (hand-from-string "4C 4D 5S 5D JD JH")))
    (= 0 (jack-value (card-from-name "2D") (hand-from-string "4C 4D 5S 5D 6H JH")))
    (= 0 (jack-value (card-from-name "2D") (hand-from-string "4C 4D 5S 5D 6H 10H")))
    (= 0 (jack-value (card-from-name "JD") (hand-from-string "4C 4D 5S 5D 6H 10H")))
    (= 16 (hand-value (card-from-name "5D") (hand-from-string "6C 10D 5H 4S") :crib))
    (= 13 (hand-value (card-from-name "AD") (hand-from-string "JD 3S 3C 2S") :crib))
    (= 6 (hand-value (card-from-name "AD") (hand-from-string "3H 3S 4S 4D") :crib))
    (= 29 (hand-value (card-from-name "5C") (hand-from-string "JC 5S 5D 5H") :crib))
    (= 16 (hand-value (card-from-name "6D") (hand-from-string "3C 3D 3H 9S") :crib))
    (= 17 (hand-value (card-from-name "4H") (hand-from-string "2D 3C 4S 4C") :crib))
  ))



