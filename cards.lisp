(in-package :cage433-cribbage)

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
         (contiguous-groups (remove-if-not #_(>= (length _) 3)
                                       (cl-utilities:split-sequence-if #_(null (gethash _ by-indices)) *RANK-INDEXES* :remove-empty-subseqs t)))
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
                    (remove-if-not #_(string-equal (card-rank _) "J") hand))
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

(defun play-pairs-and-higher-value (cards)
  (if cards
    (progn
      (let ((n (length (take-while [equal (card-rank (car cards))] (mapcar #'card-rank (cdr cards))))))
        (cond 
          ((= n 0) 0)
          ((= n 1) 2)
          ((= n 2) 6)
          ((= n 3) 12)
          (t (error (format nil "Unexpected number of matches ~a" cards))))))
    0))


(defun play-run-value (cards)
  (if cards
    (apply #'max 
            (maplist (lambda (sub-ranks) 
                      (let ((len (length sub-ranks))
                            (distinct-ranks (remove-duplicates sub-ranks)))
                        (if (and (>= len 3)
                                 (= len (length distinct-ranks))
                                 (= (1- len) 
                                    (- (apply #'max distinct-ranks) 
                                       (apply #'min distinct-ranks))))
                          len
                          0)))
                    (reverse (mapcar #'card-rank-value cards))))
    0))

(defun play-thirty-one-value (cards)
  (if (= 31 (apply #'+ (mapcar #'card-rank-value cards)))
    1
    0))

(defun play-value (cards)
  (+
    (play-fifteen-value cards)
    (play-pairs-and-higher-value cards)
    (play-run-value cards)
    (play-thirty-one-value cards)))

(defun card-to-short-string (card)
  (format nil "~a~a" (card-rank card) (card-suit card)))

(defun cards-as-string (cards)
  (format nil "~{~a~^ ~}" (mapcar #'card-to-short-string cards)))

(defun display-cards (cards)
  (format t "~a~%" (cards-as-string cards)))

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
    
(defun test-cards()
  (info "The deck"
    (spec "Should have 52 cards"
      (= 52 (length *CARDS*)))
    (spec "Should have no duplicated cards"
      (= 52 (length (remove-duplicates (shuffled-deck))))))
  (info "The cards"
    (spec "Can round trip from string"
      (=== "10H" (card-to-short-string (card-from-name "10H")) :test #'equal)
      (=== "AD" (card-to-short-string (card-from-name "AD")) :test #'equal)))
  (info "When valuing fifteens during the play"
    (spec "Should count valid fifteens"
      (=== 2 (play-fifteen-value (hand-from-string "10C 5H")))
      (=== 2 (play-fifteen-value (hand-from-string "10C 3H 2H"))))
    (spec "Should not count invalid fifteens"
      (=== 0 (play-fifteen-value (hand-from-string "")))
      (=== 0 (play-fifteen-value (hand-from-string "8D")))
      (=== 0 (play-fifteen-value (hand-from-string "8D JC")))))
  (info "Card ranks"
    (spec "should be as expected"
      (= 10 (card-rank-value (card-from-name "10H")))
      (= 10 (card-rank-value (card-from-name "QH")))
      (= 1 (card-rank-value (card-from-name "AH")))
      (= 5 (card-rank-value (card-from-name "5D")))))
  (info "When valuing flushes"
    (spec "Valid flushes should count"
      (= 4 (flush-value (hand-from-string "2H 4H 6H QH") :hand))
      (= 5 (flush-value (hand-from-string "2H 4H 6H QH AH 2C") :hand))
      (= 5 (flush-value (hand-from-string "2H 4H 6H QH AH 2C") :crib)))
    (spec "Invlaid flushes shouldn't"
      (= 0 (flush-value (hand-from-string "") :crib))
      (= 0 (flush-value (hand-from-string "2H 4H 6H QH KD") :crib))
      (= 0 (flush-value (hand-from-string "2D 4S 6H QH AH 2C") :hand))))
  (info "When valuing pairs"
    (spec "Empty hand should have no value"
      (= 0 (pairs-and-higher-value (hand-from-string ""))))
    (spec "Hand with no pairs should have no value"
      (= 0 (pairs-and-higher-value (hand-from-string "2D 3C 4S 8C 10H QH"))))
    (spec "Hand with a single pair is worth 2"
      (= 2 (pairs-and-higher-value (hand-from-string "2D 2C 3S 4C 10H QH"))))
    (spec "Hand with a triple is worth 6"
      (= 6 (pairs-and-higher-value (hand-from-string "2D 2C 2S 4C 10H QH"))))
    (spec "Hand with two pairs is worth 4"
      (= 4 (pairs-and-higher-value (hand-from-string "2D 2C AS AC 10H QH"))))
    (spec "Hand with four of a kind is worth 12"
      (= 12 (pairs-and-higher-value (hand-from-string "2D 2C 2S 2H 10H QH")))))
  (info "When valuing runs"
    (spec "Empty hand should have no value"
      (= 0 (run-value (hand-from-string ""))))
    (spec "Hand with no runs is worth 0"
      (= 0 (run-value (hand-from-string "2D 3C 5S 7C 8C 10D"))))
    (spec "Hands with single runs are worth the length of the run"
      (= 3 (run-value (hand-from-string "2D 3C 4S")))
      (= 3 (run-value (hand-from-string "2D 3C 4S 6C 7D 9D")))
      (= 6 (run-value (hand-from-string "2D 3C 4S 6C 7D 8D")))
      (= 4 (run-value (hand-from-string "2D 5C 4S 3C 7D 8D")))
      (= 5 (run-value (hand-from-string "2D 3C 4S 5C 6D 8D"))))
    (spec "Hands with multiple runs are values correctly"
      (= 12 (run-value (hand-from-string "2C 2D 3C 4S 4C 8D")))
      (= 16 (run-value (hand-from-string "2C 2D 3C 4S 4C 5D")))))
  (info "When valuing fifteens in the hand"
    (spec "Empty hand should have no value"
      (= 0 (fifteens-value (hand-from-string ""))))
    (spec "Hand with no 15s is worth 0"
      (= 0 (fifteens-value (hand-from-string "2D 4C 4S 8C 10H QH"))))
    (spec "Hands with a single fifteen are worth 2"
      (= 2 (fifteens-value (hand-from-string "10C 5D")))
      (= 2 (fifteens-value (hand-from-string "10C 5D 6H 8D 3S 3C"))))
    (spec "Hands with multiple 15s are valued correctly"
      (= 6 (fifteens-value (hand-from-string "10C 5D 6H 9D 3S 3C")))
      (= 16 (fifteens-value (hand-from-string "4C 4D 5S 5D 6H 6D")))))
  (info "His nobs"
    (spec "Empty hand should have no value"
      (= 0 (jack-value (card-from-name "JD") (hand-from-string ""))) )
    (spec "One point for jack of same suit as starter"
      (= 1 (jack-value (card-from-name "2H") (hand-from-string "4C 4D 5S 5D 6H JH")))
      (= 1 (jack-value (card-from-name "JD") (hand-from-string "4C 4D 5S 5D JD JH"))))
    (spec "No points for jacks of different suits to starter"
      (= 0 (jack-value (card-from-name "2D") (hand-from-string "4C 4D 5S 5D 6H JH")))
      (= 0 (jack-value (card-from-name "2D") (hand-from-string "4C 4D 5S 5D 6H 10H")))
      (= 0 (jack-value (card-from-name "JD") (hand-from-string "4C 4D 5S 5D 6H 10H")))))
  (info "Valuing runs during play"
    (spec "No run value before cards played"
      (=== 0 (play-run-value nil)))
    (spec "No run value from only two cards"
      (= 0 (play-run-value (hand-from-string "2D 3C"))))
    (spec "Run value same as length of run"
      (=== 3 (play-run-value (hand-from-string "2D 3C 4S")))
      (=== 6 (play-run-value (hand-from-string "5D 2D 7S 6S 3C 4S")))
      (=== 4 (play-run-value (hand-from-string "5D 2D 3C 4S"))))
    (spec "If no run then value is zero"
      (=== 0 (play-run-value (hand-from-string "3C 4S")))
      (=== 0 (play-run-value (hand-from-string "10D 2D 3C 4S")))
      (=== 0 (play-run-value (hand-from-string "5D 2D 7S 3C 4S")))))
  (info "Valuing 31 during play"
    (spec "no points for 31 before cards played"
      (=== 0 (play-thirty-one-value nil)))
    (spec "No points for 31 if total isn't 31"
      (=== 0 (play-thirty-one-value (hand-from-string "9C 10D 10S AH"))))
    (spec "one point for 31 then that total reached"
      (=== 1 (play-thirty-one-value (hand-from-string "10C 10D 10S AH")))))
  (info "Valuing 15s during play"
    (spec "No points before cards played"
      (=== 0 (play-fifteen-value (hand-from-string ""))))
    (spec "No points if total isn't 15"
      (=== 0 (play-fifteen-value (hand-from-string "9C 10D 10S AH"))))
    (spec "Two points for hitting fifteen"
      (=== 2 (play-fifteen-value (hand-from-string "9C 6D"))))
    (spec "No points for fifteen on the turn after 15"
      (=== 0 (play-fifteen-value (hand-from-string "9C 6D 3C")))))
  (info "Various hand values"
    (spec "15 8, 2 for a pair 456 456 = 16"
      (= 16 (hand-value (card-from-name "5D") (hand-from-string "6C 10D 5H 4S") :crib)))
    (spec "15 4, 2 for a pair, A23 A23, one for his nobs = 13"
      (= 13 (hand-value (card-from-name "AD") (hand-from-string "JD 3S 3C 2S") :crib)))
    (spec "15 2, 2 for a pair, 2 for a pair = 6"
      (= 6 (hand-value (card-from-name "AD") (hand-from-string "3H 3S 4S 4D") :crib)))
    (spec "15 16, 12 for four-of-a-kind, one for his nobs = 29"
      (= 29 (hand-value (card-from-name "5C") (hand-from-string "JC 5S 5D 5H") :crib)))
    (spec "15 10, 6 for three-of-a-kind = 16"
      (= 16 (hand-value (card-from-name "6D") (hand-from-string "3C 3D 3H 9S") :crib)))
    (spec "15 2, 234 234 234, 6 for three-of-a-kind = 17"
      (= 17 (hand-value (card-from-name "4H") (hand-from-string "2D 3C 4S 4C") :crib))))
  (info "Various play values"
    (spec "15 2, 5 for a run = 7"
      (= 7 (play-value (hand-from-string "AC 5D 3S 2H 4S"))))))



