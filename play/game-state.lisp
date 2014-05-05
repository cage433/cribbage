(in-package :cage433-cribbage)

(def-rstruct player
  name
  discard
  choose-play-card
  deal
  play-cards
  crib-cards
  score
  )

(def-rstruct game-state
  dealer
  pone
  starter-card
  played-cards
  discards
  play-order 
  last-to-play 
  points-to-win
  )

(defmacro with-game (game-state &body body)
  `(with-game-state ,game-state
    (with-named-player dealer
      (with-named-player pone
        ,@body))))


(defun setup-game (game-state &optional (needed-to-win 61))
  (let ((deck (shuffled-deck (make-random-state t))))
    (with-game game-state
      (setf dealer/deal (subseq deck 0 6))
      (setf pone/deal (subseq deck 6 12))
      (setf discards nil)
      (setf starter-card (nth 13 deck))
      (setf play-order (list :pone :dealer))
      (setf dealer/score 0)
      (setf last-to-play nil)
      (setf pone/score 0)
      (setf points-to-win needed-to-win)
      )

    game-state))

(defun initialise-game (dealer pone &optional (needed-to-win 61))
  (let ((game-state (make-game-state :dealer dealer 
                                     :pone pone)))
    (setup-game game-state needed-to-win)))


(defun test-initialise-game()
  (let ((game-state (initialise-game (make-player) (make-player))))
    (with-game game-state
      (info "After initialization"
        (spec "Dealer and pone should each have 6 cards"
          (= 6 (length dealer/deal))
          (= 6 (length pone/deal)))
        (spec "Starter card should be dealt"
          (not (null starter-card)))
        (spec "All dealt cards should be unique"
          (= 13 (length (remove-duplicates (append dealer/deal pone/deal (list starter-card))))))))))



(defun points-left-in-play (game-state)
  (with-game-state game-state
    (- 31 (apply #'+ (mapcar #'card-rank-value played-cards)))))

(defun make-minimal-player (name)
  (make-player :name name
               :discard (lambda (cards dealer-or-pone) 
                          (declare (ignore dealer-or-pone))
                          (list (subseq cards 0 4) (subseq cards 4 6)))
               :choose-play-card (lambda (game-state remaining-cards dealer-or-pone)
                                  (declare (ignore dealer-or-pone))
                                    (with-game game-state
                                      (let ((points-left (points-left-in-play game-state)))
                                            (find-if #_(<= (card-rank-value _) points-left ) remaining-cards))))
               :score 0))


(defun game-player (game-state dealer-or-pone)
  (with-game game-state
    (ecase dealer-or-pone
      (:dealer dealer)
      (:pone pone))))

(defun pad-left (text len)
  (if (< (length text) len)
    (concatenate 'string (make-string (- len (length text)) :initial-element #\Space) text)
    text))

(defun map-tree (fn tree)
  (cond ((null tree) nil)
        ((consp tree)
          (cons (map-tree fn (car tree))
                (map-tree fn (cdr tree))))
        (t (funcall fn tree))))

(defun stringify (list-of-rows)
  (map-tree #_(format nil "~A" _) list-of-rows))

(defun col-widths (list-of-rows)
  (apply #'mapcar #'max (map-tree #'length (stringify list-of-rows))))

(defun tabulate (&rest list-of-rows)
  (let ((widths (col-widths list-of-rows))
        (list-of-text-rows (stringify list-of-rows)))
    (mapc (lambda (text-row)
            (format t "~{~A~^ ~}~%" 
                    (mapcar #'pad-left text-row widths)))
          list-of-text-rows)))

(defun cards-to-string (cards)
  (format nil "~{~A~^ ~}" 
    (mapcar {#_(pad-left _ 3) #'card-to-short-string} cards)))

(defun print-full-game-state (game-state)
  (with-game game-state
    (labels ((player-row (player)
              (with-player player
                  (list name 
                        (cards-to-string play-cards) 
                        (cards-to-string crib-cards)
                         score))))
      (if dealer/play-cards
        (tabulate 
          (list "  Name" "Play" "Crib" "  Score")
          (player-row dealer)
          (player-row pone))
        (tabulate
          (list " Name" "Cards")
          (list dealer/name (cards-to-string dealer/deal))
          (list pone/name (cards-to-string pone/deal))))
      (if played-cards
        (format t "Played ~A~%" (cards-to-string played-cards)))
      nil)))

(defun test-game-state()
  (and
    (test-initialise-game)
    ))




