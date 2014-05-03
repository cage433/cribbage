(in-package :cage433-cribbage)

(def-rstruct player
  name
  discard
  choose-play-card
  deal
  play-cards
  crib-cards
  )

(def-rstruct game-state
  dealer
  pone
  starter-card
  score
  played-cards
  discards
  play-order 
  )

(defmacro with-game (game-state &body body)
  `(with-game-state ,game-state
    (with-named-player dealer
      (with-named-player pone
        ,@body))))


(defun initialise-game (dealer pone)
  (let* ((deck (shuffled-deck (make-random-state t)))
         (game-state (make-game-state :dealer dealer 
                                      :pone pone)))
    (with-game game-state
      (setf dealer/deal (subseq deck 0 6))
      (setf pone/deal (subseq deck 6 12))
      (setf starter-card (nth 13 deck))
      (setf play-order (list :pone :dealer)))
    game-state))


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
                                            (find-if #_(<= (card-rank-value _) points-left ) remaining-cards))))))


(defun game-player (game-state dealer-or-pone)
  (with-game game-state
    (ecase dealer-or-pone
      (:dealer dealer)
      (:pone pone))))


(defun test-game-state()
  (and
    (test-initialise-game)
    ))

(defun deal (game-state) 
  (declare (ignorable game-state))
  'undefined)

(defun play () 'undefined)
(defun show () 'undefined)
(defun discards () 'undefined)

(defun play-a-game (game-state)
  (deal game-state)
  (discards)
  (play)
  (show))



