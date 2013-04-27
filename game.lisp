(load "cards")

(defstruct player
  hand-chooser
  hand-player)

(defparameter computer-player (make-player :hand-chooser 1 :hand-player 0))

(defstruct player-cards
  hand
  (play-hand nil :type list)
  (discards nil :type list))

(defstruct game-state
  dealer-cards 
  pone-cards
  starter
  dealer-points
  pone-points
  )

(defun discard-last-two (six-cards)
  (values
    (coerce (subseq six-cards 0 4) 'list)
    (coerce (subseq six-cards 4 6) 'list)))

(defun deal-cards (&optional (random-state (make-random-state t)))
  (let* ((cards (shuffled-deck random-state)))
         (values
           (make-player-cards :hand (subseq cards 0 6))
           (make-player-cards :hand (subseq cards 6 12)))))

(defun discard (cards hand-chooser)
  (multiple-value-bind (play-hand discards) (funcall hand-chooser (player-cards-hand cards))
    (setf (player-cards-play-hand cards) play-hand)
    (setf (player-cards-discards cards) discards)))

(defun play-game (dealer pone &optional (random-state (make-random-state t)))
  (multiple-value-bind (dealer-cards pone-cards) (deal-cards random-state)
    (discard dealer-cards (player-hand-chooser dealer))
    (discard pone-cards (player-hand-chooser pone))
    (values dealer-cards pone-cards)))

(deftest test-game()
  (check
    (multiple-value-bind (dealer-cards pone-cards) (deal-cards (seed-random-state 1234))
      (=== '() 
           (intersection (coerce (player-cards-hand dealer-cards) 'list) 
                         (coerce(player-cards-hand pone-cards) 'list))
           :test equalp)))
  (check
    (let ((dealer (make-player :hand-chooser #'discard-last-two))
          (pone (make-player :hand-chooser #'discard-last-two)))
      (multiple-value-bind (dealer-cards player-cards) (play-game dealer pone)
        (forall 
            (lambda (cards) 
              (null (set-difference (coerce (player-cards-hand cards) 'list) 
                              (append (player-cards-play-hand cards)
                                      (player-cards-discards cards)))))
            (list dealer-cards player-cards))))))
