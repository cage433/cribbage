(in-package :cage433-cribbage)

(def-rstruct player
  name
  discard
  choose-play-card
  )

(def-rstruct player-cards
  deal
  play-cards
  crib-cards)


(defstruct game-state
  dealer
  pone
  dealer-cards
  pone-cards
  starter-card
  score
  play-cards
  )

(defmacro with-game-state (game-state &body body)
  `(with-slots (dealer pone dealer-cards pone-cards starter-card score play-cards) ,game-state
    ,@body))

(defun initialise-game (dealer pone)
  (let ((deck (shuffled-deck (make-random-state t)))
        (game-state (make-game-state :dealer dealer :pone pone :dealer-cards (make-player-cards) :pone-cards (make-player-cards))))
    (with-game-state game-state
      (with-player-cards dealer-cards
        (setf deal (subseq deck 0 6)))
      (with-player-cards pone-cards
        (setf deal (subseq deck 6 12)))
      game-state)))

(deftest test-game()
  (let ((game-state (initialise-game nil nil)))
    (with-game-state game-state
      (with-player-cards dealer-cards
        (check (= 6 (length deal))))
      (with-player-cards pone-cards
        (check (= 6 (length deal)))))))

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



