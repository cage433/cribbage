(in-package :cage433-cribbage)

(defun play-a-card (game-state dealer-or-pone)
  (let ((player (game-player game-state dealer-or-pone)))
    (with-player player
      (awhen (funcall choose-play-card game-state play-cards dealer-or-pone)
        (with-game game-state
          (setf played-cards (cons it played-cards)))
          (setf play-cards (remove-if #_(equal _ it) play-cards))
          it))))


(defun play-round (game-state)
  (with-game game-state
    (while (or (play-a-card game-state (car play-order))
                (play-a-card game-state (cadr play-order)))
      (setf play-order (reverse play-order)))
    (setf discards (cons played-cards discards))
    (setf played-cards nil)))

(defun test-play-round()
  (let ((game (initialise-game (make-minimal-player "fred") (make-minimal-player "mike"))))
    (with-game game
      (setf dealer/play-cards (hand-from-string "AS AC AD AH"))
      (setf pone/play-cards (hand-from-string "2S 2C 2D 2H"))
      (play-round game)
      (info "When playing a round"
        (spec "all cards will be played if sufficiently low"
          (null dealer/play-cards)
          (null pone/play-cards)
          (= 8 (length (car discards)))
          )))))




(defun do-play (game-state)
  (with-game game-state
    (while (or dealer/play-cards pone/play-cards)
      (play-round game-state))))

