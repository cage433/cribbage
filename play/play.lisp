(in-package :cage433-cribbage)


(define-condition player-has-won (error)
  ((player :initarg :player :reader winning-player)))

(defun add-points (game-state dealer-or-pone points)
  (let ((player (game-player game-state dealer-or-pone)))
    (with-player player
      (with-game game-state
        (incf score points)
        (if (>= score points-to-win)
          (error 'player-has-won :player player))))))


(defun play-a-card (game-state dealer-or-pone)
  (let ((player (game-player game-state dealer-or-pone)))
    (with-player player
      (let* ((points-left (points-left-in-play game-state))
             (cards-to-choose-from (remove-if-not #_(<= (card-rank-value _) points-left ) play-cards)))
      (awhen (and cards-to-choose-from 
                  (funcall choose-play-card game-state cards-to-choose-from dealer-or-pone))
        (with-game game-state
          (setf played-cards (cons it played-cards))
          (setf play-cards (remove-if #_(equalp _ it) play-cards))
          (setf last-to-play (game-player game-state dealer-or-pone))
          (add-points game-state dealer-or-pone (play-value played-cards))
          it))))))

(defun play-order (game-state)
  (with-game game-state
    (cond ((null last-to-play) (list :pone :dealer)) ; No-one played yet
          ((equalp last-to-play pone) (list :dealer :pone))
          ((equalp last-to-play dealer) (list :pone :dealer)))))

(defun play-sequence (game-state)
  (with-game game-state
    (while (match (play-order game-state)
              ((list first-to-play second-to-play)
                (funcall *print-game-state-fn* game-state :after-fn (lambda ()(sleep 0.5)))
                (or (play-a-card game-state first-to-play)
                    (play-a-card game-state second-to-play)))))
    (incf (player-score last-to-play))
    (setf discards (append played-cards discards))
    (setf played-cards nil))
    game-state)


(defun test-game()
  (let ((game (initialise-game (make-minimal-player "fred") 
                               (make-minimal-player "mike"))))
    (setup-game game)
    game))

(defun test-play-single-round()
  (let ((game (test-game)))
    (with-game game
      (info "When playing a round with sufficiently low cards"
        (setf dealer/play-cards (hand-from-string "AS AC AD AH"))
        (setf pone/play-cards (hand-from-string "2S 2C 2D 2H"))
        (play-sequence game)
        (spec "all cards will be played"
          (null dealer/play-cards)
          (null pone/play-cards)
          (=== 8 (length discards)))
        (spec "One player will have at least one point"
          (plusp (+ dealer/score pone/score))))
      (info "when all players have 10 rank cards"
          (setup-game game)
          (setf dealer/play-cards (hand-from-string "10C JS"))
          (setf pone/play-cards (hand-from-string "QC KH"))
          (progn (setf played-cards nil) t)
          (play-sequence game)
        (spec "pone will have a single point"
          (=== 1 pone/score)))
      (info "when both players have a pair of 10s"
          (setup-game game)
          (setf dealer/play-cards (hand-from-string "10C 10S"))
          (setf pone/play-cards (hand-from-string "10D 10H"))
          (progn (setf played-cards nil) t)
          (play-sequence game)
          (spec "pone will have 7 = (3 of a kind + last card)"
            (=== 7 pone/score))
          (spec "dealer will have 2 = (pair)"
            (=== 2 dealer/score)))
      (info "When cards add up to 31"
          (setup-game game)
          (setf dealer/play-cards (hand-from-string "10C JC"))
          (setf pone/play-cards (hand-from-string "QS KS AD"))
          (progn (setf played-cards nil) t)
          (play-sequence game)
          (spec "pone will have 2 points"
            (=== 2 pone/score))
            ))))




(defun play-rounds (game-state)
  (with-game game-state
    (while (or dealer/play-cards pone/play-cards)
      (play-sequence game-state))
    game-state))

(defun test-play-rounds()
  (let ((game (test-game)))
    (with-game game
      (info "When playing with all 10s"
        (setf dealer/play-cards (hand-from-string "10C 10S"))
        (setf pone/play-cards (hand-from-string "10D 10H"))
        (play-rounds game)
        (spec "pone will have 7 = (3 of a kind plus last card)"
            (=== 7 pone/score))
        (spec "dealer will have 3 = (pair on first round plus last card on second)"
            (=== 3 dealer/score))))))

(defun choose-cards(message cards-to-choose-from)
  (format t "~%~A: " message)
  (finish-output nil)
  (handler-case (let ((choice (hand-from-string (read-line))))
                  (if (subsetp choice cards-to-choose-from :test #'equalp)
                    choice
                    (progn (format t "Invalid choice~%")
                           (choose-cards message cards-to-choose-from))))
    (error () (format t "Not valid cards ~%") (choose-cards message cards-to-choose-from))))

(defun choose-crib-cards (dealt-cards)
  (let ((crib-cards (choose-cards "Choose crib" dealt-cards)))
    (if (= 2 (length crib-cards))
      crib-cards
      (progn
        (format t "Choose exactly two cards~%")
        (finish-output nil)
        (choose-crib-cards dealt-cards)))))

(defun input-play-card (cards-to-play)
  (let ((chosen (choose-cards "Play" cards-to-play)))
    (if (= 1 (length chosen))
      (car chosen)
      (progn
        (format t "Choose exactly one card~%")
        (finish-output nil)
        (input-play-card cards-to-play)))))


(defun make-human-player (name)
  (make-player :name name
               :choose-crib-cards (lambda (cards dealer-or-pone)
                          (declare (ignorable dealer-or-pone))
                          (clear-screen)
                          (format t "Cards are ~A~%" (cards-to-string cards))
                          (let ((discards (choose-crib-cards cards)))
                            (list (set-difference cards discards :test #'equalp)
                                  discards)))
               :choose-play-card (lambda (game-state remaining-cards dealer-or-pone)
                                   (declare (ignorable game-state dealer-or-pone))
                                   (input-play-card remaining-cards))
               :score 0))


(defun play-game (game-state)
  (with-game game-state
    (handler-case 
      (progn
        (discard-cards game-state)
        (while t
          (funcall *print-game-state-fn* game-state :message "About to play")
          (play-rounds game-state)
          (add-points game-state :pone (hand-value starter-card pone/original-play-cards :hand))
          (add-points game-state :dealer (hand-value starter-card dealer/original-play-cards :hand))
          (add-points game-state :dealer (hand-value starter-card (append dealer/crib-cards pone/crib-cards) :crib))
          (rotatef dealer pone)
          (deal-game game-state)
          (discard-cards game-state)
          (funcall *print-game-state-fn* game-state :message "setup for next round" :after-fn (lambda ()(sleep 1.0)))
          ))
      (player-has-won (c) (format t "Game was won by ~A~%" (player-name (winning-player c)))))))


  
;(defun play-hand(game-state)
;  (play-rounds game-state)
          


