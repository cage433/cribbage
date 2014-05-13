(in-package :cage433-cribbage)

(defun discard-cards (game-state)
  (with-game game-state
    (dbind (play crib) (funcall dealer/choose-crib-cards dealer/deal :dealer)
      (setf dealer/play-cards play)
      (setf dealer/original-play-cards play)
      (setf dealer/crib-cards crib))
    (dbind (play crib) (funcall pone/choose-crib-cards pone/deal :pone)
      (setf pone/play-cards play)
      (setf pone/original-play-cards play)
      (setf pone/crib-cards crib))
    game-state))

(defun test-discard-cards()
  (let ((game (initialise-game (make-minimal-player "fred") (make-minimal-player "mike"))))
    (with-game game
      (discard-cards game)
      (info "After discarding"
        (spec "Each player should have four cards in hand"
          (= 4 (length dealer/play-cards))
          (= 4 (length pone/play-cards)))
        (spec "Each player should have two cards in the crib"
          (= 2 (length dealer/crib-cards))
          (= 2 (length pone/crib-cards)))
        (spec "All cards should be distinct"
          (= 12 (length (remove-duplicates (append dealer/crib-cards 
                                                   dealer/play-cards 
                                                   pone/crib-cards 
                                                   pone/play-cards)))))))))
