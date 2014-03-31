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


(def-rstruct game-state
  dealer
  pone
  dealer-cards
  pone-cards
  starter-card
  score
  play-cards
  )

(defmacro with-game (game-state &body body)
  `(with-game-state ,game-state
    (with-named-player dealer
      (with-named-player pone
        (with-named-player-cards dealer-cards
          (with-named-player-cards pone-cards
            ,@body))))))


(defun initialise-game (dealer pone)
  (let* ((deck (shuffled-deck (make-random-state t)))
         (game-state (make-game-state :dealer dealer 
                                      :pone pone 
                                      :dealer-cards (make-player-cards) 
                                      :pone-cards (make-player-cards))))
    (with-game game-state
      (setf dealer-cards/deal (subseq deck 0 6))
      (setf pone-cards/deal (subseq deck 6 12))
      (setf starter-card (nth 13 deck)))
    game-state))


(defun test-initialise-game()
  (let ((game-state (initialise-game nil nil)))
      (with-game game-state
        (info "After initialization"
          (spec "Dealer and pone should each have 6 cards"
            (= 6 (length dealer-cards/deal))
            (= 6 (length pone-cards/deal)))
          (spec "Starter card should be dealt"
            (not (null starter-card)))
          (spec "All dealt cards should be unique"
            (= 13 (length (remove-duplicates (append dealer-cards/deal pone-cards/deal (list starter-card))))))))))


(defun discard-cards (game-state)
  (with-game game-state
    (dbind (play crib) (funcall dealer/discard dealer-cards/deal :dealer)
      (setf dealer-cards/play-cards play)
      (setf dealer-cards/crib-cards crib))
    (dbind (play crib) (funcall pone/discard pone-cards/deal :pone)
      (setf pone-cards/play-cards play)
      (setf pone-cards/crib-cards crib))))

(defun make-minimal-player (name)
  (make-player :name name
               :discard (lambda (cards dealer-or-pone) 
                          (declare (ignore dealer-or-pone))
                          (list (subseq cards 0 4) (subseq cards 4 6)))))

(defun test-discard-cards()
  (let ((game (initialise-game (make-minimal-player "fred") (make-minimal-player "mike"))))
    (with-game game
      (discard-cards game)
      (info "After discarding"
        (spec "Each player should have four cards in hand"
          (= 4 (length dealer-cards/play-cards))
          (= 4 (length pone-cards/play-cards)))
        (spec "Each player should have two cards in the crib"
          (= 2 (length dealer-cards/crib-cards))
          (= 2 (length pone-cards/crib-cards)))
        (spec "All cards should be distinct"
          (= 12 (length (remove-duplicates (append dealer-cards/crib-cards dealer-cards/play-cards pone-cards/crib-cards pone-cards/play-cards)))))
        ))))

(defun test-game()
  (and
    (test-initialise-game)
    (test-discard-cards)
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



