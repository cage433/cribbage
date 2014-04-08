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


(defun discard-cards (game-state)
  (with-game game-state
    (dbind (play crib) (funcall dealer/discard dealer/deal :dealer)
      (setf dealer/play-cards play)
      (setf dealer/crib-cards crib))
    (dbind (play crib) (funcall pone/discard pone/deal :pone)
      (setf pone/play-cards play)
      (setf pone/crib-cards crib))
    game-state))

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

(defun game-player (game-state dealer-or-pone)
  (with-game game-state
    (ecase dealer-or-pone
      (:dealer dealer)
      (:pone pone))))

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

(defun make-test-game()
  (discard-cards 
    (initialise-game (make-minimal-player "fred") (make-minimal-player "mike"))))

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

(defun test-game()
  (and
    (test-initialise-game)
    (test-discard-cards)
    (test-play-round)))

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



