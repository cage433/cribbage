(load "cards")

(defstruct game-state
  dealer-cards 
  pone-cards
  discards
  starter
  dealer-points
  pone-points
  )

(defmacro with-game (game-state &body body)
  `(let ((game-state ,game-state))
    (with-slots (dealer-cards pone-cards discards starter dealer-points pone-points) game-state
      ,@body)))

(defstruct player
  dealer-discarder
  pone-discarder)

(defmacro with-player (player &body body)
  `(with-slots (dealer-discarder pone-discarder) ,player
    ,@body))

(defun minimal-discarder (cards)
  (list (subseq cards 0 2)
        (subseq cards 2 6)))

(defparameter minimal-player
  (make-player
    :dealer-discarder #'minimal-discarder
    :pone-discarder #'minimal-discarder))

(defun discard-dealer (game-state player)
  (with-game game-state
    (with-player player
      (dbind (two-cards four-cards) (funcall dealer-discarder dealer-cards)
        (setf dealer-cards four-cards)
        (setf discards (append discards two-cards))))))
        
        
(defun discard-pone (game-state player)
  (with-game game-state
    (with-player player
      (dbind (two-cards four-cards) (funcall pone-discarder pone-cards)
        (setf pone-cards four-cards)
        (setf discards (append discards two-cards))))))
    

(defun deal-cards (game-state random-state)
  (let ((deck (shuffled-deck random-state)))
    (with-game game-state
      (setf dealer-cards (subseq deck 0 6))
      (setf pone-cards (subseq deck 6 12))
      (setf starter (nth 12 deck)))))



(defun play-round (game-state dealer pone random-state)
  (deal-cards game-state random-state)
  (discard-dealer game-state dealer)
  (discard-pone game-state pone)
  (while (have-cards game-state)
         (do-play game-state dealer pone))
  (show-pone game-state)
  (show-dealer game-state)
  )

(deftest test-game()
  (with-game (make-game-state)
    (deal-cards game-state (make-random-state t))
      (check
        (and
          (=== 6 (length dealer-cards))
          (=== 6 (length pone-cards))
          (not (null starter))
          (null (intersection dealer-cards pone-cards))
          (not (member starter pone-cards))
          (not (member starter dealer-cards))))

      (discard-dealer game-state minimal-player)
      (check (=== 4 (length dealer-cards)))
      (check (=== 2 (length discards)))
      (check (=== 6 (length pone-cards)))

      (discard-pone game-state minimal-player)
      (check (=== 4 (length dealer-cards)))
      (check (=== 4 (length discards)))
      (check (=== 4 (length pone-cards)))
      ))
