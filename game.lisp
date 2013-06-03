(load "cards")

(defstruct game-state
  dealer
  pone
  (crib nil)
  starter
  (next-to-play :dealer)
  )

(defmacro with-game (game-state &body body)
  `(let ((game-state ,game-state))
    (with-slots (dealer pone crib starter next-to-play) game-state
      (with-slots ((dealer-cards cards)) dealer
        (with-slots ((pone-cards cards)) pone

      ,@body)))))

(defstruct player
  name
  discarder
  (cards nil)
  (discards nil)
  (points 0)
  )

(defmacro with-player (player &body body)
  `(with-slots (name discarder cards discards points) ,player
    ,@body))

(defun minimal-discarder (cards dealer-or-pone)
  (list (subseq cards 0 2)
        (subseq cards 2 6)))

(defparameter minimal-player
  (make-player
    :name 'minimal-player
    :discarder #'minimal-discarder
    ))

(defun deal-cards (game-state &optional (random-state (make-random-state t)))
  (with-game game-state
    (let ((deck (shuffled-deck random-state)))
      (with-player dealer
        (setf cards (subseq deck 0 6)))
      (with-player pone
        (setf cards (subseq deck 6 12)))
      (setf starter (nth 12 deck)))))

(defun discard (game-state)
  (with-game game-state
    (with-player dealer
      (dbind (two-cards four-cards) (funcall discarder cards :dealer)
        (setf cards four-cards)
        (setf crib two-cards)))
    (with-player pone
      (dbind (two-cards four-cards) (funcall discarder cards :pone)
        (setf cards four-cards)
        (setf crib (append crib two-cards))))))

(defun have-cards (game-state)
  (with-game game-state
    (or (player-cards dealer) (player-cards pone))))

(defun toggle-next-to-play (game-state)
  (with-game game-state
    (ecase next-to-play
      (:dealer (setf next-to-play :pone))
      (:pone (setf next-to-play :dealer)))))
          


;(defun play-round (game-state random-state)
;  (deal-cards game-state random-state)
;  (discard game-state)
;  (while (have-cards game-state)
;         (do-play game-state dealer pone))
;  (show-pone game-state)
;  (show-dealer game-state)
;  )

(deftest test-game()
  (with-game (make-game-state 
               :dealer (make-player :name "fred" :discarder #'minimal-discarder)
               :pone (make-player :name "mike" :discarder #'minimal-discarder))
    (combine-results
      (check (not (have-cards game-state)))
      (progn (deal-cards game-state (make-random-state t)) t)
      (check (=== 6 (length dealer-cards)))
      (check (=== 6 (length pone-cards)))
      (check (not (null starter)))
      (check (null (intersection dealer-cards pone-cards)))
      (check (not (member starter pone-cards)))
      (check (not (member starter dealer-cards)))

      (progn (discard game-state) t)
      (check (=== 4 (length dealer-cards)))
      (check (=== 4 (length crib)))
      (check (=== 4 (length pone-cards)))
      (check (have-cards game-state))

      (check (=== :dealer next-to-play))
      (toggle-next-to-play game-state)
      (check (=== :pone next-to-play))
      (toggle-next-to-play game-state)
      (check (=== :dealer next-to-play))

      )))
