(load "cards")

(defstruct game-state
  dealer
  pone
  (crib nil)
  starter
  play-cards
  discards
  (next-to-play :dealer)
  )

(defmacro with-game (game-state &body body)
  `(let ((game-state ,game-state))
    (with-slots (dealer pone crib starter play-cards discards next-to-play) game-state
      (with-slots ((dealer-cards cards) (dealer-discarder discarder)) dealer
        (with-slots ((pone-cards cards) (pone-discarder discarder)) pone

      ,@body)))))

(defstruct player
  name
  discarder
  choose-play-card
  (cards nil)
  (points 0)
  )

(defmacro with-player (player &body body)
  `(with-slots (name discarder choose-play-card cards points) ,player
    ,@body))

(defun minimal-discarder (cards dealer-or-pone)
  (declare (ignorable dealer-or-pone))
  (list (subseq cards 0 2)
        (subseq cards 2 6)))

(defun minimal-choose-play-cards (cards game-state)
  (declare (ignorable game-state))
  (assert cards () "Can't choose from empty hand")
  (list (car cards) (cdr cards)))

(defun minimal-player (name)
  (make-player
    :name name
    :discarder #'minimal-discarder
    :choose-play-card #'minimal-choose-play-cards
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
    (dbind (two-cards four-cards) (funcall dealer-discarder dealer-cards :dealer)
      (setf dealer-cards four-cards)
      (setf crib two-cards))
    (dbind (two-cards four-cards) (funcall pone-discarder pone-cards :pone)
      (setf pone-cards four-cards)
      (setf crib (append crib two-cards)))))

(defun have-cards (game-state)
  (with-game game-state
    (or dealer-cards pone-cards)))

(defun toggle-next-to-play (game-state)
  (with-game game-state
    (ecase next-to-play
      (:dealer (setf next-to-play :pone))
      (:pone (setf next-to-play :dealer)))))
          
(defun play-points (play-cards)
  (declare (ignorable play-cards))
  0)

(defun do-play (game-state)
  (with-game game-state
    (with-player (ecase next-to-play
                   (:dealer dealer)
                   (:pone pone))
      (if cards
        (dbind (play-card remainder) (funcall choose-play-card cards game-state)
          (when play-card
            (setf play-cards (cons play-card play-cards))
            (incf points (play-points play-cards))
            (setf discards (cons play-card discards))
            (setf cards remainder))))
      (toggle-next-to-play game-state))))

(defun show-pone (game-state)
  (declare (ignorable game-state)))

(defun show-dealer (game-state)
  (declare (ignorable game-state)))

(defun play-round (game-state random-state)
  (deal-cards game-state random-state)
  (discard game-state)
  (while (have-cards game-state)
         (do-play game-state))
  (show-pone game-state)
  (show-dealer game-state)
  )

(deftest test-game()
  (with-game (make-game-state 
               :dealer (minimal-player "fred")
               :pone (minimal-player "mike"))
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

(deftest test-game2()
  (with-game (make-game-state 
               :dealer (minimal-player "fred")
               :pone (minimal-player "mike"))
    (combine-results
      (progn (play-round game-state (make-random-state t)) t)
      (check (=== 8 (length discards))))))
