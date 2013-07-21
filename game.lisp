(defstruct game-state
  dealer
  pone
  (crib nil)
  starter
  play-cards
  discards
  (next-to-play :dealer))

(defmacro with-game (game-state &body body)
  `(with-slots (dealer pone crib starter play-cards discards next-to-play) ,game-state
     (labels ((current-play-points() (apply #'+ (mapcar #'card-rank-value play-cards))))
      (with-slots ((dealer-cards cards) (dealer-crib crib) (dealer-discarder discarder) (dealer-name name)) dealer
        (with-slots ((pone-cards cards) (pone-crib crib) (pone-discarder discarder) (pone-name name)) pone
      ,@body)))))

(defstruct player
  name
  discarder
  choose-play-card
  (cards nil)
  (crib nil)
  (points 0))

(defmacro with-player (player &body body)
  `(with-slots (name discarder choose-play-card cards points) ,player
     (labels ((playable-cards() (remove-if-not (lambda (card) (> (card-rank-value card) (- 31 (current-play-points)))))))
    ,@body)))

(defun print-full-game-state (game-state &key (point-of-view nil))
  (with-game game-state
    (clear-screen)
    (format t "Dealer ~a~%Pone ~a~%" dealer-name pone-name)
    (when crib
      (format t "Crib~%~0,4T")
      (display-cards crib))
    (if (player-cards dealer)
      (progn
        (format t "Dealer~%~0,4T")
        (display-cards (player-cards dealer)))
      (format t "Dealer has no cards~%"))
    (if (player-cards pone)
      (progn
        (format t "Dealer~%~0,4T")
        (display-cards (player-cards pone)))
      (format t "Dealer has no cards~%"))))

(defun minimal-discarder (cards dealer-or-pone)
  (declare (ignorable dealer-or-pone))
  (list (subseq cards 0 2)
        (subseq cards 2 6)))


(defun minimal-choose-play-cards (cards game-state)
  (declare (ignorable game-state))
  (assert cards () "Can't choose from empty hand")
  (list (car cards) (cdr cards)))

(defun human-choose-discard (cards dealer-or-pone)
  (declare (ignorable cards dealer-or-pone))
  (display-cards cards)
  (format t "Discard two cards [1-6]~%")
  (let ((chosen-indices (mapcar #'1- (read-numbers))))
    (list
      (mapcar (lambda (k) (nth k cards)) chosen-indices)
      (mapcar (lambda (k) (nth k cards))
              (set-difference '(0 1 2 3 4 5) chosen-indices)))))

(defun human-choose-play-cards (cards game-state)
  (declare (ignorable game-state))
  (display-cards cards)
  (format t "Choose card ")
  (let* ((i (read ))
         (card (nth i cards))
         (remainder (remove-if (lambda (c) (eq c card)) cards)))
    (list card remainder)))

(defun minimal-player (name)
  (make-player
    :name name
    :discarder #'minimal-discarder
    :choose-play-card #'minimal-choose-play-cards
    ))

(defun human-player ()
  (make-player
    :name "fred"
    :discarder #'human-choose-discard 
    :choose-play-card #'human-choose-play-cards
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
        (dbind (play-card remainder) 
               (funcall choose-play-card cards game-state)
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
  (format t "Dealing~%")
  (print-full-game-state game-state)
  (discard game-state)
  (print-full-game-state game-state)
  (format t "Play~%")
  (while (have-cards game-state)
         (do-play game-state))
  (format t "Show~%")
  (show-pone game-state)
  (show-dealer game-state)
  )

(format t "Making test-game~%")
(deftest test-game()
  (let ((game-state (make-game-state 
               :dealer (minimal-player "fred")
               :pone (minimal-player "mike"))))
    (with-game game-state
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
        ))))


(deftest test-game2()
  (let ((game-state (make-game-state 
               :dealer (minimal-player "fred")
               :pone (minimal-player "mike"))))
  (with-game game-state
    (combine-results
      (progn (play-round game-state (make-random-state t)) t)
      (check (=== 8 (length discards)))))))
