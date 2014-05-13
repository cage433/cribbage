(in-package :cage433-cribbage)





(defun playable-cards (game-state player)
  (with-game game-state
    (let ((max-rank-value (max-playable-rank)))
      (with-player player
        (remove-if (lambda (card) (> (card-rank-value card) max-rank-value)) cards-in-hand)))))


(defun minimal-discarder (cards dealer-or-pone)
  (declare (ignorable dealer-or-pone))
  (list (subseq cards 0 2)
        (subseq cards 2 6)))


(defun minimal-choose-play-cards (cards game-state)
  (declare (ignorable game-state))
  (assert cards () "Can't choose from empty hand")
  (list (car cards) (cdr cards)))

(define-condition leave-game (condition) ())

(defun read-card-indices(n)
  (labels ((usage() 
             (format t  "Enter 'q' to quit or ~a space separated number(s)~%" n)
             (read-card-indices n)))
    (let ((numbers
            (handler-case 
              (let ((input (cl-utilities:split-sequence #\Space (read-line) :remove-empty-subseqs t)))
                (cond ((null input) (usage))
                      ((string-equal "q" (string-downcase (car input))) (error 'leave-game))
                      (t (handler-case 
                          (mapcar #'parse-integer input)
                            (parse-error () (usage)))))))))
      (if (= (length numbers) n)
        (mapcar #'1- numbers) ; convert to zero based indices
        (usage)))))

(defun human-choose-discard (cards dealer-or-pone)
  (declare (ignorable cards dealer-or-pone))
  (clear-screen)
  (display-cards cards)
  (format t "Choose discards ~%")
  (let ((chosen-indices (read-card-indices 2)))
    (list
      (mapcar #_(nth _ cards) chosen-indices)
      (mapcar #_(nth _ cards) (set-difference '(0 1 2 3 4 5) chosen-indices)))))

(defun human-choose-play-cards (cards game-state)
  (with-game game-state
    (let ((max-rank-value (max-playable-rank)))
      (if (find-if #_(<= (card-rank-value _) max-rank-value) cards)
        (progn
          (format t "Choose card ~%")
          (let ((card (nth (car (read-card-indices 1)) cards)))
            (if (<= (card-rank-value card) max-rank-value)
              (list card (remove-if #_(eq _ card) cards))
              (progn 
                (format t "~a is too high valued~%" (card-to-short-string card))
                (human-choose-play-cards cards game-state)))))
        (list nil cards)))))

(defun minimal-player (name)
  (make-player
    :name name
    :discarder #'minimal-discarder
    :choose-play-card #'minimal-choose-play-cards
    ))

(defun human-player (name)
  (make-player
    :name name
    :discarder #'human-choose-discard 
    :choose-play-card #'human-choose-play-cards
    :is-human t
    ))

(defun deal-cards (game-state &optional (random-state (make-random-state t)))
  (with-game game-state
    (let ((deck (shuffled-deck random-state)))
      (with-player dealer
        (setf cards-in-hand (subseq deck 0 6)))
      (with-player pone
        (setf cards-in-hand (subseq deck 6 12)))
      (setf starter (nth 12 deck)))))

(defun discard (game-state)
  (with-game game-state
    (dbind (two-cards four-cards) (funcall dealer-discarder dealer-cards-in-hand :dealer)
      (setf dealer-show-cards four-cards)
      (setf dealer-cards-in-hand four-cards)
      (setf crib two-cards))
    (dbind (two-cards four-cards) (funcall pone-discarder pone-cards-in-hand :pone)
      (setf pone-show-cards four-cards)
      (setf pone-cards-in-hand four-cards)
      (setf crib (append crib two-cards)))))

(defun have-cards-to-play (game-state)
  (with-game game-state
    (or dealer-cards-in-hand pone-cards-in-hand)))

(defun toggle-next-to-play (game-state)
  (with-game game-state
    (ecase next-to-play
      (:dealer (setf next-to-play :pone))
      (:pone (setf next-to-play :dealer)))))
          


(defun do-play (game-state)
  (print-game-state game-state)
  (with-game game-state
    (format t "~%Played cards~%    ~a~%~%" (cards-as-string play-cards))
    (with-player (ecase next-to-play
                   (:dealer dealer)
                   (:pone pone))
      (if cards-in-hand
        (dbind (play-card remainder) 
               (funcall choose-play-card cards-in-hand game-state)
          (when play-card
            (setf play-cards (append play-cards (list play-card)))
            (incf points (play-value play-cards))
            (setf cards-in-hand remainder))))
      (toggle-next-to-play game-state))))

(defun show-pone (game-state)
  (declare (ignorable game-state)))

(defun show-dealer (game-state)
  (declare (ignorable game-state)))

(defun play-round (game-state random-state )
  (with-game game-state
    (deal-cards game-state random-state)
    (discard game-state)
    (while (or dealer-cards-in-hand pone-cards-in-hand)
      (while (or (playable-cards game-state dealer) (playable-cards game-state pone))
            (do-play game-state ))
      (setf play-cards nil))
    (show-pone game-state)
    (show-dealer game-state)))

(deftest test-game()
  (let ((game-state (make-game-state 
               :dealer (minimal-player "fred")
               :pone (minimal-player "mike"))))
    (with-game game-state
      (combine-results
        (check (not (have-cards-to-play game-state)))
        (progn (deal-cards game-state (make-random-state t)) t)
        (check (=== 6 (length dealer-cards-in-hand)))
        (check (=== 6 (length pone-cards-in-hand)))
        (check (not (null starter)))
        (check (null (intersection dealer-cards-in-hand pone-cards-in-hand)))
        (check (not (member starter pone-cards-in-hand)))
        (check (not (member starter dealer-cards-in-hand)))

        (progn (discard game-state) t)
        (check (=== 4 (length (playable-cards game-state dealer))))
        (check (=== 4 (length dealer-show-cards)))
        (check (=== 4 (length dealer-cards-in-hand)))
        (check (=== 4 (length crib)))
        (check (=== 4 (length pone-show-cards)))
        (check (=== pone-show-cards pone-cards-in-hand :test #'equal))
        (check (have-cards-to-play game-state))

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
        (check (for-all #_(> (card-rank-value _) (max-playable-rank))
                        (append dealer-cards-in-hand pone-cards-in-hand)))))))


