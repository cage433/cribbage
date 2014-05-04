(in-package :cage433-cribbage)

(defun play-value (cards)
  (+
    (play-fifteen-value cards)
    (play-pairs-and-higher-value cards)
    (play-run-value cards)
    (play-thirty-one-value cards)))

(defun play-a-card (game-state dealer-or-pone)
  (let ((player (game-player game-state dealer-or-pone)))
    (with-player player
      (awhen (funcall choose-play-card game-state play-cards dealer-or-pone)
        (with-game game-state
          (setf played-cards (cons it played-cards))
          (setf play-cards (remove-if #_(equal _ it) play-cards))
          (setf last-to-play (game-player game-state dealer-or-pone))
          (incf score (play-value played-cards))
          it)))))


(defun play-round (game-state)
  (with-game game-state
    (while (or (play-a-card game-state (car play-order))
                (play-a-card game-state (cadr play-order)))
      (setf play-order (reverse play-order)))
    (incf (player-score last-to-play))
    (setf discards (cons played-cards discards))
    (setf played-cards nil))
    game-state)


(defun pad-left (text len)
  (if (< (length text) len)
    (concatenate 'string (make-string (- len (length text)) :initial-element #\Space) text)
    text))

(defun map-tree (fn tree)
  (cond ((null tree) nil)
        ((consp tree)
          (cons (map-tree fn (car tree))
                (map-tree fn (cdr tree))))
        (t (funcall fn tree))))

(defun stringify (list-of-rows)
  (map-tree #_(format nil "~A" _) list-of-rows))

(defun col-widths (list-of-rows)
  (apply #'mapcar #'max (map-tree #'length (stringify list-of-rows))))

(defun tabulate (&rest list-of-rows)
  (let ((widths (col-widths list-of-rows))
        (list-of-text-rows (stringify list-of-rows)))
    (mapc (lambda (text-row)
            (format t "~{~A~^ ~}~%" 
                    (mapcar #'pad-left text-row widths)))
          list-of-text-rows)))

(defun print-full-game-state (game-state)
  (with-game game-state
    (labels ((player-row (player)
              (with-player player
                  (list name (format nil "~{~A~^ ~}" (mapcar {#_(pad-left _ 3) #'card-to-short-string} play-cards)) score))))
      (tabulate 
        (list "  Name" "  Cards" "  Score")
        (player-row dealer)
        (player-row pone))
      game-state)))

(defun test-play-round()
  (let ((game (initialise-game (make-minimal-player "fred") 
                               (make-minimal-player "mike"))))
    (with-game game
      (info "When playing a round with sufficiently low cards"
        (setup-game game)
        (setf dealer/play-cards (hand-from-string "AS AC AD AH"))
        (setf pone/play-cards (hand-from-string "2S 2C 2D 2H"))
        (play-round game)
        (spec "all cards will be played"
          (null dealer/play-cards)
          (null pone/play-cards)
          (= 8 (length (car discards))))
        (spec "One player will have at least one point"
          (plusp (+ dealer/score pone/score))))
      (info "when all players have 10 rank cards"
          (setup-game game)
          (setf dealer/play-cards (hand-from-string "10C JS"))
          (setf pone/play-cards (hand-from-string "QC KH"))
          (progn (setf played-cards nil) t)
          (print-full-game-state game)
          (play-round game)
          (print-full-game-state game)
        (spec "pone will have a single point"
          (= 1 pone/score)))
      )))




(defun do-play (game-state)
  (with-game game-state
    (while (or dealer/play-cards pone/play-cards)
      (play-round game-state))))

