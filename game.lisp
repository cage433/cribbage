(in-package :cage433-cribbage)

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
     (let ((sorted-players (sort (list dealer pone) (lambda (x y) (string< (player-name x) (player-name y))))))
      (labels ((current-play-points() (apply #'+ (mapcar #'card-rank-value play-cards))))
        (declare (ignorable #'current-play-points))
        (with-slots ((dealer-cards cards) (dealer-crib crib) (dealer-discarder discarder) (dealer-name name)) dealer
          (with-slots ((pone-cards cards) (pone-crib crib) (pone-discarder discarder) (pone-name name)) pone
        ,@body))))))

(defstruct player
  name
  discarder
  (is-human nil)
  choose-play-card
  (cards nil)
  (crib nil)
  (points 0))

(defmacro with-player (player &body body)
  `(with-slots (name discarder is-human choose-play-card cards points) ,player
     (labels ((playable-cards() (remove-if-not (lambda (card) (> (card-rank-value card) (- 31 (current-play-points)))) cards)))
      (declare (ignorable #'playable-cards))
    ,@body)))

(defun print-full-game-state (game-state &rest viewable)
  (with-game game-state
    (clear-screen)
    (mapc #_(with-player _
             (format t "~a ~a ~a ~%    ~{~a~^ ~}~%" 
                     name 
                     (if (equal dealer _) " (D) " "     ")
                     points 
                     (if is-human 
                       (mapcar #'card-to-short-string cards)
                       (make-list (length cards) :initial-element "XX"))))
          sorted-players))
    
    )

(defun minimal-discarder (cards dealer-or-pone)
  (declare (ignorable dealer-or-pone))
  (list (subseq cards 0 2)
        (subseq cards 2 6)))


(defun minimal-choose-play-cards (cards game-state)
  (declare (ignorable game-state))
  (assert cards () "Can't choose from empty hand")
  (list (car cards) (cdr cards)))

(define-condition leave-game (condition) ())

(defun read-numbers()
  (handler-case 
    (let ((input (cl-utilities:split-sequence #\Space (read-line) :remove-empty-subseqs t)))
      (cond ((null input) (format t "Enter 'q' to quit or space separated number(s)") (read-numbers))
            ((string-equal "q" (string-downcase (car input))) (error 'leave-game))
            (t (handler-case 
                (mapcar #'parse-integer input)
                (parse-error (e) (progn (format t  "Enter 'q' to quit or space separated numbers - couldn't parse ~a~%" input)
                                        (read-numbers)))))))))

(defun human-choose-discard (cards dealer-or-pone)
  (declare (ignorable cards dealer-or-pone))
  (clear-screen)
  (display-cards cards)
  (format t "Choose discards ~%")
  (let ((chosen-indices (mapcar #'1- (read-numbers))))
    (list
      (mapcar #_(nth _ cards) chosen-indices)
      (mapcar #_(nth _ cards) (set-difference '(0 1 2 3 4 5) chosen-indices)))))

(defun human-choose-play-cards (cards game-state)
  (with-game game-state
    (let ((max-rank-value (- 31 (current-play-points))))
      (format t "Choose card ~%")
      (let* ((i (1- (car (read-numbers))))
            (card (nth i cards))
            (remainder (remove-if #_(eq _ card) cards)))
        (list card remainder)))))

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
  (print-full-game-state game-state)
  (with-game game-state
    (format t "~%Played cards~%    ~a~%~%" (cards-as-string play-cards))
    (with-player (ecase next-to-play
                   (:dealer dealer)
                   (:pone pone))
      (if cards
        (dbind (play-card remainder) 
               (funcall choose-play-card cards game-state)
          (when play-card
            (setf play-cards (append play-cards (list play-card)))
            (incf points (play-points play-cards))
            (setf discards (cons play-card discards))
            (setf cards remainder)
            (format t "~a plays ~a~%" name (card-to-short-string play-card)))))
      (toggle-next-to-play game-state))))

(defun show-pone (game-state)
  (declare (ignorable game-state)))

(defun show-dealer (game-state)
  (declare (ignorable game-state)))

(defun play-round (game-state random-state )
  (deal-cards game-state random-state)
  (discard game-state)
  (while (have-cards game-state)
         (do-play game-state ))
  (show-pone game-state)
  (show-dealer game-state)
  )

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

