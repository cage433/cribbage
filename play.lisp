(load "load")

(in-package :cage433-cribbage)

(defun p()
  (let ((game-state (make-game-state 
               :dealer (human-player "alex")
               :pone (minimal-player "mike"))))
    (play-round game-state (make-random-state t))))

