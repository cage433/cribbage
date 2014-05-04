#!/bin/bash

while [ true ]; do
  inotifywait -q -e modify *.lisp play/*.lisp lisp-utils/*.lisp
  tmux set -q -g status-bg black
  rlwrap sbcl --noinform --load load --eval '(in-package :cage433-cribbage)' --eval '(ci)'
  if [[ $? == 0 ]]; then
    tmux set -q -g status-bg blue
  else
    tmux set -q -g status-bg red
  fi
done

