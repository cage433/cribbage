#!/bin/bash

while [ true ]; do
  inotifywait -q -e modify *.lisp
  tmux set -q -g status-bg black
  rlwrap sbcl --load play --eval '(in-package :cage433-cribbage)' --eval '(run-tests)'
  if [[ $? == 0 ]]; then
    tmux set -q -g status-bg blue
  else
    tmux set -q -g status-bg red
  fi
done

