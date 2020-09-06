#!/bin/bash

while [ true ]; do
  # Weirdly, unless the command is wrapped in a $() we can't Ctrl-C the script
  $(fswatch -1 *.lisp play/*.lisp lisp-utils/*.lisp > /dev/null 2>&1)
  tmux set -q -g status-bg black
  #rlwrap sbcl --noinform --load load --eval '(in-package :cage433-cribbage)' --eval '(ci)'
  rlwrap sbcl --noinform --load load --eval '(ci)'
  if [[ $? == 0 ]]; then
    tmux set -q -g status-bg blue
  else
    tmux set -q -g status-bg red
  fi
done


