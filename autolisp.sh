#!/bin/bash

# tmux new-session [-d] [-n window-name] [-s session-name] [-t target-session] [shell-command]

echo "starting autolisp"
echo "reattach with 'tmux attach -t autolisp'"

tmux new-session -d -s autolisp 'sbcl --load /home/wfraser/lisp/serv.lisp'

