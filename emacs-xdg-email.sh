#!/bin/bash


# emacsclient -eval '(progn (require '"'"'emacs-xdg-email) (xdg-email-parser "'$1'"))'
emacsclient -eval '(xdg-email-parser "'$1'")'
