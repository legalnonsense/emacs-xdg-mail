#!/bin/bash


emacsclient -eval '(xdg-email-parser "'$1'")'

