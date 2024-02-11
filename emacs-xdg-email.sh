#!/bin/bash


emacsclient -eval '(jrf/xdg-email-parser "'$1'")'

