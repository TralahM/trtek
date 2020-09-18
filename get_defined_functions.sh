#!/usr/bin/env bash

cat src/{*.lisp,gps/*.lisp} | grep "^(defun "|awk -F" " '{print $2 "    ;; " $3 " " $4 " " $5 " " $6 " " $7}' |sort|uniq
