#!/usr/bin/env bash

cat src/{*.lisp,gps/*.lisp} |egrep "^\(def.*"|awk -F" " '{print "\t" $2 "    ;; " $3 " " $4 " " $5 " " $6 " " $7}'|sort|uniq|tee functions.txt
