; Author: Tralah M Brian
;
; Copyright © 2020 Tralah M Brian

; Permission is hereby granted, free of charge, to any person obtaining
; a copy of this software and associated documentation files (the "Software"),
; to deal in the Software without restriction, including without limitation
; the rights to use, copy, modify, merge, publish, distribute, sublicense,
; and/or sell copies of the Software, and to permit persons to whom the
; Software is furnished to do so, subject to the following conditions:

; The above copyright notice and this permission notice shall be included
; in all copies or substantial portions of the Software.

; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
; OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
; IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,
; DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
; TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE
; OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

(in-package :trtek)


(defun sigmoid (x)
  "The sigmoid function "
  (/ 1.0 (+ 1.0 (exp (- x)))))

(defun sigmoid* (x)
  "Integral of the sigmoid function "
  (let ((temp (sigmoid x)))
    (* temp (- 1.0 temp))))

;; blah blah blah.
(defun cmplmnt (fn)
  "complement of a function `fn' i.e not fn"
  #'(lambda (&rest args) (not (apply fn args))))


;; (remove-if (cmplmnt #'oddp) '(1 2 3 4 5 6)) ;=> (1 3 5)
;;



; # Returning Destructive Elements

;; (defvar *!equivs* (make-hash-table))

;; (defun ! (fn)
;;   (or (gethash fn *!equivs*) fn))

;; (defun def! (fn fn!)
;;   (setf (gethash fn *!equivs*) fn!))

;; (def! #'remove-if #'delete-if)

;; instead of (delete-if #'oddp lst)
;; we would say (funcall (! #'remove-if) #'oddp lst)

;Memoizing utility
(defun memoize (fn)
  "Memoizing utility for expensive function calls"
  (let ((cache (make-hash-table :test #'equal)))
    #'(lambda (&rest args)
        (multiple-value-bind (val win) (gethash args cache)
          (if win
              val
              (setf (gethash args cache)
                    (apply fn args)))))))


;; (setq slowid (memoize #'(lambda (x) (sleep 5) x)))

;; (time (funcall slowid 1));; 5.15 seconds

;; (time (funcall slowid 1));; 0.00 seconds



;; Composing Functions

(defun compose (&rest fns)
  "Composing Functions takes a list of functions and returns a composed function in order of specification"
  (if fns
      (let ((fn1 (car (last fns)))
            (fns (butlast fns)))
        #'(lambda (&rest args)
            (reduce #'funcall fns :from-end t
                    :initial-value (apply fn1 args))))
      #'identity))

;; eg (compose #'list #'1+) returns a fx equivalent to #'(lambda (x) (list (1+ x)))
;; (funcall (compose #'1+ #'find-if) #'oddp '(2 3 4)) ;==> 4


; More function builders

(defun fif (if then &optional else)
  #'(lambda (x)
  (if (funcall if x)
      (funcall then x)
    (if else (funcall else x)))))


(defun fint (fn &rest fns)
  fn
  (let ((chain (apply #'fint fns)))
    #'(lambda (x)
        (and (funcall fn x) (funcall chain x))))
  )

(defun fun (fn &rest fns)
  if (null fns)
  fn
  (let ((chain (apply #'fun fns)))
    #'(lambda (x)
        (or (funcall fn x) (funcall chain x)))))



;;For the case where you want users to be able to type in expressions without
;;parenthenses; it reads a line of input and returns it as a list
(defun readlist (&rest args)
  (values (read-from-string
            (concatenate 'string "("
                         (apply #'read-line args)
                         ")"))))

;; (readlist)
;;; call me "Ed"
;;;=> (CALL ME "Ed")

;; The function prompt combines printing a question and reading the answer.It
;; takes the arguments of format,except the initial stream argument
(defun prompt (&rest args)
  "The function prompt combines printing a question and reading the answer.It takes the arguments of format,except the initial stream argument
"
  (apply #'format *query-io* args)
  (read *query-io*))

;; (prompt "Enter a number between ~A and ~A. ~%>> " 1 10)
;;;>>> 3
;;; => 3


;; break-loop is for situations where you want to imitate the Lisp toplevel.
;;It takes 2 functions and an &rest argument,which is repeatedly given to
;;prompt. As long as the second function returns false for the input, the first
;;function is aplied to it
(defun break-loop (fn quit &rest args)
  (format *query-io* "Entering break-loop ~%")
  (loop
    (let ((in (apply #'prompt args)))
      (if (funcall quit in)
          (return)
          (format *query-io* "~A~%" (funcall fn in))))))

; (break-loop #'eval #'(lambda (x) (eq x :q)) ">> ")
;;=> Entering break-loop.
;;;>> (+ 2 3)
;;;=> 5
;;; >> :q
;;;=> :Q
; Utility Functions for Operations on Lists
; Small Functions which operate on lists

(proclaim '(inline last1 single append1 conc1 mklist))

;; last element in a list
(defun last1 (lst)
  "get last element in a list `lst'"
  (car (last lst)))

;; test whether lst is a list of one element
(defun single (lst)
  "test whether `lst' is a list of one element"
  (and (consp lst) (not (cdr lst))))

;; attach a new element to end of a list non-destructively
(defun append1 (lst obj)
  "attach a new element `obj' to end of a list `lst' non-destructively"
  (append lst (list obj)))

;; attach a new element to end of a list destructively
(defun conc1 (lst obj)
  "attach a new element `obj' to end of a list `lst' destructively"
  (nconc lst (list obj)))

;; Ensure obj is a list
(defun mklist (obj)
  "Ensure `obj' is a list"
  (if (listp obj) obj (list obj)))


; Longer Functions That Operate on Lists

;; Check whether a list x is longer than a list y
(defun longer (x y)
  "Check whether a list `x' is longer than a list `y'"
  (labels ((compare (x y)
                    (and (consp x) (or (null y)
                                       (compare (cdr x) (cdr y))))))
    (if (and (listp x) (listp y)) (compare x y) (> (length x) (length y)))))

;; Apply filter function fn to list lst
(defun filter (fn lst)
  "Apply filter function `fn' to list `lst'"
  (let ((acc nil))
    (dolist (x lst)
      (let ((val (funcall fn x)))
        (if val (push val acc))))
    (nreverse acc)))

;; Groups List into Sublists of Length n, remainder stored in last sublist
(defun group (source n)
  "Groups List `source' into Sublists of Length `n', remainder stored in last sublist"
  (if (zerop n) (error "Zero length"))
  (labels ((rec (source acc)
                (let ((rest (nthcdr n source)))
                  (if (consp rest)
                    (rec rest (cons (subseq source 0 n) acc))
                    (nreverse (cons source acc))))))
    (if source (rec source nil) nil)))

; Doubly Recursive List Utilities

;; Flatten List lst with Nested Lists
(defun flatten (x)
  "Flatten List `x' with Nested Lists"
  (labels ((rec (x acc)
                (cond ((null x) acc)
                      ((atom x) (cons x acc))
                      (t (rec (car x) (rec (cdr x) acc))))))
    (rec x nil)))

;; Prune List with Nested Lists using the function test
(defun prune (test tree)
  "Prune List `tree' with Nested Lists using the function `test'"
  (labels ((rec (tree acc)
                (cond ((null tree) (nreverse acc))
                      ((consp (car tree))
                       (rec (cdr tree)
                            (cons (rec (car tree) nil) acc)))
                      (t (rec (cdr tree)
                              (if (funcall test (car tree))
                                acc
                                (cons (car tree) acc)))))))
    (rec tree nil)))

;; Another widely used class of Lisp functions are the mapping functions which
;; apply a function to a sequence of arguments.
;; both map0-n and map1-n are written using the general form mapa-b, which works
;; for any range of numbers and not only for ranges of positive integers
(defun mapa-b (fn a b &optional (step 1))
  "mapa-b, mapping function which ;; applies a function to a sequence of arguments and works for any range of numbers and not only for ranges of positive integers"
  (do ((i a (+ i step))
       (result nil))
      ((> i b) (nreverse result))
    (push (funcall fn i) result)))


;; (mapa-b #'1+ -2 0 .5);====> (-1 -0.5 0.0 0.5 1.0)



(defun map0-n (fn n)
  (mapa-b fn 0 n))

(defun map1-n (fn n)
  (mapa-b fn 1 n))


;; (map0-n #'1+ 5); ===> (1 2 3 4 5 6)

;; (map1-n #'1+ 5); ===> (2 3 4 5 6)

(defun map-> (fn start test-fn succ-fn)
  (do ((i start (funcall succ-fn i))
       (result nil))
      ((funcall test-fn i) (nreverse result))
    (push (funcall fn i) result)))

(defun mappend (fn &rest lsts)
  (apply #'append (apply #'mapcar fn lsts)))

;; The utility mapcars is for cases where we want to mapcar a function over
;; several lists. If we have two lists of numbers and we want to get a single
;; list of the square roots of both we could say in raw lisp (mapcar #'sqrt
;; (append list1 list2)) or using mapcars
(defun mapcars (fn &rest lsts)
  "The utility mapcars is for cases where we want to mapcar a function over several lists. "
  (let ((result nil))
    (dolist (lst lsts)
      (dolist (obj lst)
        (push (funcall fn obj) result)))
    (nreverse result)))

;; (mapcars #'sqrt '(1 2 4 6 7 9) '( 25 81 625 225))

;; Recursive mapcar a version of mapcar for trees and does what mapcar does on
;; flat lists, it does on trees
(defun rmapcar (fn &rest args)
  "Recursive mapcar a version of mapcar for trees and does what mapcar does on flat lists, it does on trees"
  (if (some #'atom args)
      (apply fn args)
      (apply #'mapcar
             #'(lambda (&rest args)
                 (apply #'rmapcar fn args))
             args)))
;; (rmapcar #'sqrt '(1 2 4 6 7 9 ( 25 81 625 225)))
; Functions which search lists
(defun find2 (fn lst)
  (if (null lst)
      nil
      (let ((val (funcall fn (car lst))))
        (if val
            (values (car lst) val)
            (find2 fn (cdr lst))))))

(defun before (x y lst &key (test #'eql))
  (and lst
       (let ((first (car lst)))
         (cond ((funcall test y first) nil)
               ((funcall test x first) lst)
               (t (before x y (cdr lst) :test test))))))
;;; (before 'a 'b '(a)) ; ==> (A)

(defun after (x y lst &key (test #'eql))
  (let ((rest (before y x lst :test test)))
    (and rest (member x rest :test test))))
;;; (after 'a 'b '(b a d)); ==> (A D)
;;; (after 'a 'b '(a)) ; ==> NIL

;; Check whether list lst contains duplicate obj using some test default
;; equality
(defun duplicate (obj lst &key (test #'eql))
  (member obj (cdr (member obj lst :test test))
          :test test))

;;; (duplicate 'a '(a b c a d)) ;==> (A D)

(defun split-if (fn lst)
  (let ((acc nil))
    (do ((src lst (cdr src)))
        ((or (null src) (funcall fn (car src)))
         (values (nreverse acc) src))
      (push (car src) acc))))

;;; (split-if #'(lambda (x) (> x 4))
; '(1 2 3 4 5 6 7 8 9 10))
;;; =>  (1 2 3 4)
;;; =>  (5 6 7 8 9 10)

; Search Functions which compare elements

(defun most (fn lst)
  (if (null lst)
      (values nil nil)
      (let* ((wins (car lst))
             (max (funcall fn wins)))
        (dolist (obj (cdr lst))
          (let ((score (funcall fn obj)))
            (when (> score max)
              (setq wins obj max score))))
        (values wins max))))
; (most #'length '((a b) (a b c) (a) (e f g))) ;==> (A B C) ;===> 3

(defun best (fn lst)
  (if (null lst)
      nil
      (let ((wins (car lst)))
        (dolist (obj (cdr lst))
          (if (funcall fn obj wins)
              (setq wins obj)))
        wins)))

; (best #'> '(1 2 3 4 5)) ; ==> 5

(defun mostn (fn lst)
  (if (null lst)
      (values nil nil)
      (let ((result (list (car lst)))
            (max (funcall fn (car lst))))
        (dolist (obj (cdr lst))
          (let ((score (funcall fn obj)))
            (cond ((> score max)
                   (setq max score result (list obj)))
                  ((= score max)
                   (push obj result)))))
        (values (nreverse result) max))))


; (mostn #'length '((ab) (a b c) (a) (e f g))) ;==>((A B C) (E F G)) ; ==> 3
;Functions which operate on symbols and strings

; Symbols and strings are closely related.By means of priinting and reading
; functions we can go back and forth between the two representation.


;; The first,mkstr takes any number of arguments and concatenates their printed
;; representations into a string:
;;; Built upon symb

(defun mkstr (&rest args)
  "mkstr takes any number of arguments and concatenates their printed representations into a string"
  (with-output-to-string (s)
    (dolist (a args) (princ a s))))


; (mkstr pi " pieces of " 'pi) ;===> "3.141592653589793 pieces of PI"

(defun symb (&rest args)
  (values (intern (apply #'mkstr args))))

;; Generalization of symb it takes a series of objects,prints and rereads them.
;; it can return symbols like symb,but it can also return anything else read can
(defun reread (&rest args)
  (values (read-from-string (apply #'mkstr args))))


;; takes a symbol and returns a list of symbols made from the characters in
;; its name
(defun explode (sym)
  (map 'list #'(lambda (c)
                 (intern (make-string 1 :initial-element c)))
       (symbol-name sym)))

; (explode 'bomb);==> (B O M B)

; (explode 'tralahtek)

(defun cumsum (lst &key (smsf 0))
  " Calculate the Cumulative Sum of a List `lst'. and return a new list with the incremental sums at each step.  (`cumsum' '(1 3 4 6 8) :smsf 0) where `:smsf' is an optional parameter specifying where to start summing from. i.e the offset of counting."
  (if (null lst)
      '()
    (cons (+ smsf (car lst))
          (funcall #'cumsum (cdr lst) :smsf (+ smsf (car lst))))))
