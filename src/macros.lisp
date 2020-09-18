(in-package :trtek)

(defmacro nil! (var)
  "set var to nil"
  `(setq ,var nil))

(defmacro while (test &body body)
  `(do ()
       ((not ,test))
     ,@body))

(defmacro fn (expr) `#',(rbuild expr))

(defun rbuild (expr)
  (if (or (atom expr) (eq (car expr) 'lambda))
      expr
      (if (eq (car expr) 'compose)
          (build-compose (cdr expr))
          (build-call (car expr) (cdr expr)))))

(defun build-call (op fns)
  (let ((g (gensym)))
    `(lambda (,g)
       (,op ,@(mapcar #'(lambda (f)
                          `(,(rbuild f) ,g))
                      fns)))))

(defun build-compose (fns)
  (let ((g (gensym)))
    `(lambda (,g)
       ,(labels ((rec (fns)
                   (if fns
                       `(,(rbuild (car fns))
                          ,(rec (cdr fns)))
                       g)))
          (rec fns)))))


; (mapcar (fn (or integerp symbolp)) '(c 3 p 0.2))  => '(T T T NIL)
; (mapcar (fn (and integerp oddp)) '(c 3 p 0.2))  => '(NIL T NIL NIL)
; (mapcar (fn (list 1- identity 1+)) '(8 3 5 2))  => '((7 8 9) (2 3 4) (4 5 6) (1 2 3))
; (mapcar (fn (list integerp oddp)) '(8 3 5 2))  => '((T NIL) (T T) (T T) (T NIL))

;; Macros for list recursion

(defun lrec (rec &optional base)
  "Function to define flat list recursers.
  The first argument to lrec must be a function of two arguments,
  the current car of the list,and a function which can be called to continue the
  recursion."
  (labels ((self (lst)
             (if (null lst)
                 (if (functionp base)
                     (funcall base)
                     base)
                 (funcall rec (car lst)
                          #'(lambda () (self (cdr lst)))))))
    #'self))

;; Using lrec we coul express our-length as:
;; (funcall (lrec #'(lambda (x f) (1+ (funcall f))) 0) '(1 2 3 4 5 6 7))
;; Or Our-every as:
;; (funcall (lrec #'(lambda (x f) (and (oddp x) (funcall f))) t) '(1 2 3 4 5 6 7 8))
;; (funcall (lrec #'(lambda (x f) (and (oddp x) (funcall f))) t) '(1 3 5 7 9))

(defmacro alrec (rec &optional base)
  "clt12 version"
  (let ((gfn (gensym)))
    `(lrec #'(lambda (it ,gfn)
               (symbol-macrolet ((rec (funcall ,gfn)))
                 ,rec))
           ,base)))

(defmacro on-cdrs (rec base &rest lsts)
  `(funcall (alrec ,rec #'(lambda () ,base)) ,@lsts))


(defun tek-copy-list (lst)
  (on-cdrs (cons it rec) nil lst))

(defun tek-remove-duplicates (lst)
  (on-cdrs (adjoin it rec) nil lst))

(defun tek-find-if (fn lst)
  (on-cdrs (if (funcall fn it) it rec) nil lst))

(defun tek-some (fn lst)
  (on-cdrs (or (funcall fn it) rec) nil lst))

(defun tek-every (fn lst)
  (on-cdrs (or (funcall fn it) rec) t lst))


(defun unions (&rest sets)
  (on-cdrs (union it rec) (car sets) (cdr sets)))

; (unions '(a b c d e) '(a f d)) => '(E C B A F D)

(defun intersections (&rest sets)
  (unless (some #'null sets)
    (on-cdrs (intersection it rec) (car sets) (cdr sets))))

; (intersections '(a b c d e) '(a f d)) => '(D A)

(defun differences (set &rest outs)
  (on-cdrs (set-difference rec it) set outs))

; (differences '(a b c d e) '(a f) '(d)) => '(B C E)

(defun maxmin (args)
  (when args
    (on-cdrs (multiple-value-bind (mx mn) rec
               (values (max mx it) (min mn it)))
             (values (car args) (car args))
             (cdr args))))

; (maxmin '(3 4 2 8 5 1 6 7))  =>  8 1

(defmacro nif (expr pos zero neg)
  "Numerical If macro"
  `(case (truncate (signum ,expr))
     (1 ,pos)
     (0 ,zero)
     (-1 ,neg)))

(defmacro avg (&rest args)
  "Calculate the average of list macro."
  `(/ (+ ,@args) ,(length args)))

; (describe 'avg )
; (macroexpand `(avg 1 2 3 4 5 6 7 8))
; (macroexpand `(avg 1 2 5 4 7 6 8 86))
; (eval (macroexpand `(avg 1 2 5 4 7 6 8.0 86)))
; (time (eval (macroexpand `(avg 1 2 5 4 7 6 8.0 86))))
; (+ 1 2 5 4 7 6 8 86)
; (defvar x nil)
; (setq x nil)

(defmacro using (var form &body body)
  `(let ((,var ,form))
     (unwind-protect (progn ,@body)
       (progn (setq ,var nil) (format t  "Cleaning up ~a to ~a~%" 'var ,var)))))

(defun test-using (x val)
  (using x val
    (format t "Using ~a as ~a~%" 'x x)
    ))

; (test-using 'p 55.0)
