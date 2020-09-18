(in-package :trtek)

(defvar *epsilon* 1.0e-6)

(defun chars (str)
  "Get a list of all the characters in a string."
  (loop for c across str collect c))

; (chars "Get a list of all the characters in a string.")

(defun approx-equal (x y &optional (*epsilon* *epsilon*))
  "Check whether x and y are approximately equal.
  Useful for floating point numbers."
  (or (= x y)
      (< (/ (abs (- x y)) (max (abs x) (abs y))) *epsilon*)))

(defun fact (x)
  (if (= x 0)
      1
      (* x (fact (- x 1)))))

;; (fact 6) => 720

(defun choose (n r)
  (/ (fact n) (fact (- n r)) (fact r)))

;; (choose 4 2) => 6
;; (choose 6 2) => 15

(defun deg-to-rad (deg)
  "Convert `deg` degrees to radians"
  (* deg pi 1/180))

;; (deg-to-rad 180) => 3.141592653589793d0
;; (deg-to-rad 90) => 1.5707963267948966d0
;; (deg-to-rad 45) => 0.7853981633974483d0

(defun rad-to-deg (rad)
  "Convert `rad` radians to degrees"
  (* rad (/ 180 pi)))

;; (rad-to-deg pi) => 180.0d0
;; (rad-to-deg (* pi 2)) => 360.0d0
;; (rad-to-deg (* pi .5)) => 90.0d0


(defun cos-deg (angle)
  "Cosine of `angle` in Degrees"
  (cos (deg-to-rad angle)))

;; (cos-deg 60) => 0.5
;; (cos-deg 90) => 0.0
;; (cos-deg 30) => 0.866
;; (cos-deg 0) => 1.0d0
;; (cos-deg 45) => 0.7071

(defun acos-deg (x)
  "Arc-Cosine of `x` in Degrees"
  (rad-to-deg (acos x)))

;; (acos-deg .5) => 60.0
;; (acos-deg 0) => 90.0
;; (acos-deg 1) => 0.0d0
;; (acos-deg -1) => 180.0

(defun sin-deg (angle)
  "Sine of `angle` in Degrees"
  (sin (deg-to-rad angle)))

;; (sin-deg 60) => 0.866
;; (sin-deg 90) => 1.0d0
;; (sin-deg 30) => 0.5
;; (sin-deg 0) => 0.0d0
;; (sin-deg 45) => 0.7071

(defun asin-deg (x)
  "Arc-Sine of `x` in Degrees"
  (rad-to-deg (asin x)))

;; (asin-deg .5) => 30.0
;; (asin-deg 0) => 0.0d0
;; (asin-deg 1) => 90.0
;; (asin-deg -1) => -90.0

(defun tan-deg (angle)
  "Tangent of `angle` in Degrees"
  (tan (deg-to-rad angle)))

;; (tan-deg 45) => 1
;; (tan-deg 60) => 1.732
;; (tan-deg 30) => 0.577

(defun atan-deg (x)
  "Arc-Tangent of `x` in Degrees"
  (rad-to-deg (atan x)))

;; (atan-deg .5) => 26.5651
;; (atan-deg 0) => 0.0d0
;; (atan-deg 1.0) => 45.0

; (defun lrec (rec &optional base)
;   "Function to define flat list recursers.
;   The first argument to lrec must be a function of two arguments,
;   the current car of the list,and a function which can be called to continue the
;   recursion."
;   (labels ((self (lst)
;              (if (null lst)
;                  (if (functionp base)
;                      (funcall base)
;                      base)
;                  (funcall rec (car lst)
;                           #'(lambda () (self (cdr lst)))))))
;     #'self))

;; Using lrec we coul express our-length as:
;; (funcall (lrec #'(lambda (x f) (1+ (funcall f))) 0) '(1 2 3 4 5 6 7))
;; Or Our-every as:
;; (funcall (lrec #'(lambda (x f) (and (oddp x) (funcall f))) t) '(1 2 3 4 5 6 7 8))
;; (funcall (lrec #'(lambda (x f) (and (oddp x) (funcall f))) t) '(1 3 5 7 9))


;; Functions Built Using Lrec
;;;; copy-list
; (lrec #'(lambda (x f) (cons x (funcall f))))

;;remove-duplicates
; (lrec #'(lambda (x f) (adjoin x (funcall f))))

;; find-if for some function fn
; (lrec #'(lambda (x f) (if (fn x) x (funcall f))))

;;some, for some function fn
; (lrec #'(lambda (x f) (or (fn x) (funcall f))))

(defun rfind-if (fn tree)
  "Recursive version of find-if which works on trees as well as flat lists."
  (if (atom tree)
      (and (funcall fn tree) tree)
      (or (rfind-if fn (car tree))
          (if (cdr tree) (rfind-if fn (cdr tree))))))


;; (rfind-if #'oddp '(2 (3 4) 5))

;;Function for recursion on trees
;; tree traverser to build a wider range of tree recursive functions

(defun ttrav (rec &optional (base #'identity))
  (labels ((self (tree)
             (if (atom tree)
                 (if (functionp base)
                     (funcall base tree)
                     base)
                 (funcall rec (self (car tree))
                          (if (cdr tree)
                              (self (cdr tree)))))))

    #'self))

;; Functions built with ttrav always traverse a whole tree.


;; our-copy-tree
;; (ttrav #'cons)


;; count-leaves
;; (ttrav #'(lambda (l r) (+ 1 (or r 1))) 1)


;; flatten
;; (ttrav #'nconc #'mklist)


(defun trec (rec &optional (base #'identity))
  "General function trec which taakes three objects:
  the currect object and two recursers which are closures representing
  recursions down the left and right subtrees."
  (labels
      ((self (tree)
         (if (atom tree)
             (if (functionp base)
                 (funcall base tree)
                 base)
             (funcall rec tree
                      #'(lambda () (self (car tree)))
                      #'(lambda () (if (cdr tree) (self (cdr tree))))))))))


; Expressing functions by calls to constructors instead of sharp-quoted lambdaexpressions could, unfortunately, entail unnecessary work at runtime. A sharpquoted lambda-expression is a constant, but a call to a constructor function will be
; evaluated at runtime. If we really have to make this call at runtime, it might not
; be worth using constructor functions. However, at least some of the time we can
; call the constructor beforehand. By using #., the sharp-dot read macro, we can
; have the new functions built at read-time. So long as compose and its arguments
; are defined when this expression is read, we could say, for example,
;; (find-if #.(compose #’oddp #’truncate) lst)
; Then the call to compose would be evaluated by the reader, and the resulting
; function inserted as a constant into our code. Since both oddp and truncate are
; built-in, it would safe to assume that we can evaluate the compose at read-time,
; so long as compose itself were already loaded.
; In general, composing and combining functions is more easily and efficiently
; done with macros. This is particularly true in Common Lisp, with its separate
; name-space for functions.
