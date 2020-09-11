(in-package :trtek)


(defun sigmoid (x)
  (/ 1.0 (+ 1.0 (exp (- x)))))

(defun sigmoid* (x)
  (let ((temp (sigmoid x)))
    (* temp (- 1.0 temp))))

;; blah blah blah.
