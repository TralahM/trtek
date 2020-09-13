(in-package :trtek)

(defun fact (x)
  (if (= x 0)
      1
      (* x (fact (- x 1)))))


(defun choose (n r)
  (/ (fact n) (fact (- n r) (fact r))))
