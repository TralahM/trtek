(defpackage trtek/tests/main
  (:use :cl
        :trtek
        :fiveam)
  (:export #:run! #:all-tests test-trtek))

(in-package :trtek/tests/main)


(def-suite all-tests
  :description "The master suite of all trtek tests.")



(in-suite all-tests)

(defun test-trtek ()
  (run! 'all-tests))



(test test-cumsum
      "(= (trtek:cumsum '(1 2 3)) '(1 3 6)) should be true"
      (is (equal (trtek:cumsum '(1 2 3)) '(1 3 6))))



(test test-sumlist
      "(= (trtek:sumlist '(1 2 3)) 6) should be true"
      (is (eql (trtek:sumlist '(1 2 3)) 6)))



(test test-cmplmnt
      (is (equal (remove-if #'oddp '(1 2 3 4 5 6))  '(2 4 6)))
      (is (equal (remove-if (cmplmnt #'evenp) '(1 2 3 4 5 6))  '(2 4 6)))
      (is (equal (remove-if (cmplmnt #'oddp) '(1 2 3 4 5 6))  '(1 3 5))))



(test test-compose
      (is (equal (funcall (compose #'1+ #'find-if) #'oddp '(2 3 4))  4)))


(test test-last1
      (is (equal (last1 '(1 2 3 4)) 4)))


(test test-single
      (is (equal (single '(1)) t))
      (is (equal (single '(1 2 3 4)) nil)))



(test test-mklist
      (is (equal (mklist '12)   '(12)))
      (is (equal (mklist '(1 2)) '(1 2))))



(test test-longer
      (is (equal (longer '(1 2 3 45) '(1 5 4 5 6 7))  NIL))
      (is (equal (longer '(1 2 3 45) '(1 5))  T)))



(test test-flatten
      (is (equal (flatten '(1 2 3 4 6 8 9 12 (32 11 39 7) 19)) '(1 2 3 4 6 8 9 12 32 11 39 7 19)))
      (is (equal (flatten '(1 2 3 4 6 8 9 12 32 11 39 7 19)) '(1 2 3 4 6 8 9 12 32 11 39 7 19)))
      (is (equal (flatten '(1 2 3 4 6 (8 9 12 (32 11 39) 7) 19)) '(1 2 3 4 6 8 9 12 32 11 39 7 19))))


(test test-mapa-b
      (is (equal (mapa-b #'1+ -2 0 .5) '(-1 -0.5 0.0 0.5 1.0))))


(test test-map0-n
      (is (equal (map0-n #'1+ 5)  '(1 2 3 4 5 6))))


(test test-map1-n
      (is (equal (map1-n #'1+ 5)  '(2 3 4 5 6))))

(test test-before
      (is (equal (before 'b 'a '(b a b a b d)) '(B A B A B D)))
      (is (equal (before 'c 'd '(a b c d))  '(C D)))
      (is (equal (before 'a 'b '(a))  '(A))))


(test test-after
      (is (equal (after 'a 'b '(b a b a d)) '(A B A D)))
      (is (equal (after 'a 'b '(a b c))  NIL))
      (is (equal (after 'a 'b '(b a d)) '(A D))))


(test test-duplicate
      (is (equal (duplicate 'b '(a b c a b c a b d))  '(B C A B D)))
      (is (equal (duplicate 'a '(a b c a d))  '(A D))))


(test test-best
      (is (equal (best #'< '(1 2 3 4 5))  1))
      (is (equal (best #'> '(1 2 3 4 5))  5)))


(test test-explode
      (is (equal (explode 'bomb) '(B O M B))))


(test test-zip
      (is (equal (zip '(1 2 3) '(4 5 6)) '((1 4) (2 5) (3 6)))))


(test test-zipsum
      (is (equal (zipsum '(1 2 3) '(4 5 6))  '(5 7 9)))
      (is (equal (zipsum '(1 -2 3) '(4 5 6) '(10 11 12))  '(15 14 21)))
      (is (equal (zipsum '(1 -2 3) '(4 5 6))  '(5 3 9))))


(test test-zipdiff
      (is (equal (zipdiff '(10 20 30) '(4 5 6) '(2 6 10))  '(4 9 14)))
      (is (equal (zipdiff '(10 20 30) '(4 5 6))  '(6 15 24)))
      (is (equal (zipdiff '(1 2 3) '(4 5 6))  '(-3 -3 -3))))


(test test-zipmult
      (is (equal (zipmult '(1 2 3) '(4 5 6) '(7 8 9))  '(28 80 162)))
      (is (equal (zipmult '(1 2 3) '(4 5 6))  '(4 10 18))))


(test test-zipdiv
      (is (equal (zipdiv '(1.0 2.0 3.0) '(4 5 6) '(5 2 0.25)) '(0.05 0.2 2.0)))
      (is (equal (zipdiv '(1.0 2.0 3.0) '(4 5 6))  '(0.25 0.4 0.5)))
      (is (equal (zipdiv '(1 2 3) '(4 5 6))  '(1/4 2/5 1/2))))


(test test-dot-product
      (is (equal (dot-product '(1 2 3) '(4 5 6) '(7 8 9)) 270))
      (is (equal (dot-product '(1 2 3) '(4 5 6))  32)))


(test test-cross-product
      (is (equal (cross-product `(1 2) `(a b)) '((1 a) (2 a) (1 b) (2 b))))
      (is (equal (cross-product `(1 2) `(1 2)) '((1 1) (2 1) (1 2) (2 2)))))




(test factorial-test
      (is (equal (fact 5)  120))
      (is (equal (fact 6)  720)))


(test n-choose-r-test
      (is (equal (choose 6 2)  15))
      (is (equal (choose 4 2)  6)))


(test deg-to-rad-test
      (is (approx-equal (deg-to-rad 90)  1.5707963267948966d0))
      (is (approx-equal (deg-to-rad 45)  0.7853981633974483d0))
      (is (approx-equal (deg-to-rad 180)  3.141592653589793d0)))


(test rad-to-deg-test
      (is (approx-equal (rad-to-deg (* pi 2))  360.0d0))
      (is (approx-equal (rad-to-deg (* pi .5))  90.0d0))
      (is (approx-equal (rad-to-deg pi)  180.0d0)))


(test cos-deg-test
      (is (approx-equal (cos-deg 90)  6.1232339957d-17))
      (is (approx-equal (cos-deg 30)  0.8660254))
      (is (approx-equal (cos-deg 0)  1.0d0))
      (is (approx-equal (cos-deg 45)  0.70710678))
      (is (approx-equal (cos-deg 60)  0.5)))


(test acos-deg-test
      (is (approx-equal (acos-deg 0)  90.0))
      (is (approx-equal (acos-deg 1)  0.0d0))
      (is (approx-equal (acos-deg -1)  180.0))
      (is (approx-equal (acos-deg .5)  60.0)))


(test sin-deg-test
      (is (approx-equal (sin-deg 90)  1.0d0))
      (is (approx-equal (sin-deg 30)  0.5))
      (is (approx-equal (sin-deg 0)  0.0d0))
      (is (approx-equal (sin-deg 45)  0.70710678))
      (is (approx-equal (sin-deg 60)  0.8660254)))


(test asin-deg-test
      (is (approx-equal (asin-deg 0)  0.0d0))
      (is (approx-equal (asin-deg 1)  90.0))
      (is (approx-equal (asin-deg -1)  -90.0))
      (is (approx-equal (asin-deg .5)  30.0)))


(test tan-deg-test
      (is (approx-equal (tan-deg 60)  1.7320508))
      (is (approx-equal (tan-deg 30)  0.5773502))
      (is (approx-equal (tan-deg 45)  1)))


(test atan-deg-test
      (is (approx-equal (atan-deg 0)  0.0d0))
      (is (approx-equal (atan-deg 1.0)  45.0))
      (is (approx-equal (atan-deg .5)  26.56505089)))


