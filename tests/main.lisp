(defpackage trtek/tests/main
  (:use :cl
        :trtek
        :prove))
(in-package :trtek/tests/main)

;; NOTE: To run this test file, execute `(asdf:test-system :trtek)' in your Lisp.

(deftest test-target-1
  (print "should (= 1 1) to be true"
    (ok (= 1 1))))

(deftest test-cumsum
  (print "(= (trtek:cumsum '(1 2 3)) '(1 3 6)) should be true"
           (ok (eql (trtek:cumsum '(1 2 3)) '(1 3 6)))))

(deftest test-sumlist
  (print "(= (trtek:sumlist '(1 2 3)) 6) should be true"
           (ok (eql (trtek:sumlist '(1 2 3)) 6))))
