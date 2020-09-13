(defpackage trtek/tests/main
  (:use :cl
        :trtek
        :rove))
(in-package :trtek/tests/main)

;; NOTE: To run this test file, execute `(asdf:test-system :trtek)' in your Lisp.

(deftest test-target-1
  (testing "should (= 1 1) to be true"
    (ok (= 1 1))))

(deftest test-cumsum
  (testing "(= (trtek:cumsum '(1 2 3)) '(1 3 6)) should be true"
           (ok (eql (trtek:cumsum '(1 2 3)) '(1 3 6)))))

(deftest test-sumlist
  (testing "(= (trtek:sumlist '(1 2 3)) 6) should be true"
           (ok (eql (trtek:sumlist '(1 2 3)) 6))))
