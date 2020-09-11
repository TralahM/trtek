(defpackage trtek/tests/main
  (:use :cl
        :trtek
        :rove))
(in-package :trtek/tests/main)

;; NOTE: To run this test file, execute `(asdf:test-system :trtek)' in your Lisp.

(deftest test-target-1
  (testing "should (= 1 1) to be true"
    (ok (= 1 1))))
