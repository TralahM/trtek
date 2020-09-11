(defpackage trtek-utils/tests/main
  (:use :cl
        :trtek-utils
        :rove))
(in-package :trtek-utils/tests/main)

;; NOTE: To run this test file, execute `(asdf:test-system :trtek-utils)' in your Lisp.

(deftest test-target-1
  (testing "should (= 1 1) to be true"
    (ok (= 1 1))))
