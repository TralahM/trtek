(defsystem "trtek"
  :version "0.1.0"
  :author "Tralah M Brian <musyoki.brian@tralahtek.com>"
  :license "GPL"
  :depends-on ("common-lisp"
               "uiop"
               "alexandria")
  :components ((:module "src"
                :components
                ((:file "packages")
                 (:file "main"))))
  :description ""
  :long-description #.(uiop:read-file-string
      (uiop:subpathname *load-pathname* "README.md"))
  :in-order-to ((test-op (test-op "trtek/tests"))))

(defsystem "trtek/tests"
  :author "Tralah M Brian <musyoki.brian@tralahtek.com>"
  :license "GPL"
  :depends-on ("trtek"
               "rove")
  :components ((:module "tests"
                :components
                ((:file "main"))))
  :description "Test system for trtek"
  :perform (test-op (op c) (symbol-call :rove :run c)))
