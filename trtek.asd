(defsystem "trtek"
  :version "0.1.0"
  :author "Tralah M Brian <musyoki.brian@tralahtek.com>"
  :maintainer "Tralah M Brian <musyoki.brian@tralahtek.com>"
  :license "GPL"
  ; :homepage "https://github.com/TralahM/trtek"
  ; :bug-tracker "https://github.com/TralahM/trtek/issues"
  ; :source-control (:git "git@github.com:TralahM/trtek.git")
  :depends-on (:uiop :alexandria)
  :components ((:module "src"
                :serial t
                :components ((:file "packages")
                 (:file "main")))))
  :description "An Attempt at Lisp System Building."
  :long-description #.(uiop:read-file-string (uiop:subpathname *load-pathname* "README.md"))
  ; :in-order-to ((test-op (rove "trtek/tests")))

(defsystem "trtek/tests"
  :author "Tralah M Brian <musyoki.brian@tralahtek.com>"
  :license "GPL"
  :depends-on ("trtek"
               "rove")
  :components ((:module "tests"
                :components ((:file "main"))))
  :description "Test system for trtek")
  ; :perform (test-op (op c) (symbol-call :rove :run c)))
