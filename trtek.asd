(defsystem "trtek"
  :version "0.1.0"
  :author "Tralah M Brian <musyoki.brian@tralahtek.com>"
  :maintainer "Tralah M Brian <musyoki.brian@tralahtek.com>"
  :license "GPL"
  ; :homepage "https://github.com/TralahM/trtek"
  ; :bug-tracker "https://github.com/TralahM/trtek/issues"
  ; :source-control (:git "git@github.com:TralahM/trtek.git")
  :depends-on (:uiop :alexandria :asdf :fiveam)
  :components ((:module "src"
                :serial t
                :components ((:file "packages")
                             (:file "main")
                             (:file "macros")
                             (:file "utils")
                             (:module "gps" :serial t :components ((:file "gps"))))))
  :description "An Attempt at Lisp System Building."
  :long-description #.(uiop:read-file-string (uiop:subpathname *load-pathname* "README.md"))
  :in-order-to ((test-op (test-op "trtek/tests"))))

(defsystem "trtek/tests"
  :author "Tralah M Brian <musyoki.brian@tralahtek.com>"
  :license "GPL"
  :defsystem-depends-on (:fiveam)
  :depends-on (:trtek :fiveam )
  :components ((:module "tests"
                :serial t
                :components ((:file "main"))))
  :description "Test system for trtek"
  :perform (test-op (op c) (uiop:symbol-call 'trtek/tests/main :test-trtek )))
