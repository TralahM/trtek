(in-package :trtek)


(defmacro nil! (var)
  "Sets its argument to nil"
  (list 'setq var nil))

;; with backquotes the macro can be defined as

(defmacro nill! (var)
  `(setq ,var nil))
