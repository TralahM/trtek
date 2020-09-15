(in-package :trtek)


(defmacro nil! (var)
  "Sets its argument to nil"
  (list 'setq var nil))
