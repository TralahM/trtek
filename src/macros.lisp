(in-package :trtek)

(defmacro nil! (var)
  "set var to nil"
  `(setq ,var nil))

(defmacro while (test &body body)
  `(do ()
       ((not ,test))
     ,@body))


(defmacro nif (expr pos zero neg)
  "Numerical If macro"
  `(case (truncate (signum ,expr))
     (1 ,pos)
     (0 ,zero)
     (-1 ,neg)))

(defmacro avg (&rest args)
  "Calculate the average of list macro."
  `(/ (+ ,args) ,(length args)))

; (macroexpand 'avg )
