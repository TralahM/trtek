(in-package :trtek)

(defstruct node contents yes no)

(defvar *nodes* nil)

(defun defnode (&rest args)
  (push args *nodes*)
  args)

(defun compile-net (root)
  (let ((node (assoc root *nodes*)))
    (if (null node)
        nil
        (let ((conts (second node))
              (yes (third node))
              (no (fourth node)))
          (if yes
              (let ((yes-fn (compile-net yes))
                    (no-fn (compile-net no)))
                #'(lambda ()
                    (format t "~A~%>> " conts)
                    (funcall (if (eq (read) ’yes)
                                 yes-fn
                                 no-fn))))
              #'(lambda () conts))))))


;; Sample Network

; (defnode 'people "Is the person a man?" 'male 'female)

; (defnode 'male "Is he living?" 'liveman 'deadman)

; (defnode 'deadman "Was he Kenyan?" 'us 'them)

; (defnode 'us "Is he on a coin?" 'coin 'cidence)

; (defnode 'coin "Is the coin a shilling?" 'shilling 'coins)

; (defnode 'shilling 'kenyatta)

; (funcall (gethash 'people *nodes*))
; (setq n (compile-net 'people))

