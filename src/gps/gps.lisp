(in-package :trtek)

(defvar *ops* nil "A list of available operators.")

(defstruct op "An operation"
  (action nil) (preconds nil) (add-list nil) (del-list nil))

(defvar *dbg-ids* nil "Identifiers used by dbg")



(defun dbg (id format-string &rest args)
  "Print Debugging info if (DEBUG ID) has been specified."
  (when (member id *dbg-ids*)
    (fresh-line *debug-io*)
    (apply #'format *debug-io* format-string args)))

(defun gdebug (&rest ids)
  "Start dbg output on the given ids."
  (setf *dbg-ids* (union ids *dbg-ids*)))

(defun undebug (&rest ids )
  "Stop dbg on the ids . With no ids . stop dbg altogether."
  (setf *dbg-ids* (if (null ids) nil
                      (set-difference *dbg-ids* ids))))

(defun dbg-indent (id indent format-string &rest args)
  "Print indented debugging info if (DEBUG ID) has been specified."
  (when (member id *dbg-ids*)
    (fresh-line *debug-io*)
    (dotimes (i indent) (princ " " *debug-io*))
    (apply #'format *debug-io* format-string args)))

(defun find-all (item sequence &rest keyword-args
                      &key (test #'eql) test-not &allow-other-keys)
  "Find all those elements of sequence that match item,
  according to the keywords. Doesn't alter sequence."
  (if test-not
      (apply #'remove item sequence
             :test-not (complement test-not) keyword-args)
      (apply #'remove item sequence
             :test (complement test) keyword-args)))

(defun GPS (state goals &optional (*ops* *ops*))
  "General Problem Solver: achieve all goals using *ops*."
  (remove-if #'atom (achieve-all (cons '(start) state) goals nil)))

(defun achieve (state goal goal-stack)
  "A goal is achieved if it already holds.
  or if there is an appropriate op for it that is applicable."
  (dbg-indent :gps (length goal-stack) "Goal: ~a" goal)
  (cond ((member-equal goal state) state)
        ((member-equal goal goal-stack) nil)
        (t (some #'(lambda (op) (apply-op state goal op goal-stack))
                 (find-all goal *ops* :test #'appropriate-p)))))

(defun member-equal (item list)
  (member item list :test #'equal))

(defun achieve-all (state goals goal-stack)
  "Try to achieve each goal, then make sure they still hold."
  (let ((current-state state))
    (if (and (every #'(lambda (g)
                        (setf current-state (achieve current-state g goal-stack)))
                    goals)
             (subsetp goals current-state :test #'eql))
        current-state)))

(defun appropriate-p (goal op)
  "An op is appropriate to a goal if it is in its add list."
  (member-equal goal (op-add-list op)))


(defun apply-op (state goal op goal-stack)
  "Return a new transformed state if op is applicable."
  (dbg-indent :gps (length goal-stack) "Consider: ~a" (op-action op))
  (let ((state2 (achieve-all state (op-preconds op)
                             (cons goal goal-stack))))
    (unless (null state2)
      ;;Return an updated state
      (dbg-indent :gps (length goal-stack) "Action: ~a" (op-action op))
      (append (remove-if #'(lambda (x) (member-equal x (op-del-list op)))
                         state2)
              (op-add-list op)))))

(defun use (oplist)
  "Use oplist as the default list of operators."
  ;;Return sth useful but not too verbose: the number of operators.
  (length (setf *ops* oplist)))

(defun executing-p (x)
  "Is x of the form: (executing ...).?"
  (starts-with x 'executing))


(defun starts-with (list x)
  "Is this a list whose first element starts with x?"
  (and (consp list) (eql (first list) x)))

(defun convert-op (op)
  "Make op conform to the (EXECUTING op) convention."
  (unless (some #'executing-p (op-add-list op))
    (push (list 'executing (op-action op)) (op-add-list op)))
  op)

(defun op (action &key preconds add-list del-list)
  "Make a new operator that obeys the (EXECUTING op) convention."
  (convert-op
    (make-op :action action :preconds preconds
             :add-list add-list :del-list del-list)))


; Some test operations for testing

(defparameter *school-ops*
  (list
    (make-op :action 'drive-son-to-school
             :preconds '(son-at-home car-works)
             :add-list '(son-at-school)
             :del-list '(son-at-home))
    (make-op :action 'shop-installs-battery
             :preconds '(car-needs-battery shop-knows-problem shop-has-money)
             :add-list '(car-works))
    (make-op :action 'tell-shop-problem
             :preconds '(in-communication-with-shop)
             :add-list '(shop-knows-problem))
    (make-op :action 'telephone-shop
             :preconds '(know-phone-number)
             :add-list '(in-communication-with-shop))
    (make-op :action 'look-up-number
             :preconds '(have-phone-book)
             :add-list '(know-phone-number))
    (make-op :action 'give-shop-money
             :preconds '(have-money)
             :add-list '(shop-has-money)
             :del-list '(have-money))))

(mapc #'convert-op *school-ops*)

(defparameter *banana-ops*
  (list
    (op 'climb-on-chair
        :preconds '(chair-at-middle-room at-middle-room on-floor)
        :add-list '(at-bananas on-chair)
        :del-list '(at-middle-room on-floor))
    (op 'push-chair-from-door-to-middle-room
        :preconds '(chair-at-door at-door)
        :add-list '(chair-at-middle-room at-middle-room)
        :del-list '(chair-at-door at-door))
    (op 'walk-from-door-to-middle-room
        :preconds '(at-door on-floor)
        :add-list '(at-middle-room)
        :del-list '(at-door))
    (op 'grasp-bananas
        :preconds '(at-bananas empty-handed)
        :add-list '(has-bananas)
        :del-list '(empty-handed))
    (op 'drop-ball
        :preconds '(has-ball)
        :add-list '(empty-handed)
        :del-list '(has-ball))
    (op 'eat-bananas
        :preconds '(has-bananas)
        :add-list '(empty-handed not-hungry)
        :del-list '(has-bananas hungry))))

; (use *school-ops*)

;; Testing.

; (gdebug :gps)

; (gps '(son-at-home car-needs-battery have-money have-phone-book) '(son-at-school) *school-ops*)

; (gps '(son-at-home car-works) '(son-at-school) )

; (gps '(son-at-home car-needs-battery have-money) '(son-at-school) *school-ops*)

;; Clobbered Sibling Goal Problem
; (gps '(son-at-home car-needs-battery have-money have-phone-book) '(have-money son-at-school) *school-ops*)
; (gps '(son-at-home car-needs-battery have-money have-phone-book) '(son-at-school have-money) *school-ops*)


; (undebug :gps)


; (gdebug :gps)

; (use *banana-ops*)
; (gps '(at-door on-floor has-ball hungry chair-at-door) '(not-hungry))
; (undebug :gps)
