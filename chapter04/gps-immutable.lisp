;;
;; A purely immutable version of the GPS from Chapter 4.
;;

;; An operator is represented as a structure composed of an action, a
;; list of preconditions, and a list of effects. The list of effects
;; can be split into an add-list (what conditions this operator adds)
;; and a delete-list (what conditions this operator deletes).
(defstruct op "An operation."
           (action nil)
           (preconds nil)
           (add-list nil)
           (del-list nil))
;;
;; Some ops for the problem from the book.
;;
(setq ops
  (list
   (make-op :action 'drive-son-to-school
            :preconds '(son-at-home car-works)
            :add-list '(son-at-school)
            :del-list '(son-at-home))
   (make-op :action 'shop-installs-battery
            :preconds '(car-needs-battery shop-knows-problem
                        shop-has-money)
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

;;
;; A goal is a symbol.
;; A state is a set (actually, a list) of goals.
;; In order to achieve a state, we have to achieve all goals it consists of.
;;
;; What does 'achieve a goal/state' mean?
;;

(defun achieve-state (from-state to-state ops)
  "Try to achieve to-state from from-state using operators ops. Return
   state which is (possibly) a superset of to-state if achievalbe,
   otherwise nil."
  (if (subsetp to-state from-state)
      from-state
      (let ((res (mapcar #'(lambda (g)
                             (achieve-goal from-state g ops))
                         to-state)))
        (concat-list-of-sets res))))

(defun concat-list-of-sets (sets)
  "Return union of all sets in the list."
  (reduce #'union sets))

(defun achieve-goal (from-state goal ops)
  "Try to achieve goal from from-state using operators ops. Return
   state containing goal if goal is achievable, otherwise nil."
  (if (member goal from-state)
      from-state
      ;; if goal is not part of from-state, we need to apply one of ops
      (let* ((potential-ops (ops-pointing-at ops goal))
             ;; the problem here is that we need both op and the updated state
             (state-and-op (some #'(lambda (op)
                                     (let ((new-state (achieve-state
                                                       from-state
                                                       (op-preconds op) ops)))
                                       (when new-state
                                         (cons new-state op))))
                                 potential-ops)))
        (when state-and-op
          (let ((state (car state-and-op))
                (op    (cdr state-and-op)))
            (apply-op-to-state op state ops))))))

(defun apply-op-to-state (op state ops)
  "Applies operation to state. Assumes that state satisfies all the
  preconditions of the operator. Logs the application to stdout."
  (let* ((reduced-state (set-difference state (op-del-list op)))
         (expanded-state (union reduced-state (op-add-list op))))
    (progn
      (format t "applying ~a to ~a.~%" (op-action op) state)
      expanded-state)))

(defun ops-pointing-at (ops goal)
  "Find all operators from ops that have goal as part of their add-list."
  (remove goal ops :test #'(lambda (goal op)
                               (not (member goal (op-add-list op))))))

;;
;; simple test of above
;;
(ops-pointing-at ops 'son-at-school)

;;
;; this one works as expected
;;
(achieve-state '(son-at-home car-works) '(son-at-school) ops)

;;
;; this one is working now, too
;;
(achieve-state '(have-money) '(shop-has-money) ops)

;;
;; surprisingly this one works as well :~)
;;
(achieve-state '(son-at-home car-needs-battery have-money have-phone-book)
               '(son-at-school)
               ops)

;;
;; Section 4.7
;;

;;
;; this can be solved correctly
;;
(achieve-state '(son-at-home have-money car-works)
               '(have-money son-at-school)
               ops)

;;
;; but the solution of this is not correct
;;
(achieve-state '(son-at-home car-needs-battery have-money have-phone-book)
               '(have-money son-at-school)
               ops)

;;
;; Sergey's failing test case
;;
(achieve-state '(son-at-home car-needs-battery have-money have-phone-book)
               '(son-at-school)
               ops)
