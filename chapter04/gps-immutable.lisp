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
;; A goal is a symbol.
;; A state is a set (actually, a list) of goals.
;; In order to achieve a state, we have to achieve all goals it consists of.
;;
;; What does 'achieve a goal/state' mean?
;;

(defun achieve-state (from-state to-state ops)
  "Try to achive to-state from from-state using operators ops. Return
   state which is (possibly) a superset of to-state if achievalbe,
   otherwise nil."
  t)

(defun achive-goal (from-state goal ops)
  "Try to achive goal from from-state using operators ops. Return
   state containing goal if goal is achievable, otherwise nil."
  (if (goal-achieved-p from-state goal)
      from-state
      ;; if goal is not part of from-state, we need to apply one of ops
      t))

(defun ops-pointing-at (ops goal)
  (find-all goal ops :test #'(lambda (goal op)
                               (member goal (op-add-list op)))))

(ops-pointing-at ops 'son-at-school)

(defun goal-achieved-p (state goal)
  (member goal state))

;;
;; Auxiliary functions.
;;
(defun find-all (item sequence &rest keyword-args
                 &key (test #'eql) test-not &allow-other-keys)
  "Find all those elements of sequence that match item,
   according to the keywords. Doesn't alter sequence."
  (if test-not
      (apply #'remove item sequence
             :test-not (complement test-not) keyword-args)
      (apply #'remove item sequence
            :test (complement test) keyword-args)))

(defun filter (predicate seq)
  "Return list containing only those elements of `seq` that satisfy the unary
   `predicate`. Uses tail recursion."
  (labels ((rec (seq acc)
             (cond
               ((null seq) acc)
               ((funcall predicate (first seq))
                (rec (rest seq) (cons (first seq) acc)))
               (t (rec (rest seq) acc)))))
    (reverse (rec seq '()))))

(filter #'oddp '(1 2 3 4 5 6 7))

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
