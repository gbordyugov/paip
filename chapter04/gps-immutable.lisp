;;
;; A purely immutable version of the GPS from Chapter 4.
;;

;; An operator is represented as a structure composed of an action, a
;; list of preconditions, and a list of effects. The list of effects
;; can be split into an add-list (what conditions this operator adds)
;; and a delete-list (what conditions this operator deletes).
(defstruct op "An operation"
           (action nil)
           (preconds nil)
           (add-list nil)
           (del-list nil))

;; A complete problem is described to GPS in terms of a starting
;; state, a goal state, and a set of known operators.
(defun GPS (state goals ops)
  "General Problem Solver: achieve all `goals` from starting from
   `state` using `ops`."
  (flet ((goal-achievable-p (goal)
           (achieve goal state ops)))
  (if (every #'goal-achievable-p goals)
      'solved
      'there-was-unfortunately-no-solution)))

;; A single goal condition can be achieved in two ways. If it is
;; already in the current state, the goal is trivially achieved with
;; no effort. Otherwise, we have to find some appropriate operator and
;; try to apply it.
(defun achieve (goal state ops)
  "A goal is achieved if it already holds, or if there is an appropriate
   op for it that is applicable."
  (or (member goal state)
      (flet ((apply-op-to-this-state (op)
               (apply-op state op ops))
             (appropriate (op)
               (appropriate-p goal op)))
        ;; This call relies on mutability
        ;; (some #'apply-op-to-this-state (find-all goal ops :test #'appropriate-p)))))
        (some #'apply-op-to-this-state (filter #'appropriate ops)))))

;; An operator is appropriate if one of the effects of the operator is
;; to add the goal in question to the current state; in other words,
;; if the goal is in the operator's add-list.
(defun appropriate-p (goal op)
  "An op is appropriate to a goal if it is in its add list."
  (member goal (op-add-list op)))

;; We can apply an operator to a state if we can achieve all the
;; preconditions of the state.
(defun apply-op (state op ops)
  "Print a message and update *state* if op is applicable."
  ;; (when (every #'achieve (op-preconds op))
  (when (every #'(lambda (goal)
                   (achieve goal state ops))
               (op-action op))
    (print (list 'executing (op-action op)))
    ;; Those are obviously destructive updates. I'll have to think
    ;; about what to do here in the immutable version.
    (setf *state* (set-difference *state* (op-del-list op)))
    (setf *state* (union *state* (op-add-list op)))
    t))



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
