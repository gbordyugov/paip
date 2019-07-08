;;
;; The chain of calls
;; achieve-all -> achieve -> apply-op
;;

;;
;; A state is a set of goals achieved so far.
;;

(defun achieve-all (state goals goal-stack)
  "Achieve all goals in sequence, using the state after achieving the
   previous goal as the starting state for achieving the next goal.
   Check if the end state is a superset of goals."
  (labels ((achieve-state (from-state to-state)
             (when from-state
               (achieve from-state to-state goal-stack))))
    (let* ((end-state (reduce #'achieve-state goals :initial-value state))
           (all-goals-reached-p (subsetp goals end-state :test #'equal)))
      (when (and end-state all-goals-reached-p)
        end-state))))

(defun achieve (state goal goal-stack)
  "Achieve a single goal starting from state."
  (cond
    ;; Have we considered this goal before?
    ((member-equal goal goal-stack) '())
    ;; Is goal already satisfied?
    ((member-equal goal state) state)
    ;; Find all opearators that have goal on their add-list.
    (t (labels ((local-apply-op (op)
                  (apply-op state goal op goal-stack))
                (not-appropriate-p (goal op)
                  (not (member-equal goal (op-add-list op)))))
         (let* ((ops-to-consider
                 (remove goal *ops* :test #'not-appropriate-p)))
           (some #'local-apply-op ops-to-consider))))))


(defun member-equal (item list)
  "Check if item is inlist using `equal` as comparison."
  (member item list :test #'equal))

(defun apply-op (state goal op goal-stack)
  "In order to apply an operator to state, we first have to fulfil all
   preconditions of the operator. We need to pass goal in order to expand
   goal-stack."
  (let* ((extended-goal-stack (cons goal goal-stack))
         (new-state (achieve-all state (op-preconds op) extended-goal-stack)))
    (when new-state
      (labels ((is-in-del-list (x)
                 (member-equal x (op-del-list op)))
               (update (state op)
                 ;; add op's add-list and remove del-list goals from state
                 (append (remove-if #'is-in-del-list new-state)
                         (op-add-list op))))
        (update new-state op)))))


(defun gps (start-state end-state)
  (labels ((to-remove-p (x)
             (not (or (equal x '(start))
                      (and (consp x)
                           (equal (first x) 'executing)))))
           (clean-up (goals)
             (remove-if #'to-remove-p
                        goals)))
    (let* ((ini-state (cons '(start) start-state))
           (end-state (achieve-all ini-state end-state nil)))
      (clean-up end-state))))
