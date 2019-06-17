;;
;; The chain of calls:
;;
;; achieve-all -> achieve -> apply-op
;;
(defun achieve-all (state goals goal-stack)
  "Achieve each goal sequentially, using the output state of achieving
   one goal as a start state for achieving the next one. Make sure
   they still hold at the end."
  (labels ((process-one-goal (previous-state goal)
             ;; fail immediately and propagate failure
             (when previous-state
               (achieve previous-state goal goal-stack))))
    (let ((end-state (reduce #'process-one-goal goals :initial-value state)))
      (when (and end-state (subsetp goals end-state :test #'equal))
        end-state))))

(defun achieve (state goal goal-stack)
  "A goal is achieved if it already holds, or if there is an
   appropriate op for it that is applicable."
  (dbg-indent :gps (length goal-stack) "Goal: ~a" goal)
  (cond ((member-equal goal state) state)
        ;; Recursive subgoal, bail out immediately.
        ((member-equal goal goal-stack) nil)
        (t (labels ((local-apply-op (op)
                      (apply-op state goal op goal-stack))
                    (not-appropriate-p (goal op)
                      (not (member-equal goal (op-add-list op)))))
             (let ((ops-to-consider
                    (remove goal *ops* :test #'not-appropriate-p)))
               (some #'local-apply-op ops-to-consider))))))

(defun member-equal (item list)
  "Check if item is in list using `equal` as comparison."
  (member item list :test #'equal))

(defun apply-op (state goal op goal-stack)
  "Return a new, transformed state if op is applicable."
  (dbg-indent :gps (length goal-stack) "Consider: ~a" (op-action op))
  ;; Extend goal stack by the current goal.
  (let* ((extended-goal-stack (cons goal goal-stack))
         (state2 (achieve-all state (op-preconds op) extended-goal-stack)))
    (when state2
      (dbg-indent :gps (length goal-stack) "Action: ~a" (op-action op))
      (labels ((is-in-del-list (x)
                 (member-equal x (op-del-list op))))
        (append (remove-if #'is-in-del-list state2)
                (op-add-list op))))))

(defun GPS (state goals &optional (*ops* *ops*))
  "General Problem Solver v2: from state, achieve goals using *ops*. What it
   basically does is extending the initial state by adding '(start) to it and
   delegating the job to `achieve-all` with the extended initial state. After that,
   it removes from the end state everyting which is not either '(state) or
   '(executing smth.)"
  (labels ((not-action-p (x)
             "Is x not something that is (start) or (executing ...)?"
             (not (or (equal x '(start)) (executing-p x))))
           (remove-non-actions (items)
             (remove-if #'not-action-p items)))
    (let* ((ini-state (cons '(start) state))
           (end-state (achieve-all ini-state goals nil)))
      (remove-non-actions end-state))))
