;;
;; The chain of calls
;; achieve-all -> achieve -> apply-op
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
      (and end-state and all-goals-reached-p))))

(defun achieve (state goals goal-stack)
  t)

(defun apply-op (state op goal-stack)
  t)

(defun gps (start-state end-state)
  t)
