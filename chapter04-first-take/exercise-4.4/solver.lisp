;;
;; The chain of calls:
;;
;; achieve-all -> achieve-each -> achieve -> apply-op
;;
(defun achieve-all (state goals goal-stack)
  "The main entry point, called from GPS. Generate some permutations
   of goals (see `orderings` below) and tries to achieve some of those
   permutations.
   goal-stack represents a stack of goals that are being worked on.
   Function achieve uses it to check for infinite loops."
  (labels ((local-achieve (goals)
             (achieve-each state goals goal-stack))
           (orderings (l)
             (if (> (length l) 1)
                 (list l (reverse l))
                 (list l))))
    (let ((goals-permutations (orderings goals)))
      (some #'local-achieve goals-permutations))))


;;
;; This function used to be called just `achieve` in the code of the book.
;;
(defun achieve-each (state goals goal-stack)
  "Achieve each goal sequentially, using the output state of achieving
   one goal as a start state for achieving the next one. Make sure
   they still hold at the end."
  (let ((current-state state))
    (labels ((local-achieve (g)
               (setf current-state (achieve-main current-state g goal-stack))))
      (if (and (every #'local-achieve goals)
               ;; make sure that all target goals still hold
               (subsetp goals current-state :test #'equal))
          current-state))))

(defun achieve-main (state goal goal-stack)
  "A goal is achieved if it already holds, or if there is an
   appropriate op for it that is applicable."
  (dbg-indent :gps (length goal-stack) "Goal: ~a" goal)
  (cond ((member-equal goal state) state)
        ;; Recursive subgoal, bail out immediately.
        ((member-equal goal goal-stack) nil)
        (t (labels ((local-apply-op (op)
                      (apply-op state goal op goal-stack)))
             (let ((ops-to-consider (appropriate-ops goal state)))
               ;; `some` returns the first non-nil result of apply the
               ;; func. The list of appropriate ops is sorted by the
               ;; number of unfulfilled conditions in an ascending
               ;; order (heuristics!).
               (some #'local-apply-op ops-to-consider))))))

(defun appropriate-ops (goal state)
  "Return a list of appropriate operators, sorted by the number of
   unfulfilled preconditions."
  (labels ((not-appropriate-p (goal op)
             (not (member-equal goal (op-add-list op))))
           (precond-unfulfilled-p (precond)
             (not (member-equal precond state)))
           (number-of-unfulfilled-preconds (op)
             (count-if #'precond-unfulfilled-p (op-preconds op))))
    (let ((ops (remove goal *ops* :test #'not-appropriate-p)))
      (sort (copy-list ops) #'< :key #'number-of-unfulfilled-preconds))))

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
             (not (or (equal x '(start)) (executing-p x)))))
    (let* ((ini-state (cons '(start) state))
           (end-state (achieve-all ini-state goals nil)))
      (remove-if #'not-action-p end-state))))
