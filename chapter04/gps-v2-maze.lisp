(defvar *ops* nil "A list of available operators.")

(load "ops/op.lisp")
(load "utils.lisp")

;;
;; The variable *ops* is a special variable. So binding it to local
;; parameter results in all references to the special variable that
;; occure anywhere in programme refere to the new binding of the
;; special variable.
;;

(defun GPS (state goals &optional (*ops* *ops*))
  "General Problem Solver: from state, achieve goals using *ops*."
  ;; Note that we extend the initial condition by appending a '(START)
  ;; list to it, so starting even with an empty list will produce
  ;; meaningful initial condition.
  ;; The purpose of it to make '() / nil less ambiguous: it now
  ;; represents a failure rather than an empty set of conditions. In
  ;; other words, a state can never become an empty list.
  ;; The helper action-p defined in utils.
  (find-all-if #'action-p
               (achieve-all (cons '(start) state) goals nil)))

(defun find-path (start end)
  "Search a maze for a path from start to end."
  (let ((results (GPS `((at ,start)) `((at ,end)))))
    (unless (null results)
      (cons start (mapcar #'destination
                          (remove '(start) results
                                  :test #'equal))))))

(defun destination (action)
  "Find the Y in (executing (move from X to Y))."
  (fifth (second action)))

(defun achieve-all (state goals goal-stack)
  "Achieve each goal in succession, using the result of achieving the
   previous one as a starting condition for achieving the next one,
   and make sure they still hold at the end."
  (let ((current-state state))
    (if (and (every #'(lambda (g)
                        (setf current-state
                              (achieve current-state g goal-stack)))
                    goals)
             (subsetp goals current-state :test #'equal))
        current-state)))

(load "debug.lisp")

;; for find-all, member-equal, appropriate-p, etc.
(load "utils.lisp")

(defun achieve (state goal goal-stack)
  "A goal is achieved if it already holds, or there is an appropriate
   op for it that is applicable."
  (norvig-dbg-indent :gps (length goal-stack) "Goal: ~a" goal)
  (cond ((member-equal goal state) state)
        ;; fail fast
        ((member-equal goal goal-stack) nil)
        (t (some #'(lambda (op) (apply-op state goal op goal-stack))
                 (find-all goal *ops* :test #'appropriate-p)))))

(defun apply-op (state goal op goal-stack)
  "Return a new, transformed state if op is applicable."
  (norvig-dbg-indent :gps (length goal-stack) "Consider: ~a" (op-action op))
  (let ((state2 (achieve-all state (op-preconds op)
                             (cons goal goal-stack))))
    (unless (null state2)
      ;; Return an updated state
      (norvig-dbg-indent :gps (length goal-stack)
                         "Action: ~a" (op-action op))
      (append (remove-if #'(lambda (x)
                             (member-equal x (op-del-list op)))
                         state2)
              (op-add-list op)))))
