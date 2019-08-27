(defvar *ops* nil "A list of available operators.")

(defstruct op
  "An operation"
  (action nil)
  (preconds nil)
  (add-list nil)
  (del-list nil))

(load "utils.lisp")

(defun GPS (state goals &optional (*ops* *ops*))
  "General Problem Solver: from state, achieve goals using *ops*."
  ;; Note that we extend the initial condition by appending a '(START)
  ;; list to it, so starting even with an empty list will produce
  ;; meaningful initial condition.
  ;; The purpose of it to make '() / nil less ambiguous: it now
  ;; represents a failure rather than an empty set of conditions. In
  ;; other words, a state can never become an empty list.
  (remove-if #'atom (achieve-all (cons '(start) state) goals nil)))

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
