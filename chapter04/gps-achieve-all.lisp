;;
;; This is an improvement over the original code of GPS v1 by
;; introducing ACHIEVE-ALL function.
;;
(load "utils.lisp")

;;
;; Defvars are just like global variables (with dynamic scope).
;;
(defvar *state* nil "The current state: a list of conditions.")

(defvar *ops* nil "A list of available operators.")

(defstruct op "An operation"
           (action nil)
           (preconds nil)
           (add-list nil)
           (del-list nil))

(load "ops/school-ops.lisp")

(defun GPS (*state* goals *ops*)
  "General Problem Solver: achieve all goals using *ops*."
  (if (achieve-all goals) 'solved))

(defun achieve (goal)
  "A goal is achieved if it already holds, or if there is an
   appropriate op for it that is applicable."
  (or (member goal *state*)
      (some #'apply-op
            (find-all goal *ops* :test #'appropriate-p))))

(defun achieve-all (goals)
  "Try to achieve each goal, then make sure they still hold."
  (and (every #'achieve goals)
       (subsetp goals *state*)))

(defun appropriate-p (goal op)
  "An op is appropriate to a goal if it is in its add list."
  (member goal (op-add-list op)))

(defun apply-op (op)
  "Print a message and update *state* if op is applicable."
  (when (achieve-all (op-preconds op))
    (print (list 'executing (op-action op)))
    ;; update the state
    (setf *state* (set-difference *state* (op-del-list op)))
    (setf *state* (union *state* (op-add-list op)))
    t))
