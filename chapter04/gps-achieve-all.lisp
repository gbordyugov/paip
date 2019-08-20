;;
;; I feel that I haven't learned a lot during my first pass through
;; Chapter 4 and decided to give it another go.
;;

;;
;; Specification:
;;
;; - The current state of the world/search is a set of conditions, for
;;   example, (reach famous)
;;
;; - We need a list of allowable operators, each of them represented
;;   by an action, a list of preconditions, and a list of effects. The
;;   latter is going to be split into an add-list and a delete-list.
;;
;; - A complete problem is described by a starting state, a goal
;;   state, and a set of known operators.
;;
;; - A single goal condition can be achieved in two ways. If it's
;;   already in the current state, it is already achieved. Otherwise,
;;   we have to find an operator that targets this goal, i.e. has the
;;   goal on its add-list.
;;
;; - We can apply an operator if we can achieve all its preconditions.
;;   This naturally leads to recursion.
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
