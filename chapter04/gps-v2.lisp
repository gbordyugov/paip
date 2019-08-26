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
  (remove-if #'atom (achieve-all (cons '(start) state) goals nil)))
