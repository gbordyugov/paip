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
