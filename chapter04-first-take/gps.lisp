;;
;; Chapter 4
;;

;; The current state of the world is a set of conditions (represented
;; by symbols). A typical goal would be the list of three conditions:
;; (rich famous healthy), and a typic current state might be (unknown
;; poor).
(defvar *state* nil "The current state: a list of conditions.")

;; A list of allowable operators. This list will be constant over the
;; course of a problem, or even a series of problems, but we want to
;; be able to change it and tackle a new problem domain.
(defvar *ops* nil "A list of available operators.")

;; An operator is represented as a structure composed of an action, a
;; list of preconditions, and a list of effects. The list of effects
;; can be split into an add-list (what conditions this operator adds)
;; and a delete-list (what conditions this operator deletes).
(defstruct op "An operation"
           (action nil)
           (preconds nil)
           (add-list nil)
           (del-list nil))

;; A complete problem is described to GPS in terms of a starting
;; state, a goal state, and a set of known operators.
(defun GPS (*state* goals *ops*)
  "General Problem Solver: achieve all goals using *ops*."
  (if (achieve-all goals) 'solved))

;; A single goal condition can be achieved in two ways. If it is
;; already in the current state, the goal is trivially achieved with
;; no effort. Otherwise, we have to find some appropriate operator and
;; try to apply it.
(defun achieve (goal)
  "A goal is achieved if it already holds, or if there is an appropriate
   op for it that is applicable."
  (or (member goal *state*)
      (some #'apply-op (find-all goal *ops* :test #'appropriate-p))))

(defun achieve-all (goals)
  "Try to achieve each goal, then make sure they still hold."
  (and (every #'achieve goals) (subsetp goals *state*)))

;; An operator is appropriate if one of the effects of the operator is
;; to add the goal in question to the current state; in other words,
;; if the goal is in the operator's add-list.
(defun appropriate-p (goal op)
  "An op is appropriate to a goal if it is in its add list."
  (member goal (op-add-list op)))

;; We can apply an operator if we can achieve all the preconditions.
(defun apply-op (op)
  "Print a message and update *state* if op is applicable."
  (when (achieve-all (op-preconds op))
    ;; note that we don't print anything before having checked achievability
    (print (list 'executing (op-action op)))
    (setf *state* (set-difference *state* (op-del-list op)))
    (setf *state* (union *state* (op-add-list op)))
    t))

;;
;; aux functions
;;
(defun find-all (item sequence &rest keyword-args
                 &key (test #'eql) test-not &allow-other-keys)
  "Find all those elements of sequence that match item,
   according to the keywords. Doesn't alter sequence."
  (if test-not
      (apply #'remove item sequence
             :test-not (complement test-not) keyword-args)
      (apply #'remove item sequence
            :test (complement test) keyword-args)))

;;
;; The example from the book
;;

(defparameter *school-ops*
  (list
   (make-op :action 'drive-son-to-school
            :preconds '(son-at-home car-works)
            :add-list '(son-at-school)
            :del-list '(son-at-home))
   (make-op :action 'shop-installs-battery
            :preconds '(car-needs-battery shop-knows-problem
                        shop-has-money)
            :add-list '(car-works))
   (make-op :action 'tell-shop-problem
            :preconds '(in-communication-with-shop)
            :add-list '(shop-knows-problem))
   (make-op :action 'telephone-shop
            :preconds '(know-phone-number)
            :add-list '(in-communication-with-shop))
   (make-op :action 'look-up-number
            :preconds '(have-phone-book)
            :add-list '(know-phone-number))
   (make-op :action 'give-shop-money
            :preconds '(have-money)
            :add-list '(shop-has-money)
            :del-list '(have-money))))

(gps '(son-at-home car-needs-battery have-money have-phone-book)
     '(son-at-school)
     *school-ops*)

(gps '(son-at-home car-needs-battery have-money)
     '(son-at-school)
     *school-ops*)

(gps '(son-at-home car-works) '(son-at-school) *school-ops*)

;;
;; But the solution of this is not correct, the problem being that we
;; achieve states in succession, and not simultaneously.
;;
(gps '(son-at-home car-needs-battery have-money have-phone-book)
     '(have-money son-at-school)
     *school-ops*)

(when nil
  ;; Adding a cycled goal will make the recursion infinite...
  (let ((*extended-ops* (cons (make-op :action 'ask-phone-number
                                       :preconds '(in-communication-with-shop)
                                       :add-list '(know-phone-number))
                              *school-ops*)))

    ;; ... and this call will exhaust stack.
    (gps '(son-at-home car-needs-battery have-money)
         '(son-at-school)
         *extended-ops*)))

(trace achieve)

;;
;; 4.10 The Lack of Intermediate Information Problem
;;

(defvar *dbg-ids* nil "Identifiers used by dbg")

(defun dbg (id format-string &rest args)
  "Print debugging info if (DEBUG-NORVIG ID) has been specified."
  (when (member id *dbg-ids*)
    (fresh-line *debug-io*)
    (apply #'format *debug-io* format-string args)))

(defun debug-norvig (&rest ids)
  "Start dbg output on the given ids."
  (setf *dbg-ids* (union ids *dbg-ids*)))

(defun undebug-norvig (&rest ids)
  "Stop dbg on the ids. With no ids, stop dbg altogether."
  (setf *dbg-ids* (if (null ids)
                      nil
                      (set-difference *dbg-ids* ids))))

(defun dbg-indent (id indent format-string &rest args)
  "Print indented debugging info if (debug-norvig ID) has been specified."
  (when (member id *dbg-ids*)
    (fresh-line *debug-io*)
    (dotimes (i indent) (princ " " *debug-io*))
    (apply #'format *debug-io* format-string args)))
