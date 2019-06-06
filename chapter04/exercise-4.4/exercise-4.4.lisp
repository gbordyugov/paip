;;
;; Exercise 4.4
;;

;;
;; The Not Looking after You Don't Leap Problem. Write a program that
;; keeps track of the remaining goals so that it does not get stuck
;; considering only one possible operation when others will eventually
;; lead to the goal. Hint: have `achieve` take an extra argument
;; indicating the goals that remain to be achieved after the current
;; goal is achieved. `achieve` should succeed only if it can achieve
;; the current goal and also `achieve-all` the remaining goals.
;;

;;
;; First off: the not looking before you leep problem: Trying to
;; achieve one of the goals can block achieving some other goals on
;; the goal list, i.e. (jump-off-cliff land-safely).
;;
;; In the second version of GPS this problem is solved by passing the
;; current state around and returning an updated state from functions.
;;

;;
;; The Not Looking after You Don't Leap Problem.
;;
;; Suppose we're trying to achieve (son-at-school have-money) given
;; (son-at-home have-money car-works). Additionally, suppose that we
;; have an operator that takes son to the school by a cab and consumes
;; money. This operator is the first one on the operator list. So each
;; time it is applied, we loose money, which contradicts one of the
;; final goals. We cannot achieve all the goals although there is a
;; valid solution: Driving son to school ourselves.
;;

(load "ops.lisp")

(load "school-ops.lisp")

(load "debug.lisp")

(load "solver-exercise-4.4.lisp")

(use *school-ops*)

;;
;; The standard problem.
;;
(GPS '(son-at-home car-needs-battery have-money have-phone-book)
     '(son-at-school))

(use (push (op 'taxi-son-to-school
               :preconds '(son-at-home have-money)
               :add-list '(son-at-school)
               :del-list '(son-at-home have-money))
           *school-ops*))

(debug-norvig :gps)

;;
;; This one doesn't work because of the not looking after you don't
;; leap problem. The actual solution would be to drive son to school
;; ourselves, but the solver gets stuck by driving son to school with
;; taxi and then trying to fulfill `have-money` goal.
;;
(GPS '(son-at-home have-money car-works)
     '(son-at-school have-money))
