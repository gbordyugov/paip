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

(defstruct op "An operation"
           (action nil)
           (preconds nil)
           (add-list nil)
           (del-list nil))

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

;;
;; This is only the bookkeeping for ops in new and old format
;;
(defun executing-p (x)
  "Is x of the form: (executing ...)?"
  (starts-with x 'executing))

(defun starts-with (list x)
  "Is this a list whose first element is x?"
  (and (consp list) (eql (first list) x)))

(defun convert-op (op)
  "Make op conform to the (EXECUTING op) convention."
  (unless (some #'executing-p (op-add-list op))
    (let ((execute-statement (list 'executing (op-action op))))
      (push execute-statement (op-add-list op))))
  op)

(when '()
  (let ((example-op (make-op :action 'give-shop-money
                             :preconds '(have-money)
                             :add-list '(shop-has-money)
                             :del-list '(have-money))))
    (convert-op example-op)))

(defun op (action &key preconds add-list del-list)
  "Make a new operator that obeys the (EXECUTING op) convention."
  (convert-op
   (make-op :action action :preconds preconds :add-list add-list
            :del-list del-list)))

(mapc #'convert-op *school-ops*)

;;
;; Debugging infrastructure.
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

;;
;; Here starts the main logic of the solver.
;;

;;
;; The chain of calls:
;;
;; achieve-all -> achieve-each -> achieve
;;
(defun achieve-all (state goals goal-stack)
  "The main entry point, called from GPS. Generate some permutations
   of goals (see `orderings` below) and tries to achieve some of those
   permutations."
  (let ((goals-permutations (orderings goals)))
    (labels ((local-achieve (goals)
               (achieve-each state goals goal-stack)))
      (some #'local-achieve goals-permutations))))

(defun orderings (l)
  "For a list with one element, returns a list, consisting of the
   original list. For a list with more than one element, return a list
   consisting of the original list and its reverse."
  (if (> (length l) 1)
      (list l (reverse l))
      (list l)))

(defun achieve-each (state goals goal-stack)
  "Achieve each goal sequentially, using the output state of achieving
   one goal as a start state for achieving the next one. Make sure
   they still hold at the end."
  (let ((current-state state))
    (labels ((local-achieve (g)
               (setf current-state (achieve current-state g goal-stack))))
      (if (and (every #'local-achieve goals)
               ;; make sure that all target goals still hold
               (subsetp goals current-state :test #'equal))
          current-state))))

(defun achieve (state goal goal-stack)
  "A goal is achieved if it already holds, or if there is an
   appropriate op for it that is applicable."
  (dbg-indent :gps (length goal-stack) "Goal: ~a" goal)
  (cond ((member-equal goal state) state)
        ;; Recursive subgoal, bail out immediately.
        ((member-equal goal goal-stack) nil)
        (t (labels ((local-apply-op (op)
                      (apply-op state goal op goal-stack)))
             ;; `some` returns the first non-nil result of apply the
             ;; func. The list of appropriate ops is sorted by the
             ;; number of unfulfilled conditions in an ascending
             ;; order (heuristics!).
             (some #'local-apply-op (appropriate-ops goal state))))))

(defun appropriate-ops (goal state)
  "Return a list of appropriate operators, sorted by the number of
   unfulfilled preconditions."
  (labels ((appropriate-p (goal op)
             (member-equal goal (op-add-list op)))
           (not-appropriate-p (goal op)
             (not (appropriate-p goal op)))
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
  (let ((state2 (achieve-all state (op-preconds op)
                             (cons goal goal-stack))))
    (unless (null state2)
      (dbg-indent :gps (length goal-stack) "Action: ~a" (op-action op))
      (append (remove-if #'(lambda (x)
                             (member-equal x (op-del-list op)))
                         state2)
              (op-add-list op)))))

(defun use (oplist)
  "Use oplist as the default list of operators."
  ;; Return something useful, but not too verbose: the number of operators.
  (length (setf *ops* oplist)))

(defun GPS (state goals &optional (*ops* *ops*))
  "General Problem Solver v2: from state, achieve goals using *ops*. What it
   basically does is extending the initial state by adding '(start) to it and
   delegating the job to `achieve-all` with the extended initial state. After that,
   it removes from the end state everyting which is not either '(state) or
   '(executing smth.)"
  (let* ((ini-state (cons '(start) state))
         (end-state (achieve-all ini-state goals nil)))
    (remove-if #'not-action-p end-state)))

(defun not-action-p (x)
  "Is x not something that is (start) or (executing ...)?"
  (not (or (equal x '(start)) (executing-p x))))

(defvar *ops* nil
  "A list of available operators.")

(use *school-ops*)

(GPS '(son-at-home car-needs-battery have-money have-phone-book)
     '(son-at-school))

(use (push (op 'taxi-son-to-school
               :preconds '(son-at-home have-money)
               :add-list '(son-at-school)
               :del-list '(son-at-home have-money))
           *school-ops*))

;;
;; This one doesn't work because of the not looking after you don't
;; leap problem.
;;
(debug-norvig :gps)

(GPS '(son-at-home have-money car-works)
     '(son-at-school have-money))
