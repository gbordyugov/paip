;;
;; Exercise 4.4
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

(defun executing-p (x)
  "Is x of the form: (executing ...)?"
  (starts-with x 'executing))

(defun starts-with (list x)
  "Is this a list whose first element is x?"
  (and (consp list) (eql (first list) x)))

(defun convert-op (op)
  "Make op conform to the (EXECUTING op) convention."
  (unless (some #'executing-p (op-add-list op))
    (push (list 'executing (op-action op)) (op-add-list op)))
  op)

(defun op (action &key preconds add-list del-list)
  "Make a new operator that obeys the (EXECUTING op) convention."
  (convert-op
   (make-op :action action :preconds preconds :add-list add-list
            :del-list del-list)))

(mapc #'convert-op *school-ops*)

(defvar *ops* nil
  "A list of available operators.")

(defun achieve-all (state goals goal-stack)
  (some #'(lambda (goals) (achieve-each state goals goal-stack))
        (orderings goals)))

(defun achieve-each (state goals goal-stack)
  "Achieve each goal, and make sure they still hold at the end."
  (let ((current-state state))
    (if (and (every #'(lambda (g)
                        (setf current-state
                              (achieve current-state g goal-stack)))
                    goals)
             (subsetp goals current-state :test #'equal))
        current-state)))

(defun orderings (l)
  "For a list with one element, returns a list, consisting of the
   original list. For a list with more than one element, return a list
   consisting of the original list and its reverse."
  (if (> (length l) 1)
      (list l (reverse l))
      (list l)))

(defun achieve (state goal goal-stack)
  "A goal is achieved if it already holds, or if there is an
   appropriate op for it that is applicable."
  (cond ((member-equal goal state) state)
        ;; Recursive subgoal, bail out immediately.
        ((member-equal goal goal-stack) nil)
        (t (some #'(lambda (op) (apply-op state goal op goal-stack))
                 (appropriate-ops goal state)))))

(defun appropriate-ops (goal state)
  "Return a list of appropriate operators, sorted by the number of
   unfulfilled preconditions."
  (let ((ops (remove goal *ops*
                     :test #'(lambda (goal op)
                               (not (appropriate-p goal op))))))
    (sort (copy-list ops) #'< :key #'(lambda (op)
                                       (count-if #'(lambda (precond)
                                                     (not (member-equal precond state)))
                                                 (op-preconds op))))))

(defun member-equal (item list)
  (member item list :test #'equal))

(defun apply-op (state goal op goal-stack)
  "Return a new, transformed state if op is applicable."
  ;; Extend goal stack by the current goal.
  (let ((state2 (achieve-all state (op-preconds op)
                             (cons goal goal-stack))))
    (unless (null state2)
      (append (remove-if #'(lambda (x)
                             (member-equal x (op-del-list op)))
                         state2)
              (op-add-list op)))))

(defun appropriate-p (goal op)
  "An op is appropriate to a goal if it is in its add-list."
  (member-equal goal (op-add-list op)))

(defun use (oplist)
  "Use oplist as the default list of operators."
  ;; Return something useful, but not too verbose: the number of operators.
  (length (setf *ops* oplist)))

(defun GPS (state goals &optional (*ops* *ops*))
  "General Problem Solver v2: from state, achieve goals using *ops*."
  (remove-if #'not-action-p (achieve-all (cons '(start) state) goals nil)))

(defun not-action-p (x)
  "Is x not something that is (start) or (executing ...)?"
  (not (or (equal x '(start))
           (executing-p x))))

(use *school-ops*)

(gps '(son-at-home car-needs-battery have-money have-phone-book)
     '(son-at-school))
