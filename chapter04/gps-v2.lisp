;;
;; Chapter 4.11 GPS Version 2: A More General Problem solver
;;

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
;; Operations.
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

;; The idea here is to have a list of the form (executing operator) on
;; add-list of operators, see convert-op below.
(defun executing-p (x)
  "Is x of the form: (executing ...)?"
  (starts-with x 'executing))

(defun starts-with (list x)
  "Is this a list whose first element is x?"
  (and (consp list) (eql (first list) x)))

;; Since the elements of the add-list are to be added to the state
;; upon execution of the operator, adding a (executing ...) form to
;; the add-list make this (executing ...) form appear on the list
;; which represents the state after the application of the operator.
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
  "Achieve each goal, and make sure they still hold at the end."
  (let ((current-state state))
    (if (and (every #'(lambda (g)
                        (setf current-state
                              (achieve current-state g goal-stack)))
                    goals)
             (subsetp goals current-state :test #'equal))
        current-state)))

;; The new version that users achieve-each
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

;; Attempt to achieve goal from stack. Goal-stack represents the stack
;; of goals that we're working on. It is used to prevent the recursive
;; sub-goal problem.
(defun achieve (state goal goal-stack)
  "A goal is achieved if it already holds, or if there is an
   appropriate op for it that is applicable."
  ;; (length goal-stack) is used as the amount of indent.
  (dbg-indent :gps (length goal-stack) "Goal: ~a" goal)
  (cond ((member-equal goal state) state)
        ;; Immediately fail if a goal is a subgoal of itself.
        ((member-equal goal goal-stack) nil)
        (t (some #'(lambda (op) (apply-op state goal op goal-stack))
                 ;; (find-all goal *ops* :test #'appropriate-p)))))
                 (remove goal *ops*
                         :test #'(lambda (goal op)
                                   (not (appropriate-p goal op))))))))

(defun member-equal (item list)
  (member item list :test #'equal))

(defun apply-op (state goal op goal-stack)
  "Return a new, transformed state if op is applicable."
  ;; (length goal-stack) is used as the amount of indent.
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

;; (defun GPS (state goals &optional (ops *ops*))
;;   "General Problem Solver: from state, achieve goals using ops."
;;   (let ((old-ops *ops*))
;;     (setf *ops* ops)
;;     (let ((result (remove-if #'atom (achieve-all (cons '(start) state)
;;                                                  goals nil))))
;;       (setf *ops* old-ops)
;;       result)))

(use *school-ops*)

(gps '(son-at-home car-needs-battery have-money have-phone-book)
     '(son-at-school))

(debug-norvig :gps)

(undebug-norvig)

;; An easy one.
(gps '(son-at-home car-works)
     '(son-at-school))

;; Three cases that GPS v1 got wrong
(gps '(son-at-home car-needs-battery have-money have-phone-book)
     '(have-money son-at-school))

(gps '(son-at-home car-needs-battery have-money have-phone-book)
     '(son-at-school have-money))

(gps '(son-at-home car-needs-battery have-money)
     '(son-at-school))

;; A trivial one.
(gps '(son-at-home) '(son-at-home))


;;
;; Chapter 4.12 Monkey and Bananas
;;

(defparameter *banana-ops*
  (list
   (op 'climb-on-chair
       :preconds '(chair-at-middle-room at-middle-room on-floor)
       :add-list '(at-bananas on-chair)
       :del-list '(at-middle-room on-floor))
   (op 'push-chair-from-door-to-middle-room
       :preconds '(chair-at-door at-door)
       :add-list '(chair-at-middle-room at-middle-room)
       :del-list '(chair-at-door at-door))
   (op 'walk-from-door-to-middle-room
       :preconds '(at-door on-floor)
       :add-list '(at-middle-room)
       :del-list '(at-door))
   (op 'grasp-bananas
       :preconds '(at-bananas empty-handed)
       :add-list '(has-bananas)
       :del-list '(empty-handed))
   (op 'drop-ball
       :preconds '(has-ball)
       :add-list '(empty-handed)
       :del-list '(has-ball))
   (op 'eat-bananas
       :preconds '(has-bananas)
       :add-list '(empty-handed not-hungry)
       :del-list '(has-bananas hungry))))

(use *banana-ops*)

(debug-norvig :gps)

(undebug-norvig)

(gps '(at-door on-floor has-ball hungry chair-at-door)
     '(not-hungry))

;;
;; Chapter 4.13 The Maze Searching Domain
;;

(defun make-maze-ops (pair)
  "Make maze ops in both directions."
  (list (make-maze-op (first pair) (second pair))
        (make-maze-op (second pair) (first pair))))

(defun make-maze-op (here there)
  "Make an operator to move between two places."
  (op `(move from ,here to ,there)
      :preconds `((at ,here))
      :add-list `((at ,there))
      :del-list `((at ,here))))

(defun mappend (fn the-list)
  "Apply fn to each element of list and append the results."
  (apply #'append (mapcar fn the-list)))

(defparameter *maze-ops*
  (mappend #'make-maze-ops
           '((1 2) (2 3) (3 4) (4 9) (9 14) (9 8) (8 7) (7 12) (12 13)
             (12 11) (11 6) (11 16) (16 17) (17 22) (21 22) (22 23)
             (28 18) (23 24) (24 19) (19 20) (20 15) (15 10) (10 5) (20 25))))

(use *maze-ops*)

(gps '((at 1)) '((at 25)))

(defun find-path (start end)
  "Search a maze for a path from start to end."
  (let ((results (GPS `((at ,start)) `((at ,end)))))
    (unless (null results)
      (cons start (mapcar #'destination
                          (remove '(start) results :test #'equal))))))

(defun destination (action)
  "Find the Y in (executing (move from X to Y))."
  (fifth (second action)))

(find-path 1 25)

(find-path 1 1)

(equal (find-path 1 25) (reverse (find-path 25 1)))

;;
;; Chapter 4.14 The Blocks World Domain
;;

(defun make-block-ops (blocks)
  (let ((ops nil))
    (dolist (a blocks)
      (dolist (b blocks)
        (unless (equal a b)
          (dolist (c blocks)
            (unless (or (equal c a) (equal c b))
              (push (move-op a b c) ops)))
          (push (move-op a 'table b) ops)
          (push (move-op a b 'table) ops))))
    ops))

(defun move-op (a b c)
  "Make an operator to move A from B to C."
  (op `(move ,a from ,b to ,c)
      :preconds `((space on ,a) (space on ,c) (,a on ,b))
      :add-list (move-ons a b c)
      :del-list (move-ons a c b)))

(defun move-ons (a b c)
  (if (eq b 'table)
      `((,a on ,c))
      `((,a on ,c) (space on ,b))))

(use (make-block-ops '(a b)))

(gps '((a on table) (b on table) (space on a) (space on b) (space on table))
     '((a on b) (b on table)))

(debug-norvig :gps)

(gps '((a on b) (b on table) (space on a) (space on table))
     '((b on a)))

(undebug-norvig)

(use (make-block-ops '(a b c)))

;; This one works.
(gps '((a on b) (b on c) (c on table) (space on a) (space on table))
     '((b on a) (c on b)))

;; But this one doesn't, at least, with the first version of GPS v2.
;; It recognises the clobbering, but doesn't know what to do about it.
;; Introducing a new version of achieve-all (with achieve-each) solves
;; the problem.
(gps '((a on b) (b on c) (c on table) (space on a) (space on table))
     '((c on b) (b on a)))


;; A simple case with an overly complicated solution.
(gps '((c on a) (a on table) (b on table) (space on c) (space on b)
       (space on table))
     '((c on table)))
