;;
;; A purely immutable version of the GPS from Chapter 4.
;;

;; An operator is represented as a structure composed of an action, a
;; list of preconditions, and a list of effects. The list of effects
;; can be split into an add-list (what conditions this operator adds)
;; and a delete-list (what conditions this operator deletes).
(defstruct op "An operation"
           (action nil)
           (preconds nil)
           (add-list nil)
           (del-list nil))

(defun apply-op (op state)
  "Apply op to state, returning the resulting state. Does not check if op
   can be applied to state."
  (let* ((reduced-state (set-difference state (op-del-list op))))
    (union reduced-state (op-add-list op))))

(let ((state '(bli blu))
      (op (make-op :action 'what
                     :preconds '()
                     :add-list '(bla)
                     :del-list '(blu))))
  (apply-op op state))

(defun op-applicable-p (state op)
  "Check if operator can be applied to state."
  (every #'(lambda (prec) (member prec state)) (op-preconds op)))

(let ((state '(bli blu))
      (op1 (make-op :action 'what
                     :preconds '(bla)
                     :add-list '(ble)
                     :del-list '(blu)))
      (op2 (make-op :action 'what
                     :preconds '(bli)
                     :add-list '(bla)
                     :del-list '(blu))))
  (op-applicable-p state op2))

;;
;; Auxiliary functions.
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

(defun filter (predicate seq)
  "Return list containing only those elements of `seq` that satisfy the unary
   `predicate`. Uses tail recursion."
  (labels ((rec (seq acc)
             (cond
               ((null seq) acc)
               ((funcall predicate (first seq))
                (rec (rest seq) (cons (first seq) acc)))
               (t (rec (rest seq) acc)))))
    (reverse (rec seq '()))))

(filter #'oddp '(1 2 3 4 5 6 7))
