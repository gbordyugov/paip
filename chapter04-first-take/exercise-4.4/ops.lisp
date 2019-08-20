(defstruct op "An operation"
           (action nil)
           (preconds nil)
           (add-list nil)
           (del-list nil))

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

(defvar *ops* nil
  "A list of available operators.")

(defun use (oplist)
  "Use oplist as the default list of operators."
  ;; Return something useful, but not too verbose: the number of operators.
  (length (setf *ops* oplist)))
