;;
;; The main structure to represent an operation.
;;
(defstruct op "An operation"
           (action nil)
           (preconds nil)
           (add-list nil)
           (del-list nil))
