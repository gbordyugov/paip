;;
;; Exercise 4.2
;;

(defun mappend (fn the-list)
  "Apply fn to each element of list and append the results."
  (apply #'append (mapcar fn the-list)))

(defun suffices (lst)
  "Return list of suffices of lst."
  '())

(defun prefices (lst)
  "Return list of prefices of lst."
  '())

(defun insert-at-each-position (el items)
  ;; Return list of lists consisting of inserting el into items at all
  ;; possible positions.
  (let ((prefs (prefices items))
        (suffs (suffices items))
        (elist (list el)))
    (mapcar #'(lambda (p s) (append p elist s))
            prefs suffs)))

(insert-at-each-position 5 '(1 2 3))

(defun permutations (items)
  ;; Return all permutations of items.
  (if (< (length items) 2)
      items
      ;; I don't really know how to best proceed here.
      (let ((rest-perms (permutations (cdr items))))
        (mapcar #'(lambda (x) (x) items)))))

(permutations '(1 2))
