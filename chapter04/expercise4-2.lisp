;;
;; Exercise 4.2
;;

(defun mappend (fn the-list)
  "Apply fn to each element of list and append the results."
  (apply #'append (mapcar fn the-list)))

(defun insert-at-each-position (x items)
  ;; Return list of lists consisting of inserting x into items at all
  ;; possible positions.
  '())

(defun permutations (items)
  ;; Return all permutations of items.
  (if (< (length items) 2)
      items
      ;; I don't really know how to best proceed here.
      (let ((rest-perms (permutations (cdr items))))
        (mapcar #'(lambda (x) (x) items)))))

(permutations '(1 2))
