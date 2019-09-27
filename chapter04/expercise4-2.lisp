;;
;; Exercise 4.2
;;

(defun mappend (fn the-list)
  "Apply fn to each element of list and append the results."
  (apply #'append (mapcar fn the-list)))

(defun suffices (lst)
  "Return list of suffices of lst."
  (reduce #'(lambda (x acc)
              (cons (cons x (car acc)) acc))
          lst :from-end t :initial-value '(())))

(defun prefices (lst)
  "Return list of prefices of lst."
  (reverse (reduce #'(lambda (acc x)
                       (cons (cons x (car acc)) acc))
                   lst :initial-value '(()))))

(defun insert-at-each-position (el items)
  ;; Return list of lists consisting of inserting el into items at all
  ;; possible positions.
  (let ((prefs (prefices items))
        (suffs (suffices items))
        (elist (list el)))
    (mapcar #'(lambda (p s) (append p elist s)) prefs suffs)))

(defun permutations (items)
  ;; Return all permutations of items.
  (if (< (length items) 2)
      (list items)
      (mappend #'(lambda (permutation)
                   (insert-at-each-position (car items) permutation))
               (permutations (cdr items)))))


(permutations '(1 2))
(permutations '(1 2 3))
(permutations '(1 2 3 4))

(defun permutations-norvig (bag)
  "Return a list of all the permutations of the input."
  (if (null bag)
      '(())
      (mapcan #'(lambda (e)
                  (mapcar #'(lambda (p) (cons e p))
                          (permutations-norvig
                           (remove e bag :count 1 :test #' eq))))
              bag)))

(permutations-norvig '(1 2 3 4))
