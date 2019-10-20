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
      ;; In words:
      ;; For each element e of bag, remove this element from bag.
      ;; Generate all permutatations of the rest and cons e onto each
      ;; of those permutations.
      (mapcan #'(lambda (e)
                  (let ((smaller-bag (remove e bag :count 1 :test #' eq)))
                    (mapcar #'(lambda (p) (cons e p))
                            (permutations-norvig smaller-bag))))
              bag)))

;;
;; I do not understand the role of MAPCAN here and am trying to figure
;; it out below.
;;
;; What I'm starting to understand now:
;; - '(()) is a list of length one, with one element being an empty list '()
;;

;;
;; Examples of unrolled body of PERMUTATIONS-NORVIG for the trivial
;; case. This is the correct one, it uses MAPCAN.
;;
(mapcan #'(lambda (e)
           (mapcar #'(lambda (p) (cons e p))
                    '(())))
        '(1))

(permutations-norvig '(1))
(permutations-norvig '(1 2))
(permutations-norvig '(1 2 3))
(permutations-norvig '(1 2 3 4))

;;
;; That's an incorrect implementation using MAPCAR in place of MAPCAN
;; above.
;;
(defun permutations-norvig-mapcar (bag)
  "Return a list of all the permutations of the input."
  (if (null bag)
      '(())
      ;; In words:
      ;; For each element e of bag, remove this element from bag.
      ;; Generate all permutatations of the rest and cons e onto each
      ;; of those permutations.
      (mapcar #'(lambda (e)
                  (let ((smaller-bag (remove e bag :count 1 :test #' eq)))
                    (mapcar #'(lambda (p) (cons e p))
                            (permutations-norvig-mapcar smaller-bag))))
              bag)))

;;
;; The one with MAPCAR adds another level of structure.
;;
(mapcar #'(lambda (e)
           (mapcar #'(lambda (p) (cons e p))
                    '(())))
        '(1))

(permutations-norvig-mapcar '(1))
(permutations-norvig-mapcar '(1 2))
(permutations-norvig-mapcar '(1 2 3))
(permutations-norvig-mapcar '(1 2 3 4))

;;
;; Toiling furhter...
;;
(let ((perms '((1 2) (2 1))))
  (mapcar #'(lambda (p) (cons 0 p)) perms))

;;
;; I have to implement it from scratch in order to grok it.
;;
(defun my-norvig-permutations (bag)
  "My take on Norvig's logic of calculating the permutations."
  (if (null bag)
      '(())
      ;; For each item in bag, remove it from bag, compute all
      ;; permutations of the rest and prepend the item to all the
      ;; permutations.
      (mapcan #'(lambda (element)
                  (let* ((rest-bag (remove element bag))
                         (rest-perms (my-norvig-permutations rest-bag)))
                    (mapcar #'(lambda (perm)
                                (cons element perm))
                            rest-perms)))
              bag)))

(my-norvig-permutations '(1))
(my-norvig-permutations '(1 2))
(my-norvig-permutations '(1 2 3))
(my-norvig-permutations '(1 2 3 4))
