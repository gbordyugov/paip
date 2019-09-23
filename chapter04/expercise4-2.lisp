;;
;; Exercise 4.2
;;
(defun permutations (items)
  (if (< (length items) 2)
      items
      '()))

(permutations '(1 2))
