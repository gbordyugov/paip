;;
;; Exercise 4.2
;;
(defun permutations (items)
  (if (< (length items) 2)
      items
      ;; I don't really know how to best proceed here.
      (let ((perms (permutations (cdr items))))
        (mapcar #'(lambda (x) (x) items)))))

(permutations '(1 2))
