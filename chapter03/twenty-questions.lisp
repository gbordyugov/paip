;;
;; Exercise 3.5
;;

(defun ask (thing)
  (progn
    (format t "is it ~A?~%" thing)
    (eql 'yes (read))))

(defun twenty-questions ()
  t)
