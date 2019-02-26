;;
;; Exercise 3.5
;;

(defun ask (thing)
  (progn
    (format t "Is it ~A?" thing)
    (case (read)
      ((y yes) (prin1 'wow!))
      ((n no ) (prin1 'nopes))
      (t       (prin1 'whatever)))
    'ok))

(read)

(ask 'thing)

'bla

(defun twenty-questions ()
  t)

(defstruct question
  name
  (yes nil)
  (no  nil))

(make-question :name 'thing)
