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

;;
;; just playing around with the syntax of Common Lisp
;;

(defun make-thing (name &optional (yes nil) (no nil))
  (list name yes no))

(defun thing-name (thing)
  (nth 0 thing))

(defun thing-yes (thing)
  (nth 1 thing))

(defun thing-no (thing)
  (nth 2 thing))
