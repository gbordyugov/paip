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

(defun update-thing-with-answer (thing answer)
  (case answer
    ((y yes) (let ((name      (thing-name thing))
                   (yes-child (thing-yes thing))
                   ( no-child (thing-no thing))
                   (next-answer    (question)))
               (make-thing name
                           (update-thing-with-answer next-answer)
                           no-child)))
    ((n no) (let ((name      (thing-name thing))
                  (yes-child (thing-yes thing))
                  ( no-child (thing-no thing))
                  (next-answer    (question)))
              (make-thing name
                          yes-child
                          (update-thing-with-answer next-answer))))
    (it thing)))
