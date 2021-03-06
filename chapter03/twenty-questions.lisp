;;
;; Exercise 3.5
;;

;;
;; constructor of the data structure
;;
(defun make-thing (name &optional (yes nil) (no nil))
  (list name yes no))

;;
;; accessors
;;
(defun thing-name (thing)
  (nth 0 thing))

(defun thing-yes (thing)
  (nth 1 thing))

(defun thing-no (thing)
  (nth 2 thing))


;;
;; the main recursion
;;

(defun give-up ()
  (progn
    (format t "Giving up. What is this? ")
    (let ((name (read)))
      (make-thing name))))

(defun bingo! (thing)
  (progn
    (format t "Bingo!")
    thing))

(defun ask-about (thing)
  (progn
    (format t "Is it a ~A? " (thing-name thing))
    (case (read)
      ((y yes) 'yes)
      ((n no ) 'no)
      (it      'it)
      ;; incomprehensible reply, repeat the question
      (t       (ask-about thing)))))

(defun check-and-recurse (thing)
  (if thing
      (recurse thing)
      (give-up)))

(defun recurse (thing)
  (let ((name (thing-name thing))
        (yes  (thing-yes  thing))
        (no   (thing-no   thing)))
    (case (ask-about thing)
      (yes (make-thing name (check-and-recurse yes) no))
      (no  (make-thing name yes (check-and-recurse no)))
      (it  (bingo! thing)))))

(defun play (&optional (thing (make-thing 'thing)))
  (progn
    (format t "~&My database: ~a ~&" thing)
    (play (recurse thing))))

(play)
