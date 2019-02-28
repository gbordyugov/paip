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

(defun recurse-yes (thing)
  t)

(defun recurse-no (thing)
  t)

(defun give-up (thing)
  t)

(defun bingo! (thing)
  t)

(defun rec (thing)
  (case (ask-about thing)
    (yes (recurse-yes thing))
    (no  (recurse-no  thing))
    (it  (bingo!      thing))))

(defun ask-about (thing)
  (progn
    (format t "Is it ~A? " (thing-name thing))
    (case (read)
      ((y yes) 'yes)
      ((n no ) 'no)
      (it      'it)
      (t       (ask-about thing)))))

;; (ask-about (make-thing 'thing))
