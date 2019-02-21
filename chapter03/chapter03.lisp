;;
;; Chapter 3.2 Special Forms
;;

(defstruct name
  first
  (middle nil)
  last)

(setf b (make-name :first 'Barney :last 'Rubble))

b

(name-first b)

(name-middle b)

(name-last b)

(name-p b)

(name-p 'Barney)

(setf (name-middle b) 'Q)

b

;;
;; Conditionals
;;

(setf n 101)

(and (> n 100) (princ "N is large."))

(or (<= n 100) (princ "N is large."))

(cond ((> n 100) (princ "N is large.")))

(when (> n 100) (princ "N is large."))

(defun tax-bracket (income)
  "Determine what percent tax should be paid for this income."
  (cond ((< income 10000.0) 0.00)
        ((< income 30000.0) 0.20)
        ((< income 50000.0) 0.25)
        ((< income 70000.0) 0.30)
        (t                  0.35)))

(tax-bracket 50000.0)

(defun case-example-1 (x)
  (case x
    (1 10)
    (2 20)))

(defun case-example-2 (x)
  (case x
    ((1 2 3) 'ok)
    ((3 4 5) 'not-ok)))

(case-example-2 6)

(defun typecase-example (x)
  (typecase x
    (number (abs x))
    (list (length x))))

;;
;; Special Forms for Dealing with Variables and Places
;;

(let ((x 40)
      (y (+ 1 1)))
  (+ x y))

((lambda (x y) (+ x y)) 40 (+ 1 1))

(let* ((x 6)
       (y (* x x)))
  (+ x y))

;;
;; Exercise 3.1
;;

;;
;; this is the simple `let` above
;;
((lambda (x y) (+ x y)) 40 (+ 1 1))

;;
;; this is the starred `let*` above
;;
((lambda (x)
   ((lambda (y)
      (+ x y))
    (* x x)))
 6)

;;
;; one more test
;;
(let* ((x 2)
       (y (* 2 x))
       (z (+ 3 y)))
  (+ x y z))

((lambda (x)
   ((lambda (y)
      ((lambda (z)
         (+ x y z))
       (+ 3 y)))
    (* 2 x)))
 2)

;;
;; four nested lambdas test
;;
(let* ((a (+ 3 3))
       (b (+ a 3))
       (c (+ b a))
       (d (* a b c)))
  (+ a b c d))

((lambda (a)
   ((lambda (b)
     ((lambda (c)
       ((lambda (d)
         (+ a b c d))
        (* a b c)))
      (+ b a)))
    (+ a 3)))
 (+ 3 3))

;;
;; Structs stuff
;;

(defstruct player
  (score 0)
  (wins 0))

(defun determine-winner-1 (players)
  "Increment the WINS for the player with highest score."
  (incf (player-wins (first (sort players #'> :key #'player-score)))))

(defun determine-winner-2 (players)
  "Increment the WINS for the player with highest score."
  (let ((temp (first (sort players #'> :key #'player-score))))
    (setf (player-wins temp) (+ (player-wins temp) 1))))

;;
;; Functions and Special Forms for Repetition
;;

(defun length1 (list)
  (let ((len 0))
    (dolist (element list)
      (incf len))
    len))

(defun length1.1 (list)
  (let ((len 0))
    (dolist (element list len)
      (infc len))))

(defun length2 (list)
  (let ((len 0))
    (mapc #'(lambda (element) (incf len)) list)
    len))

(defun length3 (list)
  (do ((len 0 (+ len 1))
       (l list (rest l)))
      ((null l) len)))

;;
;; The `loop` macro
;;

(defun length4 (list)
  (loop for element in list
       count t))

(defun length5 (list)
  (loop for element in list
     summing l))

(defun length6 (list)
  (loop with len = 0
       until (null list)
       for element = (pop list)
       do (incf len)
       finally (return len)))

(defun length7 (list)
  (count-if #'true list))

(defun true (x)
  t)

(defun length8 (list)
  (if (null list)
      0
      (+ 1 (position-if #'true list :from-end t))))

(mapcar #'- '(1 2 3))
(mapcar #'+ '(1 2) '(10 20))
(mapcar #'+ '(1 2) '(10 20) '(100 200))

(remove 1 '(1 2 3 2 1 0 -1))
(remove 1 '(1 2 3 2 1 0 -1) :key #'abs)
(remove 1 '(1 2 3 2 1 0 -1) :test #'<)
(remove 1 '(1 2 3 2 1 0 -1) :start 4)

(remove-if #'oddp '(1 2 3 2 1 0 -1))
(remove-if-not #'oddp '(1 2 3 2 1 0 -1))
(find-if #'evenp '(1 2 3 2 1 0 -1))

;;
;; Repetition through Recursion
;;

(defun length9 (list)
  (if (null list)
      0
      (+ 1 (length9 (rest list)))))

(length9 '(a b c))

(defun length10 (list)
  (length10-aux list 0))

(defun length10-aux (sublist len-so-far)
  (if (null sublist)
      len-so-far
      (length10-aux (rest sublist) (+ 1 len-so-far))))

(length10 '(a b c d))

(defun length11 (list &optional (len-so-far 0))
  (if (null list)
      len-so-far
      (length11 (rest list) (+ 1 len-so-far))))

(length11 '(a b c d e f))

(defun length12 (the-list)
  (labels ((length13 (list len-so-far)
             (if (null list)
                 len-so-far
                 (length13 (rest list) (+ 1 len-so-far)))))
    (length13 the-list 0)))

(length12 '(a b c d))

;;
;; Other Special Forms
;;

(progn (setf x 0) (setf x (+ x 1)) x)

;;
;; Macros
;;

(defmacro while (test &rest body)
  "Repeat body while test is true."
  (list* 'loop
         (list 'unless test '(return nil))
         body))

(macroexpand-1 '(while (< i 10) (print (* i i)) (setf i (+ i 1))))

(setf i 7)

(while (< i 10)
  (print (* i i))
  (setf i (+ i 1)))

;;
;; Backquote notation
;;

(defmacro while (test &rest body)
  "Repeat body while test is true."
  `(loop (unless ,test (return nil))
      ,@body))

(macroexpand-1 '(while (< i 10) (print (* i i)) (setf i (+ i 1))))

(setf test1 '(a test))
`(this is ,test1)
`(this is ,@test1)
`(this is . ,test1)
`(this is ,@test1 -- this is only ,@test1)

;;
;; Exercise 3.2
;;
;; it's list*
;;
