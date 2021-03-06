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

;;
;; Exercise 3.3
;;

(defun dotted-print (exp)
  (if (atom exp)
      (princ exp)
      (progn
        (princ "(")
        (princ (first exp))
        (princ " . ")
        (dotted-print (rest exp))
        (princ ")"))))

(dotted-print 5)

(dotted-print '(1 2 3 4))

;;
;; Exercise 3.4 skipped
;;

;;
;; 3.6 Functions for Maintaining Tables
;;

(setf state-table
      '((AL. Alabama) (AK . Alaska) (AZ . Arizona) (AR . Arkansas)))

(assoc 'AK state-table)
(cdr (assoc 'AK state-table))
(assoc 'TX state-table)

(rassoc 'Arizona state-table)

(setf table (make-hash-table))

(setf (gethash 'AL table) 'Alabama)
(setf (gethash 'AK table) 'Alaska)
(setf (gethash 'AZ table) 'Arizona)
(setf (gethash 'AR table) 'Arkansas)

(gethash 'AK table)
(gethash 'TX table)

(setf (get 'AL 'state) 'Alabama)
(setf (get 'AK 'state) 'Alaska)
(setf (get 'AZ 'state) 'Arizona)
(setf (get 'AR 'state) 'Arkansas)

(get 'AK 'state)
(get 'TX 'state)

;;
;; 3.7 Functions on Trees
;;

(setf tree '((a b) ((c)) (d e)))
(tree-equal tree (copy-tree tree))

(defun same-shape-tree (a b)
  "Are two trees the same except for the leaves?"
  (tree-equal a b :test #'true))

(defun true (&rest ignore) t)

(same-shape-tree tree '((1 2) ((3)) (4 5)))
(same-shape-tree tree '((1 2)  (3)  (4 5)))

(subst 'new 'old '(old ((very old))))
(sublis '((old . new)) '(old ((very old))))
(subst 'new 'old 'old)

(defun english->french (words)
  (let ((translations
         '((are . va)
           (book .libre)
           (friend . ami)
           (hello . bonjour)
           (how . comment)
           (my . mon)
           (red . rouge)
           (you . tu))))
    (sublis translations words)))

(english->french '(hello my friend - how are you today?))


;;
;; 3.9 Functions on Sets
;;

(setf r '(a b c d))
(setf s '(c d e))
(intersection r s)

(bit-and #*11110 #*11001)
(logand #b11110 #b11001)

;;
;; 3.10 Destructive functions
;;

(setf x '(a b c))
(setf y '(1 2 3))
(append x y)

(nconc x y)
x
y

;;
;; for Exercise 3.5 see `twenty-questions.lisp`
;;

;;
;; 3.13 Debugging
;;

(step (+ 3 4 (* 5 6 (/ 7 8))))

;;
;; 3.14 Antibugging Tools
;;

(defun sqr (x)
  "Multiply x by itself."
  (check-type x number)
  (* x x))

;;
;; Timing Tools
;;

(defun f (n)
  (dotimes (i n) nil))

(time (f 100000))

;;
;; 3.15 Evaluation
;;

(+ 1 2 3 4)

(funcall #'+ 1 2 3 4)

(apply #'+ '(1 2 3 4))

(apply #'+ 1 2 '(3 4))

(eval '(+ 1 2 3 4))

;;
;; 3.16 Closures
;;

(mapcar #'(lambda (x) (+ x x)) '(1 3 10))

(defun adder (c)
  "Return a function that adds c to its argument."
  #'(lambda (x) (+ x c)))

(mapcar (adder 3) '(1 3 10))

(mapcar (adder 10) '(1 3 10))

(defun bank-account (balance)
  "Open a bank account starting with the given balance."
  #'(lambda (action amount)
      (case action
        (deposit  (setf balance (+ balance amount)))
        (withdraw (setf balance (- balance amount))))))

(setf   my-account (bank-account 500.00))
(setf your-account (bank-account 250.00))
(funcall my-account 'withdraw 75.0)
(funcall your-account 'deposit 250.0)
(funcall your-account 'withdraw 100.0)
(funcall my-account 'withdraw 25.00)

;;
;; 3.17 Special Variables
;;

(defvar *counter* 0)

(defun report-counter ()
  (format t "Counter = ~d" *counter*))

(report-counter)

(let ((*counter* 100))
  (report-counter))

;;
;; Exercise 3.6
;;

(setf a 'global-a)
(defvar *b* 'global-b)

(defun fn () *b*)

(let ((a 'local-a)
      (*b* 'local-b))
  (list a *b* (fn) (symbol-value 'a) (symbol-value '*b*)))


;;
;; 3.18 Multiple Values
;;

(round 5.1)

(defun show-both (x)
  (multiple-value-bind (int rem)
      (round x)
    (format t "~f = ~d + ~f" x int rem)))

(show-both 5.1)

(values 1 2 3)

;;
;; 3.19 More about Parameters
;;

(defun math-quiz (op range n)
  "Ask the user a series of math problems."
  (dotimes (i n)
    (problem (random range) op (random range))))

(defun problem (x op y)
  "Ask a math problem, read a reply, and say if it is correct."
  (format t "~&How much is ~d ~a ~d?" x op y)
  (if (eql (read) (funcall op x y))
      (princ "Correct!")
      (princ "Sorry, that's not right.")))

(math-quiz '+ 5 5)

(defun math-quiz (&optional (op '+) (range 100) (n 10))
  "Ask the user a series of math problems."
  (dotimes (i n)
    (problem (random range) op (random range))))

(defun math-quiz (&key (op '+) (range 100) (n 10))
  "Ask the user a series of math problems."
  (dotimes (i n)
    (problem (random range) op (random range))))

:xyz

;; &optional

'&optional

(defun f (&xyz) (+ &xyz &xyz))
(f 3)

;; (defun f (:xyz) (+ :xyz :xyz))

(defun g (&key x y) (list x y))

(let ((keys '(:x :y :z)))
  (g (second keys) 1 (first keys) 2))

(find 3 '(1 2 3 4 -5 6.0))

(find 6 '(1 2 3 4 -5 6.0))

(find 6 '(1 2 3 4 -5 6.0) :test #'equalp)

(find 4 '(1 2 3 4 -5 6.0) :test #'<)

(find 5 '(1 2 3 4 -5 6.0) :key #'abs)

(setf (symbol-function 'find-all-if) #'remove-if-not)

(setf nums '(1 2 3 2 1))

;; (find-all 1 nums :test #'=)

(defun find-all (item sequence &rest keyword-args
                 &key (test #'eql) test-not &allow-other-keys)
  "Find all those elements of sequence that match item,
   according to the keywords. Doesn't alter sequence."
  (if test-not
      (apply #'remove item sequence
             :test-not (complement test-not) keyword-args)
      (apply #'remove item sequence
            :test (complement test) keyword-args)))

(find-all 1 nums :test #'= :key #'abs)

;;
;; Exercise 3.7
;;
;; It makes it easier to override parameter values.
;;

;;
;; Exercise 3.8
;;

(defun f (&rest keyword-args &key bla &allow-other-keys)
  (princ keyword-args))

(defun remove-this-and-next (item lst)
  "Remove the item from the list and the next element, too."
  (let ((next (second (member item lst))))
    (remove item (remove next lst))))

(remove-this-and-next 2 '(1 2 3 4))

(defun find-all (item sequence &rest keyword-args
                 &key (test #'eql) test-not &allow-other-keys)
  "Find all those elements of sequence that match item,
   according to the keywords. Doesn't alter sequence."
  (if test-not
      (apply #'remove item sequence :test-not (complement test-not)
             (remove-this-and-next :test-not keyword-args))
      (apply #'remove item sequence :test (complement test)
             (remove-this-and-next :test keyword-args))))

;;
;; Exercise 3.9
;;

(defun length-reduce (lst)
  (reduce #'(lambda (counter element) (+ counter 1)) lst))

(length-reduce '(1 2 3 4))

;;
;; Exercise 3.10
;;

(describe 'lcm)
(describe 'nreconc)

;;
;; Exercise 3.11
;;

(defun extend-alist (key value alist)
  (let ((pair (list key value)))
    (cons pair alist)))

;;
;; Exercise 3.12
;;

(defun exercise-3-12 (lst)
  (format t "~@(~{~a~^ ~}~)." lst))

(exercise-3-12 '(quick brown fox))
