(+ 2 2)

(+ 1 2 3 4 5 6 7 8 9 10)

(- (+ 9000 900 90 9) (+ 5000 500 50 5))

'(Pat Kim)

(append '(pat kim) '(robin sandy))

(append '(pat kim) (list '(john q public) 'sandy))

(length (append '(pat kim) (list '(john q public) 'sandy)))

(setf p '(john q public))

p

(setf x 10)

(+ x x)

(+ x (length p))

(first p)

(rest p)

(second p)

(third p)

(fourth p)

(length p)

(setf x '((1st element) 2 (element 3) ((4)) 5))

(length x)

(first x)

(cons 'mr p)

(cons (first p) (rest p))

(setf town (list 'Anytown 'USA))

(list p 'of town 'may 'have 'already 'won!)

(append p '(of) town '(may have already won!))

p

(last p)

(first (last p))

;;
;; Chapter 1.5
;;

(defun last-name (name)
  "Select the last name from a name represented as a list."
  (first (last name)))

(last-name p)

(last-name '(Rear Admiral Grace Murray Hopper))

(last-name '(Rex Morgan MD))

(last-name '(Spot))

(last-name '(Aristotle))

(defun first-name (name)
  "Select the first name from a name represented as a list."
  (first name))

p

(first-name p)

(first-name '(Wilma Flinstone))

(setf names '((John Q Public)
              (Malcolm X)
              (Admiral Grace Murray Hopper)
              (Spot)
              (Aristotle)
              (A A Milne)
              (Z Z Top)
              (Sir Larry Olivier)
              (Miss Scarlet)))

(first-name (first names))

;;
;; Chapter 1.6
;;

(mapcar #'last-name names)

(mapcar #'- '(1 2 3 4))

(mapcar #'+ '(1 2 3 4) '(10 20 30 40))

(mapcar #'first-name names)

(defparameter *titles*
  '(Mr Mrs Miss Ms Sir Madam Dr Admiral Major General MD)
  "A list of titles that can appear at the start of a name.")

(defun first-name (name)
  "Select the first name from a name represnted as a list."
  (if (member (first name) *titles*)
      (first-name (rest name))
      (first name)))

(mapcar #'first-name names)

(first-name '(Madam Major General Paula Jones))

(trace first-name)

(first-name '(Jonn Q Public))

(first-name '(Madam Major General Paula Jones))

(untrace first-name)

(first-name '(Mr Blue Jeans))

;;
;; Chapter 1.7 Higher-Order Functions
;;

(defun mappend (fn the-list)
  "Apply fn to each element of list and append the results."
  (apply #'append (mapcar fn the-list)))

(apply #'+ '(1 2 3 4))

(apply #'append '((1 2 3) (a b c)))

(defun self-and-double (x)
  "Return a tuple consisting of x and its double."
  (list x (+ x x)))

(self-and-double 3)

(apply #'self-and-double '(3))

(mapcar #'self-and-double '(1 10 300))

(mappend #'self-and-double '(1 10 300))

(defun numbers-and-negations (input)
  "Given a list, return only the numbers and their negations."
  (mappend #'number-and-negation input))

(defun number-and-negation (x)
  "If x is a number, return a list of x and -x."
  (if (numberp x)
      (list x (- x))
      nil))

(numbers-and-negations '(testing 1 2 3 test))

(defun mappend (fn the-list)
  "Apply fn to each element of list and append the results."
  (if (null the-list)
      nil
      (append (funcall fn (first the-list))
              (mappend fn (rest  the-list)))))

((lambda (x) (+ x 2)) 4)

(funcall #'(lambda (x) (+ x 2)) 4)

(mapcar #'(lambda (x) (+ x x)) '(1 2 3 4 5))

(mappend #'(lambda (l) (list l (reverse l)))
         '((1 2 3) (a b c)))

;;
;; Chapter 1.8 Other Data Types
;;

"a string"
(length "a string")
(length "")

;;
;; Exercise 1.1
;;
(defun last-name (name)
  "Select the last name from a name represnted as a list."
  (first-name (reverse name)))

(last-name '(Rex Morgan MD))

;;
;; Exercise 1.2
;;

(defun power (e n)
  "Raise e to integer power n"
  (cond
    ((= n 0) 1)
    ((= n 1) e)
    ((evenp n) (let ((y (power e (/ n 2)))) (* y y)))
    (t (* (power e (- n 1)) e))))

(power 2 0)
(power 2 1)
(power 2 3)
(power 3.24 15)

;;
;; Exercise 1.3
;;

(defun count-atoms (expr)
  "Count atoms in expression expr."
  (if (atom expr)
      1
      (let ((counters (mapcar #'count-atoms expr)))
        (apply #'+ counters))))

(count-atoms 3)
(count-atoms '(3 4 5))
(count-atoms '(3 (4 5 (5) ((6)))))

;;
;; Exercise 1.4
;;

(defun count-anywhere (needle stack)
  "Count the number of occurences of needle in stack."
  (cond
    ((equal needle stack) 1)
    ((atom stack) 0)
    (t (let ((counters (mapcar #'(lambda (x) (count-anywhere needle x))
                               stack)))
         (apply #'+ counters)))))

(count-anywhere 'a 'b)
(count-anywhere 'a 'a)
(count-anywhere '(g b) '(g b))
(count-anywhere '(g b) '((g b) d (g b)))


;;
;; Exercise 1.5
;;

(defun dot-product (a b)
  (apply #'+ (mapcar #'* a b)))

(dot-product '(10 20) '(3 4))
