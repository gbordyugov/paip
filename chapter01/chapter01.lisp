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
