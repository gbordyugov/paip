(+ 2 2)

(+ 1 2 3 4 5 6 7 8 9 10)

(- (+ 9000 900 90 9) (+ 5000 500 50 5))

'(Pat Kim)

(append '(pat kim) '(robin sandy))

(append '(pat kim) (list '(john q public) 'sandy))

(length (append '(pat kim) (list '(john q public) 'sandy)))

(defun last-name (name)
  "Select the lasat name from a name represented as a list."
  (first (last name)))

