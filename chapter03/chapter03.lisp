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
