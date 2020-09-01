;;
;; The ELIZA algorith has the following steps:
;; 1) read an input
;; 2) find a pattern that matches the input
;; 3) transform the input into a reposnse
;; 4) print the response
;;
;; We're going to input s-expressions for the sake of simplicity of
;; parsing

;;
;; An example of pattern matching:
;; Pattern: (i need a X)
;; Response: (what would it mean to you if you got a X ?)
;; Input: (i need a vacation)
;; Transformation: (what would it mean to you if got a vacation ?)
;;
;; The core idea is to match the variable X with "vacation". This
;; pattern matching is a generalisation of the Lisp function equal.

(defun simple-equal (x y)
  "Are x and y equal? (Don't check inside strings)."
  (if (or (atom x) (atom y))
      (eql x y)
      (and (simple-equal (first x) (first y))
           (simple-equal (rest x) (rest y)))))

(defun pat-match (pattern input)
  "Does pattern match input? Any variable can match anything."
  (if (variable-p pattern)
      t
      (if (or (atom pattern) (atom input))
          (eql pattern input)
          (and (pat-match (first pattern) (first input))
               (pat-match (rest pattern) (rest input))))))

;;
;; Exercise 5.1
;;
;; Would it be a good idea to replace the complex "and" form in
;; pat-mathc with the simpler (every #'pat-match pattern input)?
;;
;; My answer is "no" as the solution with every would output T when
;; trying to match pattern and input of different lengths, for example
;; '(a b c) with '(a b)
