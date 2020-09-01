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

;;
;; There is a tradition in Lisp-based AI programs to have variables by
;; symbols that start with the question mark character.

(symbol-name 'abc)

(defun variable-p (x)
  "Is ix a variable, i.e., a symbol beginning with '?' ?"
  (and (symbolp x)
       (equal (char (symbol-name x) 0)
              #\?)))

;; should evaluate to T
(pat-match '(I need a ?X) '(I need a vacation))

;; should evaluate to NIL
(pat-match '(I need a ?X) '(I really need a vacation))

(sublis '((?X . vacation))
        '(what would it mean to you if you got a ?X ?))

(defun pat-match-first-attempt (pattern input)
  "Does pattern match input? WARNING: buggy version."
  (if (variable-p pattern)
      (list (cons pattern input))
      (if (or (atom pattern) (atom input))
          (eql pattern input)
          (append (pat-match (first pattern) (first input))
                  (pat-match (rest pattern) (rest input))))))

;;
;; The above implementation of pat-match has the following problems:
;;
;; 1) The check (eql pattern input) may return T, which is not a list,
;;    so a consequitive append will complain.
;;
;; 2) The same tist might return NIL, which would indicate failure,
;;    but it will just be treated as a list, and will be appended to
;;    the rest of answer.
;;
;; 3) We cannot distinguish between the case where the match fails -
;;    and returns NIL -- versus the case where everything matches, but
;;    there are no variable, so it returns the null a-list (this is
;;    the semipredicate problem already discussed in Chapter 4).
;;
;; 4) We want the bindings of variables to agree -- if ?X is used
;;    twice in the patter, we don't want it to match two different
;;    values in the input.
;;
;; 5) It is inefficient for pat-match to check both the first and the
;;    rest of lists, even when the corresponding first parts fail to
;;    match.
;;
;; A total of five bugs in a seven-line function.
;;
;; We can resolve thses problems by agreeing on two major conventions.
;;
;; 1) We're making pat-match a true predicate, so we will agree that
;;    it returns NIL only to indicate failure.
;;
;; 2) If we are going to be consistent about the values of variables,
;;    then the first will have to know what the rest is doing. We can
;;    accomplish this by passing the binding list as a third argument
;;    to pat-match. We make it an optional argument, because we want
;;    to be able to say simply (pat-match a b).

(defconstant fail nil "Indicates pat-match failure")

(defconstant no-bindings '((t . t))
  "Indicates pat-match success, with no variables.")

;;
;; A slim layer of additional abstractions.
;;

(defun get-binding (var bindings)
  "Find a (variable . value) pair in a binding list."
  (assoc var bindings))

(defun binding-val (binding)
  (cdr binding))

(defun lookup (var bindings)
  "Get the value part (for var) from a binding list."
  (binding-val (get-binding var bindings)))

(defun extend-bindings (var val bindings)
  "Add a (var . value) pair to a binding list."
  (cons (cons var val) bindings))
