;;
;; Chapter 21. A Grammar for a Subset of English
;;

(defun sentence ()
  (append (noun-phrase) (verb-phrase)))

(defun noun-phrase ()
  (append (Article) (Noun)))

(defun verb-phrase ()
  (append (Verb) (noun-phrase)))

(defun Article ()
  (one-of '(the a)))

(defun Noun ()
  (one-of '(man ball woman table)))

(defun Verb ()
  (one-of '(hit took saw liked)))

(defun one-of (set)
  "Pick one element of set, a make a list of it."
  (list (random-elt set)))

(defun random-elt (choices)
  (elt choices (random (length choices))))

(sentence)

;;
;; Another approach
;;

(defun Adj* ()
  (if (= (random 2) 0)
      nil
      (append (Adj) (Adj*))))

(defun PP* ()
  (if (random-elt '(t nil))
      (append (PP) (PP*))))

(defun noun-phrase ()
  (append (Article) (Adj*) (Noun) (PP*)))

(defun PP ()
  (append (Prep) (noun-phrase)))

(defun Adj ()
  (one-of '(big little blue green adiabatic)))

(defun Prep ()
  (one-of '(to in by with on)))

(sentence)

;;
;; 2.3 A Rule-Based Solution
;;

(defparameter *simple-grammar*
  '((sentence -> (noun-phrase verb-phrase))
    (noun-phrase -> (Article Noun))
    (verb-phrase -> (Verb noun-phrase))
    (Article -> the a)
    (Noun -> man ball woman table)
    (Verb -> hit took saw liked))
  "A grammar for a trivial subset of English.")

(defvar *grammar* *simple-grammar*
  "The grammar used by generate. Initially, this is *simple-grammar*,
   but we can switch to other grammars.")

;; (assoc 'noun *grammar*)

(defun rule-lhs (rule)
  "The left-hand side of a rule."
  (first rule))

(defun rule-rhs (rule)
  "The right-hand side of a rule."
  (rest (rest rule)))

(defun rewrites (category)
  "Return a list of the possible rewrites for this category."
  (rule-rhs (assoc category *grammar*)))

(defun mappend (fn the-list)
  "Apply fn to each element of list and append the results."
  (apply #'append (mapcar fn the-list)))

(defun generate (phrase)
  (cond ((listp phrase)
         (mappend #'generate phrase))
        ((rewrites phrase)
         (generate (random-elt (rewrites phrase))))
        (t (list phrase))))


(generate 'sentence)
(generate 'noun-phrase)
(generate 'verb-phrase)


;;
;; Exercise 2.1
;;

(defun generate (phrase)
  (cond ((listp phrase)
         (mappend #'generate phrase))
        (t (let ((r (rewrites phrase)))
             (if r
                 (generate (random-elt r))
                 (list phrase))))))

(generate 'sentence)
