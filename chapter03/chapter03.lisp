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
