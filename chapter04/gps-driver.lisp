(load "gps.lisp")

(gps '(son-at-home car-needs-battery have-money have-phone-book)
     '(son-at-school)
     *school-ops*)

(gps '(son-at-home car-needs-battery have-money)
     '(son-at-school)
     *school-ops*)

(gps '(son-at-home car-works)
     '(son-at-school)
     *school-ops*)

;;
;; 4.7 The Clobbering Sibling Goal Problem
;;

;;
;; This case works as expected by driving son to school.
;;
(gps '(son-at-home have-money car-works)
     '(have-money son-at-school)
     *school-ops*)

;;
;; But in this case, GPS incorrectly reports success despite it having
;; spent money on the battery.
;;
(gps '(son-at-home car-needs-battery have-money have-phone-book)
     '(have-money son-at-school)
     *school-ops*)
;;
;; ... the bug being that the expression (every #'achieve goals) tries
;; to achieve goals in sequence, which means that achieving achieving
;; a subsequent goal can undo a previously achieved goal. This means
;; that achieving (have-money son-at-school) means "first achieve
;; have-money and then achieve son-at-school".
;;
