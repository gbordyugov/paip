(load "gps-v2.lisp")
(load "ops/school-ops.lisp")
(load "ops/gps-v2-ops-utils.lisp")

(mapc #'convert-op *school-ops*)
(use *school-ops*)

(norvig-debug :gps)
(gps '(son-at-home car-needs-battery have-money have-phone-book)
     '(son-at-school))

(norvig-undebug)

;;
;; A trivial case.
;;
(gps '(son-at-home car-works)
     '(son-at-school))


;;
;; Three cases that version 1 of GPS was not able to handle.
;;
(gps '(son-at-home car-needs-battery have-money have-phone-book)
     '(have-money son-at-school))

(gps '(son-at-home car-needs-battery have-money have-phone-book)
     '(son-at-school have-money))

(gps '(son-at-home car-needs-battery have-money)
     '(son-at-school))

;;
;; Another trivial case
;;

(gps '(son-at-home) '(son-at-home))
