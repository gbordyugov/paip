(load "gps-v2.lisp")
(load "ops/school-ops.lisp")
(load "ops/gps-v2-ops-utils.lisp")

(mapc #'convert-op *school-ops*)
(use *school-ops*)

(norvig-debug :gps)
(gps '(son-at-home car-needs-battery have-money have-phone-book)
     '(son-at-school))
