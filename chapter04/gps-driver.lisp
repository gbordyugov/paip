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
