(load "gps-v2.lisp")
(load "ops/banana-ops.lisp")
(load "ops/gps-v2-ops-utils.lisp")

(mapc #'convert-op *banana-ops*)

(use *banana-ops*)

(norvig-debug :gps)

(gps '(at-door on-floor has-ball hungry chair-at-door)
     '(not-hungry))
