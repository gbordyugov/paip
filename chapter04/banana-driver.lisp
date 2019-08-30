;;
;; The main idea of this exercise is that we don't really have to
;; touch the code of the solver. All we do is supplying a new set of
;; operations.
;;
(load "gps-v2.lisp")
(load "ops/banana-ops.lisp")
(load "ops/gps-v2-ops-utils.lisp")

(mapc #'convert-op *banana-ops*)

(use *banana-ops*)

(norvig-debug :gps)

(gps '(at-door on-floor has-ball hungry chair-at-door)
     '(not-hungry))
