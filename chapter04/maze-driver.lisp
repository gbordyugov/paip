;;
;; Again, we don't really have to touch the code of the solver. All we
;; do is supplying a new set of operations.
;;
(load "gps-v2.lisp")
(load "ops/maze-ops.lisp")
(load "ops/gps-v2-ops-utils.lisp")

(mapc #'convert-op *maze-ops*)

(use *maze-ops*)

(norvig-debug :gps)

(gps '((at 1)) '((at 25)))
