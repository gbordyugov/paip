;;
;; Again, we don't really have to touch the code of the solver. All we
;; do is supplying a new set of operations.
;;
(load "gps-v2-maze.lisp")
(load "ops/maze-ops.lisp")
(load "ops/gps-v2-ops-utils.lisp")

(mapc #'convert-op *maze-ops*)

(use *maze-ops*)

(norvig-undebug :gps)

(gps '((at 1)) '((at 25)))

(find-path 1 25)

(find-path 1 1)

(equal (find-path 1 25) (reverse (find-path 25 1)))
