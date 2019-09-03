(load "gps-v2-maze.lisp")
(load "ops/block-ops.lisp")

(use (make-block-ops '(a b)))

(gps '((a on table) (b on table) (space on a) (space on b)
       (space on table))
     '((a on b) (b on table)))
