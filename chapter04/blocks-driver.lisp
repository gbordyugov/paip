(load "gps-v2-maze.lisp")
(load "ops/block-ops.lisp")

(use (make-block-ops '(a b)))

(gps '((a on table) (b on table) (space on a) (space on b)
       (space on table))
     '((a on b) (b on table)))

(norvig-debug :gps)

(gps '((a on b) (b on table) (space on a) (space on table))
     '((b on a)))

(norvig-undebug :gps)

(progn
  (load "gps-v2-maze.lisp")
  (load "ops/block-ops.lisp")
  (use (make-block-ops '(a b c)))
  ;; This combination of goals produces a solution.
  (gps '((a on b) (b on c) (c on table) (space on a) (space on table))
       '((b on a) (c on b))))

(progn
  (load "gps-v2-maze.lisp")
  (load "ops/block-ops.lisp")
  (use (make-block-ops '(a b c)))
  ;; Whereas a permutation of the goal state make the solver get
  ;; stuck. The difference is the order of the single goals in the end
  ;; state.
  (gps '((a on b) (b on c) (c on table) (space on a) (space on table))
       '((c on b) (b on a))))

(progn
  (load "gps-v2-blocks.lisp")
  (load "ops/block-ops.lisp")
  (use (make-block-ops '(a b c)))
  ;; Whereas a permutation of the goal state make the solver get
  ;; stuck. The difference is the order of the single goals in the end
  ;; state.
  (gps '((a on b) (b on c) (c on table) (space on a) (space on table))
       '((c on b) (b on a))))

;;
;; The suboptimality of solutions.
;;
(progn
  (load "gps-v2-blocks.lisp")
  (load "ops/block-ops.lisp")
  (use (make-block-ops '(a b c)))
  ;; When solving this problem, the GPS makes an unnecessary step of
  ;; putting C from A to B, whereas it could be put from A directly to
  ;; table.
  (gps '((c on a) (a on table) (b on table) (space on c) (space on b)
         (space on table))
       '((c on table))))

(progn
  (load "gps-v2-blocks.lisp")
  (load "ops/block-ops.lisp")
  (use (make-block-ops '(a b c)))
  ;; GPS solves this problme in four steps, whereas just two stpes
  ;; are enough.
  (gps '((c on a) (a on table) (b on table) (space on c) (space on b)
         (space on table))
       '((c on table) (a on b))))

(progn
  (load "gps-v2-blocks-sorted-operators.lisp")
  (load "ops/block-ops.lisp")
  (use (make-block-ops '(a b c)))
  ;; With sorted operators, it finds the optimal solution.
  (gps '((c on a) (a on table) (b on table) (space on c) (space on b)
         (space on table))
       '((c on table) (a on b))))

;;
;; The Sussman Anomaly
;;

(progn
  (load "gps-v2-blocks-sorted-operators.lisp")
  (load "ops/block-ops.lisp")
  (use (make-block-ops '(a b c)))
  (setf start '((c on a) (a on table) (b on table) (space on c)
                (space on b) (space on table)))
  (gps start '((a on b) (b on c))))

(progn
  (load "gps-v2-blocks-sorted-operators.lisp")
  (load "ops/block-ops.lisp")
  (use (make-block-ops '(a b c)))
  (setf start '((c on a) (a on table) (b on table) (space on c)
                (space on b) (space on table)))
  (gps start '((b on c) (a on b))))
