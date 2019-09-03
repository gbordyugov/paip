;;
;; The code below assumes the existence of two special blocks - TABLE
;; and SPACE.
;;
(defun make-block-ops (blocks)
  "Consider all possible combinations of three blocks A, B, and C and
   make operators for moving A from B to C."
  (let ((ops nil))
    (dolist (a blocks)
      (dolist (b blocks)
        (unless (equal a b)
          (dolist (c blocks)
            (unless (or (equal c a) (equal c b))
              (push (mov-op a b c) ops)))
          (push (move-op a 'table b) ops)
          (push (move-op a b 'table) ops))))
    ops))

(defun move-op (a b c)
  "Make an opearator to move A from B to C."
  (op `(move ,a from ,b to ,c)
      :preconds `((space on ,a) (space on ,c) (,a on , b))
      :add-list (move-ons a b c)
      :del-list (move-ons a c b)))

(defun move-ons (a b c)
  "Return ADD-LIST for moving A from B to C. Additionally takes care
   (adds / deletes) SPACE on B. Also used to generate DEL-LIST when
   moving A from C to B."
  (if (eq b 'table)
      `((,a on ,c))
      `((,a on ,c) (space on ,b))))
