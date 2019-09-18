;;
;; The Not Looking after You Don't Leap Problem
;;
(progn
  (load "gps-v2.lisp")
  (load "ops/school-ops.lisp")
  (mapc #'convert-op *school-ops*)
  (use (push (op 'taxi-son-to-school
                 :preconds '(son-at-home have-money)
                 :add-list '(son-at-school)
                 :del-list '(son-at-home have-money))
             *school-ops*))
  (gps '(son-at-home have-money car-works)
       '(son-at-school have-money)))
