(progn
  (load "gps-v2-maze.lisp")
  (load "utils.lisp")
  (load "ops/op.lisp")

  (defparameter *ex-4-3-ops*
    (list
     (op 'buy-cake
         :preconds '(have-money)
         :add-list '(have-cake)
         :del-list '(have-money))
     (op 'eat-ice-cream
         :preconds '(have-ice-cream)
         :add-list '(ice-cream-eaten dessert-eaten)
         :del-list '(have-ice-cream))
     (op 'eat-cake
         :preconds '(have-cake)
         :add-list '(cake-eaten have-ice-cream dessert-eaten)
         :del-list '(have-cake))))

  (mapc #'convert-op *ex-4-3-ops*)

  (gps '(have-money)
       '(dessert-eaten)
       *ex-4-3-ops*))
