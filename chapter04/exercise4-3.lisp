(let ((use-old-gps nil))
  (progn
    (if use-old-gps
        (load "gps-v2-exercise-4-3-pre.lisp")
        (load "gps-v2-exercise-4-3-pre.lisp"))

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

    ;;
    ;; The (reduntant) solution which found by GPS v2 is:
    ;;
    ;; ((START) (EXECUTING BUY-CAKE) (EXECUTING EAT-CAKE)
    ;;  (EXECUTING EAT-ICE-CREAM))
    ;;
    ;; The reason for that being that eating ice cream appears first
    ;; on the operator list and GPS tries to satisfy it first. The
    ;; only option to obtain an ice cream is to eat a cake. In order
    ;; to eat a cake, we need to buy it.
    ;;
    ;; Now the question is how to recognize that eating cake satisfies
    ;; the ultimate goal, i.e. DESSERT-EATEN? One idea is to check
    ;; whether we have a solution immediately after applying an
    ;;p operator.
    ;;
    (norvig-debug :gps)

    (gps '(have-money) '(dessert-eaten) *ex-4-3-ops*)

    ;;
    ;; This produces:
    ;;
    ;; Goal: DESSERT-EATEN
    ;; Consider: EAT-ICE-CREAM
    ;;  Goal: HAVE-ICE-CREAM
    ;;  Consider: EAT-CAKE
    ;;   Goal: HAVE-CAKE
    ;;   Consider: BUY-CAKE
    ;;    Goal: HAVE-MONEY
    ;;   Action: BUY-CAKE
    ;;  Action: EAT-CAKE
    ;; Action: EAT-ICE-CREAM
    ))
