;;
;; The chain of calls
;; achieve-all -> achieve -> apply-op
;;

(defun achieve-all (state goals goal-stack)
  (labels ((achieve-state (from-state to-state)
             (when from-state
               (achieve from-state to-state goal-stack))))
    t))

(defun achieve (state goals goal-stack)
  t)

(defun apply-op (state op goal-stack)
  t)

(defun gps (start-state end-state)
  t)
