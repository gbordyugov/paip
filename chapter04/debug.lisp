(defvar *dbg-ids* nil "Indentifiers used by dbg")

(defun norvig-dbg (id format-string &rest args)
  "Print debugging info if (DEBUG ID) has been specified."
  (when (member id *dbg-ids*)
    (fresh-line *debug-io*)
    (apply #'format *debug-io* format-string args)))

(defun norvig-debug (&rest ids)
  "Start dbg output on the given ids."
  (setf *dbg-ids* (union ids *dbg-ids*)))

(defun norvig-undebug (&rest ids)
  "Stop dbg on the ids. With no ids, stop dbg altogether."
  (setf *dbg-ids* (if (null ids)
                      nil
                      (set-difference *dbg-ids* ids))))

(defun norvig-dbg-indent (id indent format-string &rest args)
  "Print indented debugging info if (NORVIG-DEBUG ID) has been
   specified."
  (when (member id *dbg-ids*)
    (fresh-line *debug-io*)
    (dotimes (i indent) (princ " " *debug-io*))
    (apply #'format *debug-io* format-string args)))
