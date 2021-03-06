(defun find-all (item sequence &rest keyword-args &key (test #'eql)
                                      test-not &allow-other-keys)
  "Find all those elements of sequence that match item,
   according to the keywords. Doesn't alter sequence."
  (if test-not
      (apply #'remove item sequence
             :test-not (complement test-not) keyword-args)
      (apply #'remove item sequence
             :test (complement test) keyword-args)))

(defun member-equal (item list)
  "The need for this function: our conditions can be lists, such as,
   for example (executing run-around-block), and comparing lists calls
   for a little bit more care."
  (member item list :test #'equal))

(defun appropriate-p (goal op)
  "An op is appropriate to a goal if it is in its add-list."
  (member-equal goal (op-add-list op)))

(defun mappend (fn the-list)
  "Apply fn to each element of list and append the results."
  (apply #'append (mapcar fn the-list)))

(defun action-p (x)
  "Is ix something that is (START) or (EXECUTING ...)?"
  (or (equal x '(start)) (executing-p x)))

(setf (symbol-function 'find-all-if) #'remove-if-not)

(defun executing-p (x)
  "Is x of the form: (executing ...)?"
  (starts-with x 'executing))

(defun starts-with (list x)
  "Is this a list whose first element ix x?"
  (and (consp list) (eql (first list) x)))
