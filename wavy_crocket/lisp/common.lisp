(defun assert (v)
  (if v nil
    (do
      ;; The println call will noop if it isn't define yet
      ;; ... but that's ok. If it fails before then, something terrible happened
      (println "Assertion failure")
      (exit 1))))

(defun inc (n) (+ n 1))
(defun dec (n) (- n 1))

(defun not (v) (if (false? v) ?t nil))

(defun true? (v) (not (false? v)))

(defun len (list) 
  (if (false? list) 0 (inc (len (cdr list)))))

(defun map (f list)
  (if (false? list) '()
    (cons (f (car list)) (map f (cdr list)))))

(defun foreach (f list)
  (if (false? list) nil
    (do
      (f (car list))
      (foreach f (cdr list)))))

(defun append (list elem) 
  (if (false? list)
    (cons elem nil)
    (cons (car list)
          (append (cdr list) elem))))

(defun fold (f acc list)
  (if (false? list)
    acc
    (fold f (f acc (car list)) (cdr list))))
(defun sum (list)
  (fold + (car list) (cdr list)))
(defun product (list)
  (fold * (car list) (cdr list)))

(defun concat (l1 l2)
  (fold append l1 l2))

(defun caar (x) (car (car x)))
(defun cadr (x) (car (cdr x)))
(defun cdar (x) (cdr (car x)))
(defun cddr (x) (cdr (cdr x)))

(defun print (s) (foreach putc s))
(defun println (s)
  (print s)
  (putc ?#n))
