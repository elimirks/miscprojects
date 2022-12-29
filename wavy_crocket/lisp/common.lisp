(defun assert (v)
  (if v nil
    (do
      ;; The println call will noop if it isn't define yet
      (println "Assertion failure")
      (exit 1))))

;; Some intrinsic tests
(assert (false? '()))
(assert (false? nil))
(assert (false? 'nil))

(defun inc (n) (+ n 1))
(defun dec (n) (- n 1))

(defun not (v) (if (false? v) ?t nil))
(assert (not nil))
(assert (not (false? 42)))

(defun true? (v) (not (false? v)))
(assert (not (true? nil)))
(assert (true? 2))

(defun len (list) 
  (if (false? list) 0 (inc (len (cdr list)))))
(assert (eq? 3 (len '(1 2 3))))
(assert (eq? 0 (len nil)))
(assert (eq? 0 (len '())))
(assert (eq? 0 (len ())))

(defun map (f list)
  (if (false? list)
    '()
    (cons (f (car list)) (map f (cdr list)))))
(assert (eq? '(2 3 4) (map inc '(1 2 3))))
(assert (eq? '(nil nil nil) (map nil '(1 2 3))))

(defun append (list elem) 
  (if (false? list)
    '(elem)
    (cons (car list) (append (cdr list) elem))))
(assert (eq? '(1 2 3) (append '(1 2) 3)))

(defun fold (f acc list)
  (if (false? list)
    acc
    (fold f (f acc (car list)) (cdr list))))
(defun sum (list)
  (fold 0 add list))
(assert (eq? 6 (sum '(1 2 3))))

(defun concat (l1 l2)
  (fold append l2 l1))
(assert (eq? '(1 2 3 4) (concat '(1 2) '(3 4))))
(assert (eq? "foobar" (concat "foo" "bar")))

(defun print (s) (map putc s))
(defun println (s)
  (print s)
  (putc ?#n))

(println "Hello, world!")
