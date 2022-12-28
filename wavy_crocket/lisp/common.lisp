(defun assert (v)
  (if v nil
    (do
      (putc ?a)
      (putc ?s)
      (putc ?s)
      (putc ?e)
      (putc ?r)
      (putc ?t)
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

(map putc "hello!")
