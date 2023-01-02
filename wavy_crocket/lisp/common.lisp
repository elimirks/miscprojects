(defun assert (v)
  (if v nil
    (progn
      ;; The println call will noop if it isn't define yet
      ;; ... but that's ok. If it fails before then, something terrible happened
      (println "Assertion failure")
      (exit 1))))

(defun inc (n) (+ n 1))
(defun dec (n) (- n 1))

(defun or (a b)
  (if a 1 (if b 1 nil)))
(defun and (a b)
  (if a (if b 1 nil) nil))

(defun not (v) (if (false? v) ?t nil))

(defun true? (v) (not (false? v)))

(defun lt? (lhs rhs)
  (eq? -1 (cmp lhs rhs)))
(defun le? (lhs rhs)
  (not (gt? lhs rhs)))
(defun gt? (lhs rhs)
  (eq? 1 (cmp lhs rhs)))
(defun ge? (lhs rhs)
  (not (lt? lhs rhs)))

(defun len (l) 
  (if (false? l) 0 (inc (len (cdr l)))))

(defun map (f l)
  (if (false? l) nil
    (cons (f (car l)) (map f (cdr l)))))

(defun filter (predicate l)
  (if (false? l) nil
    (progn
      (set 'h (car l))
      (if (predicate h)
        (cons h (filter predicate (cdr l)))
        (filter predicate (cdr l))))))

(defun foreach (f l)
  (if (false? l) nil
    (progn
      (f (car l))
      (foreach f (cdr l)))))

(defun push (elem l) 
  (if (false? l)
    (cons elem nil)
    (cons (car l)
          (push elem (cdr l)))))

(defun fold (f acc l)
  (if (false? l)
    acc
    (fold f (f acc (car l)) (cdr l))))

(defun reduce (f l)
  (fold f (car l) (cdr l)))

(defun sum (l)
  (fold + (car l) (cdr l)))
(defun product (l)
  (fold * (car l) (cdr l)))

(defun append (l1 l2)
  (fold (lambda (a b) (push b a)) l1 l2))

(defun rev (l) 
  (fold (lambda (acc it) (cons it acc)) nil l))

(defun last (l)
  (if (false? l) nil
    (progn
      (set 'next (last (cdr l)))
      (if (false? next) (car l) next))))

(defun nth (index l)
  (if (false? l) nil
    (if (eq? index 0) (car l)
      (nth (- index 1) (cdr l)))))

(defun delimit (sep l) 
  (fold (lambda (acc it)
          (if (false? acc) it 
            (append acc (append sep it)))
          ) nil l))

(defun range (start end)
  (if (eq? start end) nil
    (cons start (range (+ 1 start) end))))
;; Terminates when either list is empty
(defun zip (l1 l2)
  (if (or (false? l1) (false? l2)) nil
    (cons (cons (car l1) (car l2)) (zip (cdr l1) (cdr l2)))))

(defun enumerate (l)
  (zip (range 0 (len l)) l))

(defun caar (x) (car (car x)))
(defun cadr (x) (car (cdr x)))
(defun cdar (x) (cdr (car x)))
(defun cddr (x) (cdr (cdr x)))

(defun print (s) (foreach putc (str-as-list s)))
(defun println (s)
  (print s)
  (putc ?#n))
