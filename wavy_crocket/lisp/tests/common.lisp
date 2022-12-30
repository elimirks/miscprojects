(require "lisp/common")

(println "Running intrinsic tests")
(assert (false? '()))
(assert (false? nil))
(assert (false? 'nil))
(assert (not (eq? 3 4)))
(assert (eq? 4 (car '(4 . 3))))
(assert (eq? 3 (cdr '(4 . 3))))
(assert (eq? nil (car nil)))
(assert (eq? nil (cdr nil)))

(println "Running `not` tests")
(assert (not nil))
(assert (not (false? 42)))

(println "Running `true?` tests")
(assert (not (true? nil)))
(assert (true? 2))

(println "Running `len` tests")
(assert (eq? 3 (len '(1 2 3))))
(assert (eq? 0 (len nil)))
(assert (eq? 0 (len '())))
(assert (eq? 0 (len ())))

(println "Running `map` tests")
(assert (eq? '(2 3 4) (map inc '(1 2 3))))
(assert (eq? '(-1 1 399) (map dec '(0 2 400))))
(assert (eq? '(nil nil nil) (map nil '(1 2 3))))

(println "Running `push` tests")
(assert (eq? '(1 2 3) (push 3 '(1 2))))

(println "Running `fold` tests")
(assert (eq? 6 (sum '(1 2 3))))
(assert (eq? 30 (product '(5 2 3))))
(assert (eq? nil (sum nil)))
(assert (eq? nil (product nil)))

(println "Running `rev` tests")
(assert (eq? '(3 2 1) (rev '(1 2 3))))
(assert (eq? nil (rev nil)))

(println "Running `last` tests")
(assert (eq? 3 (last '(1 2 3))))
(assert (eq? nil (last nil)))

(println "Running `delimit` tests")
(assert (eq? '(1 2 0 3 0 4) (delimit '(0) '((1 2) (3) (4)))))
(assert (eq? '(1 2 0 3 0 4 5) (delimit '(0) '((1 2) (3) (4 5)))))

(println "Running `append` tests")
(assert (eq? '(1 2 3 4) (append '(1 2) '(3 4))))

(println "Running `car` & `cdr` variantt tests")
(set 'tree '((1 . 2) . (3 . 4)))
(assert (eq? 1 (caar tree)))
(assert (eq? 3 (cadr tree)))
(assert (eq? 2 (cdar tree)))
(assert (eq? 4 (cddr tree)))

(println "All tests passed successfully!")
