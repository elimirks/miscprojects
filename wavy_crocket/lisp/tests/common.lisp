(require "lisp/common")

(println "Running intrinsic tests")
(assert (false? '()))
(assert (false? nil))
(assert (false? 'nil))
(assert (not (eq? 3 4)))
(assert (eq? 4 (inc 3)))

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
(assert (eq? '(nil nil nil) (map nil '(1 2 3))))

(println "Running `append` tests")
(assert (eq? '(1 2 3) (append '(1 2) 3)))

(println "Running `sum` and `fold` tests")
(assert (eq? 6 (sum '(1 2 3))))

(println "Running `concat` tests")
(assert (eq? '(1 2 3 4) (concat '(1 2) '(3 4))))
(assert (eq? "foobar" (concat "foo" "bar")))

(println "All tests passed successfully!")
