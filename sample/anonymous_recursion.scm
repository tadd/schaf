(print
  ((lambda (f a) (f f a))
    (lambda (f a)
      (if (= a 0)
        0
        (+ a (f f (- a 1)))))
    10))
