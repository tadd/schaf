(define tarai
  (lambda (x y z)
    (if (<= x y)
        y
        (tarai
         (tarai (- x 1) y z)
         (tarai (- y 1) z x)
         (tarai (- z 1) x y)))))

(tarai 10 9 0)
