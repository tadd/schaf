(define fib (lambda (n)
  (if (< n 2)
      n
      (+ (fib (- n 1)) (fib (- n 2))))))

(do ((i 0 (+ i 1)))
    ((= i 25000))
  (fib 8))
