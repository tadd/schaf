(define (f n)
  ((lambda () 42))
  (f (+ 1 n)))
(f 0)
