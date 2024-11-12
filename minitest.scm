(define (f)
  (lambda () '(42)))

(do ((i 0 (+ 1 i)))
    ((= i 20) 'foo)
  (f))
