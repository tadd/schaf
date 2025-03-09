(define count
  ((lambda (i) (lambda () (begin
    (call/cc (lambda (cont) (set! count cont)))
    (print i)
    (set! i (+ i 1))))) 0))

(count)
(count)
(count)
(count)
(count)
(count)
