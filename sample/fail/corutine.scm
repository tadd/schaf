(define count
  ((lambda (i) (lambda () (begin
    (call/cc (lambda (cont) (set! count cont)))
    (print "count")
    (print i)
    (set! i (+ i 1))
    (count2)))) 0))

(define count2
  ((lambda (i) (lambda () (begin
    (call/cc (lambda (cont) (set! count2 cont)))
    (if (> i 10) (set! count2 (lambda () ())))
    (print "count2")
    (print i)
    (set! i (+ i 1))
    (count)))) 0))

(count)
