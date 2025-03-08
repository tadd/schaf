(define fib (lambda (n)
  (if (< n 2)
      n
      (+ (fib (- n 1)) (fib (- n 2))))))

(define call1 (lambda (f) (f)))
(define call10x (lambda (f0 f)
  (begin
    (f0 f) (f0 f) (f0 f) (f0 f) (f0 f)
    (f0 f) (f0 f) (f0 f) (f0 f) (f0 f))))
(define call10 (lambda (f) (call10x call1 f)))
(define call100 (lambda (f) (call10x call10 f)))
(define call1000 (lambda (f) (call10x call100 f)))
(define call10000 (lambda (f) (call10x call1000 f)))

(define fib8 (lambda () (fib 8)))
(call10000 fib8)
