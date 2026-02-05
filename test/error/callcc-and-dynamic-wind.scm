;; unbound variable: not-defined
(define (f)
  (let ((k #f))
    (dynamic-wind
        (lambda () (if k not-defined))
        (lambda () (call/cc (lambda (k0) (set! k k0))))
        (lambda () #f))
    (k 'x)))
(f)
