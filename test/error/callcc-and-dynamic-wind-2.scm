;; unbound variable: not-defined
(define (voidf) #f)
(define (fail) not-defined)
(define k #f)

(define (f)
  (dynamic-wind
      voidf
      (lambda () (call/cc (lambda (k0) (set! k k0))))
      voidf))

(define (g)
  (f)
  (dynamic-wind
      voidf
      (lambda () (k 1))
      fail))
(g)
