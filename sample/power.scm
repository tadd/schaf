(define power (lambda (m n)
  (call/cc (lambda (return) ((lambda (result exponent loop)
      (begin
        (call/cc (lambda (k) (set! loop k)))
        (if (= 0 exponent) (return result))
        (set! result (* m result))
        (set! exponent (- exponent 1))
        (loop))) 1 n #f)))))


(print (power 2 3))
