(define fizzbuzz
  (lambda (n i)
    (cond
      ((= (+ n 1) i) ())
      (else
        (print (fb i))
        (fizzbuzz n (+ i 1))))))
(define fb
  (lambda (n)
    (cond
      ((= (modulo n 15) 0) "FizzBuzz")
      ((= (modulo n 3) 0) "Fizz")
      ((= (modulo n 5) 0) "Buzz")
      (else n))))
(fizzbuzz 100 1)
