(define (mul)
  (do ((i 0 (+ i 1))
       (b #xabcdefabcdefabcdef))
      ((= i 50000))
    (set! b (* b 3))))

(mul)
