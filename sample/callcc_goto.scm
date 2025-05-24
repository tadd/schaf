(define sum-to (lambda (n)
  (call/cc
   (lambda (return)
     ((lambda (i sum)
        ((lambda (loop)
           (begin
             (call/cc (lambda (k) (set! loop k)))
             (if (> i n)
                 (return sum)
                 (begin
                   (set! sum (+ sum i))
                   (set! i (+ i 1))
                   (loop)))))
         #f))
      1 0)))))

(print (sum-to 10))
