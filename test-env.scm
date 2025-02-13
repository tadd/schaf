(define proc
  (lambda ()
    (print 1)
    (lambda ()
      (print 2)
      (lambda ()
        (print 3)
        0))))

(print (((proc))))
