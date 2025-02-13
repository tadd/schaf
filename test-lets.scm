(define proc
  (lambda ()
    (lambda ()
      (lambda ()
        0))))

(apply (proc) '())
