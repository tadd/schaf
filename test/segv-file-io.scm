(define (f) #f)
;; FIXME: make this runnable with error handling implementation
(define (expect-no-segv)
  (call-with-input-file "/dev/null/not/exist" f)
  (call-with-output-file "/dev/null/not/exist" f)
  (with-input-from-file "/dev/null/not/exist" f)
  (with-output-to-file "/dev/null/not/exist" f))
