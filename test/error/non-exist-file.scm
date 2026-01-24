;; cannot open input file: /dev/null/not/exist
(call-with-input-file "/dev/null/not/exist" (lambda () #f))
