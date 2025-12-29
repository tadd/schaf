;; string-set!: cannot modify immutable object
(let ((imm (symbol->string 'Martin)))
  (string-set! imm 0 "A"))
