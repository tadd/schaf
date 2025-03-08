(define list2 (lambda (x y)
  (cons x (cons y ()))))

(define list4 (lambda (w x y z)
  (cons w (cons x (list2 y z)))))

(define make (lambda (item d)
  (if (= d 0)
      (list2 0 item)
      ((lambda (item2 d2)
         (list4 1 (make (- item2 1) d2)
                item (make item2 d2)))
       (* item 2) (- d 1)))))

(define cadr (lambda (c)
  (car (cdr c))))

(define caddr (lambda (c)
  (cadr (cdr c))))

(define cadddr (lambda (c)
  (caddr (cdr c))))

(define check (lambda (t)
  (if (= (car t) 0)
      (cadr t)
      (+ (caddr t)
         (- (check (cadr t))
            (check (cadddr t)))))))

(define lshift (lambda (x n)
  (if (<= n 1)
      x
      (* 2 (lshift x (- n 1))))))

(define max (lambda (x y)
  (if (> x y) x y)))

(define c 0)

(define inner (lambda (i iterations d)
  (if (>= i iterations)
      #f
      (begin
        (set! c (+ c (check (make i d)) (check (make (- i) d))))
        (inner (+ i 1) iterations d)))))

(define inner2 (lambda (d min-depth max-depth)
  (if (> d max-depth)
      #f
      (begin
        (set! c 0)
        (define iterations (lshift 1 (+ (- max-depth d) min-depth)))
        (inner 0 iterations d)
        (* 2 iterations)
        "	 trees of depth " d
        "	 check: " c
        (inner2 (+ d 2) min-depth max-depth)))))

(define mainf (lambda (n)
  (begin
    (define min-depth 4)
    (define max-depth (max (+ min-depth 2) n))
    (define stretch-depth (+ max-depth 1))
     "stretch  tree of depth " stretch-depth
     "	 check: " (check (make 0 stretch-depth))
    (define long-lived-tree (make 0 max-depth))
    (inner2 4 min-depth max-depth))))

(mainf 9)
