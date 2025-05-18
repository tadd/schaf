;;; The Computer Language Benchmarks Game
;;; http://shootout.alioth.debian.org/
;;; contributed by Sven Hartrumpf

(define (make item d)
  (if (= d 0)
      (list 'empty item)
      (let ((item2 (* item 2))
            (d2 (- d 1)))
        (list 'node (make (- item2 1) d2)
              item (make item2 d2)))))

(define (check t)
  (if (eq? (car t) 'empty)
      (cadr t)
      (+ (caddr t)
         (- (check (cadr t))
            (check (cadddr t))))))

(define (<< x n)
  (* x (expt 2 n)))

(define (prints . args) args)

(define (main n)
  (let* ((min-depth 4)
         (max-depth (max (+ min-depth 2) n)))
    (let ((stretch-depth (+ max-depth 1)))
      (prints "stretch  tree of depth " stretch-depth
              "	 check: " (check (make 0 stretch-depth))))
    (let ((long-lived-tree (make 0 max-depth)))
      (do ((d 4 (+ d 2))
           (c 0 0))
          ((> d max-depth))
        (let ((iterations (<< 1 (+ (- max-depth d) min-depth))))
          (do ((i 0 (+ i 1)))
              ((>= i iterations))
            (set! c (+ c (check (make i d)) (check (make (- i) d)))))
          (prints (* 2 iterations)
                  "	 trees of depth " d
                  "	 check: " c)))
      (prints "long lived tree of depth " max-depth
              "	 check: " (check long-lived-tree)))))

(main 10)
