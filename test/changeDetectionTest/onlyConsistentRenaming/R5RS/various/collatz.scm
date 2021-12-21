;; renamed lambdas/lets: 2
 
(define div2* (lambda (n s)
      (if (= (* 2 n) s)
         n
         (if (= (+ (* 2 n) 1) s) n (div2* (- n 1) s)))))
 
(define div2 (<change>
      (lambda (n)
         (div2* n n))
      (lambda (_n0)
         (div2* _n0 _n0))))
 
(define hailstone* (<change>
      (lambda (n count)
         (if (= n 1)
            count
            (if (even? n)
               (hailstone* (div2 n) (+ count 1))
               (hailstone* (+ (* 3 n) 1) (+ count 1)))))
      (lambda (_n0 _count0)
         (if (= _n0 1)
            _count0
            (if (even? _n0)
               (hailstone* (div2 _n0) (+ _count0 1))
               (hailstone* (+ (* 3 _n0) 1) (+ _count0 1)))))))
 
(define hailstone (lambda (n)
      (hailstone* n 0)))
 
(hailstone 5)
 
