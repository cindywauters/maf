;; renamed lambdas/lets: 4
 
(define incr (lambda (x)
      (+ x 1)))
 
(define sum (<change>
      (lambda (term a next b)
         (if (> a b)
            0
            (+ (term a) (sum term (next a) next b))))
      (lambda (_term0 _a0 _next0 _b0)
         (if (> _a0 _b0)
            0
            (+ (_term0 _a0) (sum _term0 (_next0 _a0) _next0 _b0))))))
 
(define simp-int (<change>
      (lambda (f a b n)
         (let ((h (/ (- b a) n)))
            (define y (lambda (k)
                  (f (+ a (* h k)))))
            (define term (lambda (k)
                  (* (if (let ((__or_res (= k 0))) (if __or_res __or_res (= k n))) 1 (+ 2 (* 2 (modulo k 2)))) (y k))))
            (/ (* h (sum term 0 incr n)) 3)))
      (lambda (_f0 _a0 _b0 _n0)
         (let ((_h0 (/ (- _b0 _a0) _n0)))
            (define y (lambda (_k0)
                  (_f0 (+ _a0 (* _h0 _k0)))))
            (define term (lambda (_k1)
                  (*
                     (if (let ((___or_res0 (= _k1 0))) (if ___or_res0 ___or_res0 (= _k1 _n0)))
                        1
                        (+ 2 (* 2 (modulo _k1 2))))
                     (y _k1))))
            (/ (* _h0 (sum term 0 incr _n0)) 3)))))
 
(define r (sqrt 2))
 
(if (= (simp-int (<change> (lambda (x) x) (lambda (_x0) _x0)) 0 10 100) 50)
   (=
      (simp-int
         (<change>
            (lambda (x)
               (sqrt (- (* r r) (* x x))))
            (lambda (_x0)
               (sqrt (- (* r r) (* _x0 _x0)))))
         (- r)
         r
         100)
      3.140293e+00)
   #f)
 
