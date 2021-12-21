;; renamed lambdas/lets: 8
 
(define result ())
 
(define display2 (<change>
      (lambda (i)
         (set! result (cons i result)))
      (lambda (_i0)
         (set! result (cons _i0 result)))))
 
(define newline2 (lambda ()
      (set! result (cons 'newline result))))
 
(define print-abc (<change>
      (lambda (a b c)
         (display2 a)
         (display " ")
         (display2 b)
         (display " ")
         (display2 c)
         (newline2))
      (lambda (_a0 _b0 _c0)
         (display2 _a0)
         (display " ")
         (display2 _b0)
         (display " ")
         (display2 _c0)
         (newline2))))
 
(define foo (<change>
      (lambda (a b c)
         (print-abc a b c)
         (let ((a 4)
               (c 5)
               (b c))
            (print-abc a b c)
            (let ((b 6)
                  (c a))
               (print-abc a b c))
            (let ((a b)
                  (c a))
               (print-abc a b c)))
         (print-abc a b c))
      (lambda (_a0 _b0 _c0)
         (print-abc _a0 _b0 _c0)
         (let ((_a1 4)
               (_c1 5)
               (_b1 _c0))
            (print-abc _a1 _b1 _c1)
            (let ((_b2 6)
                  (_c2 _a1))
               (print-abc _a1 _b2 _c2))
            (let ((_a2 _b1)
                  (_c3 _a1))
               (print-abc _a2 _b1 _c3)))
         (print-abc _a0 _b0 _c0))))
 
(foo 1 2 3)
 
(equal?
   result
   (__toplevel_cons
      'newline
      (__toplevel_cons
         3
         (__toplevel_cons
            " "
            (__toplevel_cons
               2
               (__toplevel_cons
                  " "
                  (__toplevel_cons
                     1
                     (__toplevel_cons
                        'newline
                        (__toplevel_cons
                           4
                           (__toplevel_cons
                              " "
                              (__toplevel_cons
                                 3
                                 (__toplevel_cons
                                    " "
                                    (__toplevel_cons
                                       3
                                       (__toplevel_cons
                                          'newline
                                          (__toplevel_cons
                                             4
                                             (__toplevel_cons
                                                " "
                                                (__toplevel_cons
                                                   6
                                                   (__toplevel_cons
                                                      " "
                                                      (__toplevel_cons
                                                         4
                                                         (__toplevel_cons
                                                            'newline
                                                            (__toplevel_cons
                                                               5
                                                               (__toplevel_cons
                                                                  " "
                                                                  (__toplevel_cons
                                                                     3
                                                                     (__toplevel_cons
                                                                        " "
                                                                        (__toplevel_cons
                                                                           4
                                                                           (__toplevel_cons
                                                                              'newline
                                                                              (__toplevel_cons
                                                                                 3
                                                                                 (__toplevel_cons " " (__toplevel_cons 2 (__toplevel_cons " " (__toplevel_cons 1 ())))))))))))))))))))))))))))))))
 
(define incr (<change>
      (lambda (x)
         (+ x 1))
      (lambda (_x0)
         (+ _x0 1))))
 
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
 
