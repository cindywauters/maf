; Changes:
; * removed: 0
; * added: 2
; * swaps: 0
; * negated predicates: 0
(letrec ((lp1 (lambda (i x)
                (letrec ((a (= 0 i)))
                   (if a
                      x
                      (letrec ((lp2 (lambda (j f y)
                                      (<change>
                                         ()
                                         (letrec ((b (= 0 j)))
                                            (if b (lp1 (- i 1) y) (lp2 (- j 1) f (f y)))))
                                      (letrec ((b (= 0 j)))
                                         (if b (lp1 (- i 1) y) (lp2 (- j 1) f (f y)))))))
                         (lp2 10 (lambda (n) (<change> () n) (+ n i)) x)))))))
   (lp1 10 0))