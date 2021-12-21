;; renamed lambdas/lets: 3
 
(define compare (<change>
      (lambda (lijst1 lijst2)
         (if (let ((__or_res (null? lijst1))) (if __or_res __or_res (null? lijst2)))
            0
            (if (eq? (car lijst1) (car lijst2))
               (+ 1 (compare (cdr lijst1) (cdr lijst2)))
               0)))
      (lambda (_lijst10 _lijst20)
         (if (let ((___or_res0 (null? _lijst10))) (if ___or_res0 ___or_res0 (null? _lijst20)))
            0
            (if (eq? (car _lijst10) (car _lijst20))
               (+ 1 (compare (cdr _lijst10) (cdr _lijst20)))
               0)))))
 
(define compare-iter (<change>
      (lambda (lijst1 lijst2)
         (define loop (lambda (l1 l2 res)
               (if (let ((__or_res (null? l1))) (if __or_res __or_res (null? l2)))
                  res
                  (if (eq? (car l1) (car l2))
                     (loop (cdr l1) (cdr l2) (+ res 1))
                     res))))
         (loop lijst1 lijst2 0))
      (lambda (_lijst10 _lijst20)
         (define loop (lambda (_l10 _l20 _res0)
               (if (let ((___or_res0 (null? _l10))) (if ___or_res0 ___or_res0 (null? _l20)))
                  _res0
                  (if (eq? (car _l10) (car _l20))
                     (loop (cdr _l10) (cdr _l20) (+ _res0 1))
                     _res0))))
         (loop _lijst10 _lijst20 0))))
 
(define algemene-compare (lambda (lijst1 lijst2 test)
      (if (let ((__or_res (null? lijst1))) (if __or_res __or_res (null? lijst2)))
         0
         (if (test (car lijst1) (car lijst2))
            (+ 1 (algemene-compare (cdr lijst1) (cdr lijst2) test))
            0))))
 
(define compare-greater (<change>
      (lambda (lijst1 lijst2)
         (algemene-compare lijst1 lijst2 >))
      (lambda (_lijst10 _lijst20)
         (algemene-compare _lijst10 _lijst20 >))))
 
(if (= (compare (__toplevel_cons 'a (__toplevel_cons 'b (__toplevel_cons 'c (__toplevel_cons 'd (__toplevel_cons 'e (__toplevel_cons 'f (__toplevel_cons 'g ()))))))) (__toplevel_cons 'a (__toplevel_cons 'b (__toplevel_cons 'c (__toplevel_cons 'x (__toplevel_cons 'y ())))))) 3)
   (if (= (compare (__toplevel_cons 'x (__toplevel_cons 'a (__toplevel_cons 'b ()))) (__toplevel_cons 'a (__toplevel_cons 'b (__toplevel_cons 'c (__toplevel_cons 'd (__toplevel_cons 'e (__toplevel_cons 'f (__toplevel_cons 'g ())))))))) 0)
      (if (= (compare (__toplevel_cons 'a (__toplevel_cons 'b (__toplevel_cons 'c (__toplevel_cons 'e (__toplevel_cons 'f (__toplevel_cons 'g ())))))) (__toplevel_cons 'a (__toplevel_cons 'b ()))) 2)
         (if (= (compare-iter (__toplevel_cons 'a (__toplevel_cons 'b (__toplevel_cons 'c (__toplevel_cons 'd (__toplevel_cons 'e (__toplevel_cons 'f (__toplevel_cons 'g ()))))))) (__toplevel_cons 'a (__toplevel_cons 'b (__toplevel_cons 'c (__toplevel_cons 'x (__toplevel_cons 'y ())))))) 3)
            (if (= (compare-iter (__toplevel_cons 'x (__toplevel_cons 'a (__toplevel_cons 'b ()))) (__toplevel_cons 'a (__toplevel_cons 'b (__toplevel_cons 'c (__toplevel_cons 'd (__toplevel_cons 'e (__toplevel_cons 'f (__toplevel_cons 'g ())))))))) 0)
               (if (= (compare-iter (__toplevel_cons 'a (__toplevel_cons 'b (__toplevel_cons 'c (__toplevel_cons 'e (__toplevel_cons 'f (__toplevel_cons 'g ())))))) (__toplevel_cons 'a (__toplevel_cons 'b ()))) 2)
                  (=
                     (compare-greater
                        (__toplevel_cons
                           3
                           (__toplevel_cons
                              5
                              (__toplevel_cons 6 (__toplevel_cons 1 (__toplevel_cons 2 (__toplevel_cons 5 ()))))))
                        (__toplevel_cons
                           2
                           (__toplevel_cons
                              1
                              (__toplevel_cons 0 (__toplevel_cons 8 (__toplevel_cons 5 (__toplevel_cons 5 ())))))))
                     3)
                  #f)
               #f)
            #f)
         #f)
      #f)
   #f)
 
