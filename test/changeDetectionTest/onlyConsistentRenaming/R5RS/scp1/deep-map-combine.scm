;; renamed lambdas/lets: 2
 
(define atom? (lambda (x)
      (not (pair? x))))
 
(define deep-combine (<change>
      (lambda (combiner null-value l)
         (if (null? l)
            null-value
            (if (atom? l)
               l
               (combiner (deep-combine combiner null-value (car l)) (deep-combine combiner null-value (cdr l))))))
      (lambda (_combiner0 _null-value0 _l0)
         (if (null? _l0)
            _null-value0
            (if (atom? _l0)
               _l0
               (_combiner0
                  (deep-combine _combiner0 _null-value0 (car _l0))
                  (deep-combine _combiner0 _null-value0 (cdr _l0))))))))
 
(define deep-map (lambda (f l)
      (if (null? l)
         ()
         (if (atom? l)
            (f l)
            (cons (deep-map f (car l)) (deep-map f (cdr l)))))))
 
(if (= (deep-combine + 0 (__toplevel_cons (__toplevel_cons (__toplevel_cons (__toplevel_cons 1 (__toplevel_cons 2 ())) (__toplevel_cons (__toplevel_cons 3 (__toplevel_cons 4 ())) ())) (__toplevel_cons (__toplevel_cons (__toplevel_cons 5 (__toplevel_cons 6 ())) (__toplevel_cons (__toplevel_cons 7 (__toplevel_cons 8 ())) ())) ())) (__toplevel_cons 9 ()))) 45)
   (equal?
      (deep-map
         (<change>
            (lambda (x)
               (* x x))
            (lambda (_x0)
               (* _x0 _x0)))
         (__toplevel_cons
            (__toplevel_cons
               (__toplevel_cons
                  (__toplevel_cons 1 2)
                  (__toplevel_cons (__toplevel_cons 3 (__toplevel_cons 4 ())) ()))
               (__toplevel_cons
                  (__toplevel_cons
                     (__toplevel_cons 5 (__toplevel_cons 6 ()))
                     (__toplevel_cons (__toplevel_cons 7 (__toplevel_cons 8 ())) ()))
                  ()))
            9))
      (__toplevel_cons
         (__toplevel_cons
            (__toplevel_cons
               (__toplevel_cons 1 4)
               (__toplevel_cons (__toplevel_cons 9 (__toplevel_cons 16 ())) ()))
            (__toplevel_cons
               (__toplevel_cons
                  (__toplevel_cons 25 (__toplevel_cons 36 ()))
                  (__toplevel_cons (__toplevel_cons 49 (__toplevel_cons 64 ())) ()))
               ()))
         81))
   #f)
 
