;; renamed lambdas/lets: 2
 
(define atom? (<change>
      (lambda (x)
         (if (not (pair? x)) (not (null? x)) #f))
      (lambda (_x0)
         (if (not (pair? _x0)) (not (null? _x0)) #f))))
 
(define lat? (<change>
      (lambda (l)
         (if (null? l)
            #t
            (if (atom? (car l)) (lat? (cdr l)) #f)))
      (lambda (_l0)
         (if (null? _l0)
            #t
            (if (atom? (car _l0)) (lat? (cdr _l0)) #f)))))
 
(lat?
   (__toplevel_cons
      'Jack
      (__toplevel_cons
         'Sprat
         (__toplevel_cons
            'could
            (__toplevel_cons 'eat (__toplevel_cons 'no (__toplevel_cons 'chicken (__toplevel_cons 'fat ()))))))))
 
(lat?
   (__toplevel_cons
      (__toplevel_cons 'Jack ())
      (__toplevel_cons
         'Sprat
         (__toplevel_cons
            'could
            (__toplevel_cons 'eat (__toplevel_cons 'no (__toplevel_cons 'chicken (__toplevel_cons 'fat ()))))))))
 
(lat?
   (__toplevel_cons
      'Jack
      (__toplevel_cons
         (__toplevel_cons 'Sprat (__toplevel_cons 'could ()))
         (__toplevel_cons 'eat (__toplevel_cons 'no (__toplevel_cons 'chicken (__toplevel_cons 'fat ())))))))
 
(lat? ())
 
