;; renamed lambdas/lets: 0
 
(define ontdubbel! (lambda (lijst)
      (let ((deEven ())
            (deOneven ()))
         (define ontdubbel-iter (lambda (prevE prevO restLijst)
               (if (null? restLijst)
                  (begin
                     (set-cdr! prevE ())
                     (set-cdr! prevO ())
                     (cons deEven deOneven))
                  (if (even? (car restLijst))
                     (begin
                        (if (null? prevE)
                           (set! deEven restLijst)
                           (set-cdr! prevE restLijst))
                        (ontdubbel-iter restLijst prevO (cdr restLijst)))
                     (begin
                        (if (null? prevO)
                           (set! deOneven restLijst)
                           (set-cdr! prevO restLijst))
                        (ontdubbel-iter prevE restLijst (cdr restLijst)))))))
         (ontdubbel-iter deEven deOneven lijst))))
 
(equal?
   (ontdubbel!
      (__toplevel_cons
         1
         (__toplevel_cons
            2
            (__toplevel_cons
               3
               (__toplevel_cons
                  4
                  (__toplevel_cons
                     5
                     (__toplevel_cons
                        6
                        (__toplevel_cons 7 (__toplevel_cons 8 (__toplevel_cons 9 (__toplevel_cons 10 ())))))))))))
   (__toplevel_cons
      (__toplevel_cons
         2
         (__toplevel_cons 4 (__toplevel_cons 6 (__toplevel_cons 8 (__toplevel_cons 10 ())))))
      (__toplevel_cons
         1
         (__toplevel_cons 3 (__toplevel_cons 5 (__toplevel_cons 7 (__toplevel_cons 9 ())))))))
 
