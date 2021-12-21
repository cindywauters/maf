;; renamed lambdas/lets: 1
 
(define atom? (<change>
      (lambda (x)
         (not (pair? x)))
      (lambda (_x0)
         (not (pair? _x0)))))
 
(define fringe (lambda (l)
      (if (null? l)
         ()
         (if (atom? l)
            (list l)
            (append (fringe (car l)) (fringe (cdr l)))))))
 
(equal?
   (fringe
      (__toplevel_cons
         (__toplevel_cons 1 ())
         (__toplevel_cons
            (__toplevel_cons (__toplevel_cons (__toplevel_cons (__toplevel_cons 2 ()) ()) ()) ())
            (__toplevel_cons
               (__toplevel_cons
                  3
                  (__toplevel_cons (__toplevel_cons 4 (__toplevel_cons 5 ())) (__toplevel_cons 6 ())))
               (__toplevel_cons
                  (__toplevel_cons (__toplevel_cons 7 ()) (__toplevel_cons 8 (__toplevel_cons 9 ())))
                  ())))))
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
                  (__toplevel_cons 6 (__toplevel_cons 7 (__toplevel_cons 8 (__toplevel_cons 9 ()))))))))))
 
