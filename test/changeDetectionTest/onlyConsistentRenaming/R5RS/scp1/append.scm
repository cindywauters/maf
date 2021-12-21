;; renamed lambdas/lets: 0
 
(if (equal? (append (__toplevel_cons 1 (__toplevel_cons 2 (__toplevel_cons 3 ()))) (__toplevel_cons 4 (__toplevel_cons 5 ()))) (__toplevel_cons 1 (__toplevel_cons 2 (__toplevel_cons 3 (__toplevel_cons 4 (__toplevel_cons 5 ()))))))
   (if (equal? (append (__toplevel_cons 1 (__toplevel_cons 2 (__toplevel_cons 3 ()))) ()) (__toplevel_cons 1 (__toplevel_cons 2 (__toplevel_cons 3 ()))))
      (if (equal? (append () (__toplevel_cons 1 (__toplevel_cons 2 (__toplevel_cons 3 ())))) (__toplevel_cons 1 (__toplevel_cons 2 (__toplevel_cons 3 ()))))
         (null? (append () ()))
         #f)
      #f)
   #f)
 
