;; renamed lambdas/lets: 3
 
(define atom? (<change>
      (lambda (x)
         (not (pair? x)))
      (lambda (_x0)
         (not (pair? _x0)))))
 
(define same-structure? (lambda (l1 l2)
      (if (if (atom? l1) (atom? l2) #f)
         #t
         (if (<change> (let ((__or_res (atom? l1))) (if __or_res __or_res (atom? l2))) (let ((___or_res0 (atom? l1))) (if ___or_res0 ___or_res0 (atom? l2))))
            #f
            (if (same-structure? (car l1) (car l2))
               (same-structure? (cdr l1) (cdr l2))
               #f)))))
 
(define same-structure?-or (<change>
      (lambda (l1 l2)
         (let ((__or_res (if (atom? l1) (atom? l2) #f)))
            (if __or_res
               __or_res
               (if (pair? l1)
                  (if (pair? l2)
                     (if (same-structure?-or (car l1) (car l2))
                        (same-structure?-or (cdr l1) (cdr l2))
                        #f)
                     #f)
                  #f))))
      (lambda (_l10 _l20)
         (let ((___or_res0 (if (atom? _l10) (atom? _l20) #f)))
            (if ___or_res0
               ___or_res0
               (if (pair? _l10)
                  (if (pair? _l20)
                     (if (same-structure?-or (car _l10) (car _l20))
                        (same-structure?-or (cdr _l10) (cdr _l20))
                        #f)
                     #f)
                  #f))))))
 
(if (same-structure? (__toplevel_cons (__toplevel_cons 1 (__toplevel_cons 2 ())) (__toplevel_cons (__toplevel_cons (__toplevel_cons 3 4) (__toplevel_cons (__toplevel_cons (__toplevel_cons 5 (__toplevel_cons 6 ())) (__toplevel_cons (__toplevel_cons (__toplevel_cons 7 (__toplevel_cons 8 ())) (__toplevel_cons (__toplevel_cons 9 ()) ())) ())) ())) ())) (__toplevel_cons (__toplevel_cons 'a (__toplevel_cons 'b ())) (__toplevel_cons (__toplevel_cons (__toplevel_cons 'c 'd) (__toplevel_cons (__toplevel_cons (__toplevel_cons 'e (__toplevel_cons 'f ())) (__toplevel_cons (__toplevel_cons (__toplevel_cons 'g (__toplevel_cons 'h ())) (__toplevel_cons (__toplevel_cons 'i ()) ())) ())) ())) ())))
   (not
      (same-structure?
         (__toplevel_cons
            (__toplevel_cons 1 (__toplevel_cons 2 ()))
            (__toplevel_cons
               (__toplevel_cons
                  (__toplevel_cons 3 (__toplevel_cons 4 ()))
                  (__toplevel_cons
                     (__toplevel_cons
                        (__toplevel_cons 5 (__toplevel_cons 6 ()))
                        (__toplevel_cons
                           (__toplevel_cons
                              (__toplevel_cons 7 (__toplevel_cons 8 ()))
                              (__toplevel_cons (__toplevel_cons 9 ()) ()))
                           ()))
                     ()))
               ()))
         (__toplevel_cons
            (__toplevel_cons
               (__toplevel_cons
                  (__toplevel_cons 1 (__toplevel_cons 2 ()))
                  (__toplevel_cons (__toplevel_cons 3 (__toplevel_cons 4 ())) ()))
               (__toplevel_cons
                  (__toplevel_cons
                     (__toplevel_cons 5 (__toplevel_cons 6 ()))
                     (__toplevel_cons (__toplevel_cons 7 (__toplevel_cons 8 ())) ()))
                  ()))
            (__toplevel_cons 9 ()))))
   #f)
 
