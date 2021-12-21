;; renamed lambdas/lets: 1
 
(define all-but-interval (<change>
      (lambda (lst min max)
         (define aux (lambda (last-smaller-cons aux-lst)
               (if (null? aux-lst)
                  (set-cdr! last-smaller-cons ())
                  (if (< (car aux-lst) min)
                     (aux aux-lst (cdr aux-lst))
                     (if (> (car aux-lst) max)
                        (set-cdr! last-smaller-cons aux-lst)
                        (aux last-smaller-cons (cdr aux-lst)))))))
         (aux lst lst)
         lst)
      (lambda (_lst0 _min0 _max0)
         (define aux (lambda (_last-smaller-cons0 _aux-lst0)
               (if (null? _aux-lst0)
                  (set-cdr! _last-smaller-cons0 ())
                  (if (< (car _aux-lst0) _min0)
                     (aux _aux-lst0 (cdr _aux-lst0))
                     (if (> (car _aux-lst0) _max0)
                        (set-cdr! _last-smaller-cons0 _aux-lst0)
                        (aux _last-smaller-cons0 (cdr _aux-lst0)))))))
         (aux _lst0 _lst0)
         _lst0)))
 
(if (equal? (all-but-interval (__toplevel_cons 1 (__toplevel_cons 2 (__toplevel_cons 3 (__toplevel_cons 4 (__toplevel_cons 5 (__toplevel_cons 6 ())))))) 2 4) (__toplevel_cons 1 (__toplevel_cons 5 (__toplevel_cons 6 ()))))
   (if (equal? (all-but-interval (__toplevel_cons 1 (__toplevel_cons 2 (__toplevel_cons 3 (__toplevel_cons 4 (__toplevel_cons 5 ()))))) 2 2) (__toplevel_cons 1 (__toplevel_cons 3 (__toplevel_cons 4 (__toplevel_cons 5 ())))))
      (equal?
         (all-but-interval
            (__toplevel_cons
               1
               (__toplevel_cons 2 (__toplevel_cons 5 (__toplevel_cons 6 (__toplevel_cons 7 ())))))
            3
            9)
         (__toplevel_cons 1 (__toplevel_cons 2 ())))
      #f)
   #f)
 
