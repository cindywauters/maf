;; renamed lambdas/lets: 1
 
(define schuif-in! (<change>
      (lambda (l1 l2)
         (if (null? (cdr l1))
            (begin
               (set-cdr! l1 l2)
               'ok)
            (if (null? l2)
               'ok
               (let ((rest1 (cdr l1))
                     (rest2 (cdr l2)))
                  (set-cdr! l1 l2)
                  (set-cdr! l2 rest1)
                  (schuif-in! rest1 rest2)))))
      (lambda (_l10 _l20)
         (if (null? (cdr _l10))
            (begin
               (set-cdr! _l10 _l20)
               'ok)
            (if (null? _l20)
               'ok
               (let ((_rest10 (cdr _l10))
                     (_rest20 (cdr _l20)))
                  (set-cdr! _l10 _l20)
                  (set-cdr! _l20 _rest10)
                  (schuif-in! _rest10 _rest20)))))))
 
(define lijst1 (__toplevel_cons 1 (__toplevel_cons 3 (__toplevel_cons 5 ()))))
 
(define lijst2 (__toplevel_cons 2 (__toplevel_cons 4 (__toplevel_cons 6 (__toplevel_cons 8 ())))))
 
(schuif-in! lijst1 lijst2)
 
(equal?
   lijst1
   (__toplevel_cons
      1
      (__toplevel_cons
         2
         (__toplevel_cons
            3
            (__toplevel_cons 4 (__toplevel_cons 5 (__toplevel_cons 6 (__toplevel_cons 8 ()))))))))
 
