;; renamed lambdas/lets: 2
 
(define quick-sort (lambda (a-list)
      (define rearrange (<change>
            (lambda (pivot some-list)
               (define rearrange-iter (lambda (rest result)
                     (if (null? rest)
                        result
                        (if (<= (car rest) pivot)
                           (rearrange-iter (cdr rest) (cons (cons (car rest) (car result)) (cdr result)))
                           (rearrange-iter (cdr rest) (cons (car result) (cons (car rest) (cdr result))))))))
               (rearrange-iter some-list (cons () ())))
            (lambda (_pivot0 _some-list0)
               (define rearrange-iter (lambda (_rest0 _result0)
                     (if (null? _rest0)
                        _result0
                        (if (<= (car _rest0) _pivot0)
                           (rearrange-iter (cdr _rest0) (cons (cons (car _rest0) (car _result0)) (cdr _result0)))
                           (rearrange-iter (cdr _rest0) (cons (car _result0) (cons (car _rest0) (cdr _result0))))))))
               (rearrange-iter _some-list0 (cons () ())))))
      (if (<= (length a-list) 1)
         a-list
         (let* ((pivot (car a-list))
                (sub-lists (rearrange pivot (cdr a-list))))
            (append (quick-sort (car sub-lists)) (append (list pivot) (quick-sort (cdr sub-lists))))))))
 
(equal?
   (quick-sort
      (__toplevel_cons
         9
         (__toplevel_cons
            8
            (__toplevel_cons
               7
               (__toplevel_cons
                  6
                  (__toplevel_cons
                     5
                     (__toplevel_cons
                        4
                        (__toplevel_cons
                           3
                           (__toplevel_cons 2 (__toplevel_cons 1 (__toplevel_cons 0 (__toplevel_cons 9 ()))))))))))))
   (__toplevel_cons
      0
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
                        (__toplevel_cons 7 (__toplevel_cons 8 (__toplevel_cons 9 (__toplevel_cons 9 ()))))))))))))
 
