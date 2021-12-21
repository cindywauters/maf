;; renamed lambdas/lets: 1
 
(define super-merge-n (<change>
      (lambda (lsts n)
         (define geef-n+rest (lambda (lst n)
               (if (let ((__or_res (= 0 n))) (if __or_res __or_res (null? lst)))
                  (cons () lst)
                  (let* ((res (geef-n+rest (cdr lst) (- n 1)))
                         (first (car res))
                         (rest (cdr res)))
                     (cons (cons (car lst) first) rest)))))
         (if (null? lsts)
            ()
            (let* ((g-n+rest (geef-n+rest (car lsts) n))
                   (first (car g-n+rest))
                   (rest (cdr g-n+rest)))
               (append first (super-merge-n (append (cdr lsts) (if (null? rest) rest (list rest))) n)))))
      (lambda (_lsts0 _n0)
         (define geef-n+rest (lambda (_lst0 _n1)
               (if (let ((___or_res0 (= 0 _n1))) (if ___or_res0 ___or_res0 (null? _lst0)))
                  (cons () _lst0)
                  (let* ((_res0 (geef-n+rest (cdr _lst0) (- _n1 1)))
                         (_first0 (car _res0))
                         (_rest0 (cdr _res0)))
                     (cons (cons (car _lst0) _first0) _rest0)))))
         (if (null? _lsts0)
            ()
            (let* ((_g-n+rest0 (geef-n+rest (car _lsts0) _n0))
                   (_first1 (car _g-n+rest0))
                   (_rest1 (cdr _g-n+rest0)))
               (append _first1 (super-merge-n (append (cdr _lsts0) (if (null? _rest1) _rest1 (list _rest1))) _n0)))))))
 
(equal?
   (super-merge-n
      (__toplevel_cons
         (__toplevel_cons
            'a
            (__toplevel_cons
               'b
               (__toplevel_cons 'c (__toplevel_cons 'd (__toplevel_cons 'e (__toplevel_cons 'f ()))))))
         (__toplevel_cons
            (__toplevel_cons
               'g
               (__toplevel_cons 'h (__toplevel_cons 'i (__toplevel_cons 'j (__toplevel_cons 'k ())))))
            (__toplevel_cons
               (__toplevel_cons
                  'l
                  (__toplevel_cons
                     'm
                     (__toplevel_cons 'n (__toplevel_cons 'o (__toplevel_cons 'p (__toplevel_cons 'q ()))))))
               (__toplevel_cons
                  (__toplevel_cons
                     'r
                     (__toplevel_cons
                        's
                        (__toplevel_cons 't (__toplevel_cons 'u (__toplevel_cons 'v (__toplevel_cons 'w ()))))))
                  ()))))
      3)
   (__toplevel_cons
      'a
      (__toplevel_cons
         'b
         (__toplevel_cons
            'c
            (__toplevel_cons
               'g
               (__toplevel_cons
                  'h
                  (__toplevel_cons
                     'i
                     (__toplevel_cons
                        'l
                        (__toplevel_cons
                           'm
                           (__toplevel_cons
                              'n
                              (__toplevel_cons
                                 'r
                                 (__toplevel_cons
                                    's
                                    (__toplevel_cons
                                       't
                                       (__toplevel_cons
                                          'd
                                          (__toplevel_cons
                                             'e
                                             (__toplevel_cons
                                                'f
                                                (__toplevel_cons
                                                   'j
                                                   (__toplevel_cons
                                                      'k
                                                      (__toplevel_cons
                                                         'o
                                                         (__toplevel_cons
                                                            'p
                                                            (__toplevel_cons 'q (__toplevel_cons 'u (__toplevel_cons 'v (__toplevel_cons 'w ()))))))))))))))))))))))))
 
