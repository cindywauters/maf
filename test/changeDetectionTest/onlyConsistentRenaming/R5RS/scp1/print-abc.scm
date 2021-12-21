;; renamed lambdas/lets: 3
 
(define result ())
 
(define output (<change>
      (lambda (i)
         (set! result (cons i result)))
      (lambda (_i0)
         (set! result (cons _i0 result)))))
 
(define linebreak (<change>
      (lambda ()
         (set! result (cons 'linebreak result)))
      (lambda ()
         (set! result (cons 'linebreak result)))))
 
(define print-abc (lambda (a b c)
      (output a)
      (output " ")
      (output b)
      (output " ")
      (output c)
      (linebreak)))
 
(define foo (lambda (a b c)
      (print-abc a b c)
      (let ((a 4)
            (c 5)
            (b c))
         (print-abc a b c)
         (let ((b 6)
               (c a))
            (print-abc a b c))
         (<change>
            (let ((a b)
                  (c a))
               (print-abc a b c))
            (let ((_a0 b)
                  (_c0 a))
               (print-abc _a0 b _c0))))
      (print-abc a b c)))
 
(foo 1 2 3)
 
(equal?
   result
   (__toplevel_cons
      'linebreak
      (__toplevel_cons
         3
         (__toplevel_cons
            " "
            (__toplevel_cons
               2
               (__toplevel_cons
                  " "
                  (__toplevel_cons
                     1
                     (__toplevel_cons
                        'linebreak
                        (__toplevel_cons
                           4
                           (__toplevel_cons
                              " "
                              (__toplevel_cons
                                 3
                                 (__toplevel_cons
                                    " "
                                    (__toplevel_cons
                                       3
                                       (__toplevel_cons
                                          'linebreak
                                          (__toplevel_cons
                                             4
                                             (__toplevel_cons
                                                " "
                                                (__toplevel_cons
                                                   6
                                                   (__toplevel_cons
                                                      " "
                                                      (__toplevel_cons
                                                         4
                                                         (__toplevel_cons
                                                            'linebreak
                                                            (__toplevel_cons
                                                               5
                                                               (__toplevel_cons
                                                                  " "
                                                                  (__toplevel_cons
                                                                     3
                                                                     (__toplevel_cons
                                                                        " "
                                                                        (__toplevel_cons
                                                                           4
                                                                           (__toplevel_cons
                                                                              'linebreak
                                                                              (__toplevel_cons
                                                                                 3
                                                                                 (__toplevel_cons " " (__toplevel_cons 2 (__toplevel_cons " " (__toplevel_cons 1 ())))))))))))))))))))))))))))))))
 
