;; renamed lambdas/lets: 1
 
(define create-n (lambda (n)
      (letrec ((__do_loop (lambda (n a)
                            (if (= n 0) a (__do_loop (- n 1) (cons () a))))))
         (__do_loop n ()))))
 
(define *ll* (create-n 200))
 
(define iterative-div2 (lambda (l)
      (letrec ((__do_loop (<change>
                            (lambda (l a)
                               (if (null? l)
                                  a
                                  (__do_loop (cddr l) (cons (car l) a))))
                            (lambda (_l0 _a0)
                               (if (null? _l0)
                                  _a0
                                  (__do_loop (cddr _l0) (cons (car _l0) _a0)))))))
         (__do_loop l ()))))
 
(equal?
   (iterative-div2 *ll*)
   (__toplevel_cons
      ()
      (__toplevel_cons
         ()
         (__toplevel_cons
            ()
            (__toplevel_cons
               ()
               (__toplevel_cons
                  ()
                  (__toplevel_cons
                     ()
                     (__toplevel_cons
                        ()
                        (__toplevel_cons
                           ()
                           (__toplevel_cons
                              ()
                              (__toplevel_cons
                                 ()
                                 (__toplevel_cons
                                    ()
                                    (__toplevel_cons
                                       ()
                                       (__toplevel_cons
                                          ()
                                          (__toplevel_cons
                                             ()
                                             (__toplevel_cons
                                                ()
                                                (__toplevel_cons
                                                   ()
                                                   (__toplevel_cons
                                                      ()
                                                      (__toplevel_cons
                                                         ()
                                                         (__toplevel_cons
                                                            ()
                                                            (__toplevel_cons
                                                               ()
                                                               (__toplevel_cons
                                                                  ()
                                                                  (__toplevel_cons
                                                                     ()
                                                                     (__toplevel_cons
                                                                        ()
                                                                        (__toplevel_cons
                                                                           ()
                                                                           (__toplevel_cons
                                                                              ()
                                                                              (__toplevel_cons
                                                                                 ()
                                                                                 (__toplevel_cons
                                                                                    ()
                                                                                    (__toplevel_cons
                                                                                       ()
                                                                                       (__toplevel_cons
                                                                                          ()
                                                                                          (__toplevel_cons
                                                                                             ()
                                                                                             (__toplevel_cons
                                                                                                ()
                                                                                                (__toplevel_cons
                                                                                                   ()
                                                                                                   (__toplevel_cons
                                                                                                      ()
                                                                                                      (__toplevel_cons
                                                                                                         ()
                                                                                                         (__toplevel_cons
                                                                                                            ()
                                                                                                            (__toplevel_cons
                                                                                                               ()
                                                                                                               (__toplevel_cons
                                                                                                                  ()
                                                                                                                  (__toplevel_cons
                                                                                                                     ()
                                                                                                                     (__toplevel_cons
                                                                                                                        ()
                                                                                                                        (__toplevel_cons
                                                                                                                           ()
                                                                                                                           (__toplevel_cons
                                                                                                                              ()
                                                                                                                              (__toplevel_cons
                                                                                                                                 ()
                                                                                                                                 (__toplevel_cons
                                                                                                                                    ()
                                                                                                                                    (__toplevel_cons
                                                                                                                                       ()
                                                                                                                                       (__toplevel_cons
                                                                                                                                          ()
                                                                                                                                          (__toplevel_cons
                                                                                                                                             ()
                                                                                                                                             (__toplevel_cons
                                                                                                                                                ()
                                                                                                                                                (__toplevel_cons
                                                                                                                                                   ()
                                                                                                                                                   (__toplevel_cons
                                                                                                                                                      ()
                                                                                                                                                      (__toplevel_cons
                                                                                                                                                         ()
                                                                                                                                                         (__toplevel_cons
                                                                                                                                                            ()
                                                                                                                                                            (__toplevel_cons
                                                                                                                                                               ()
                                                                                                                                                               (__toplevel_cons
                                                                                                                                                                  ()
                                                                                                                                                                  (__toplevel_cons
                                                                                                                                                                     ()
                                                                                                                                                                     (__toplevel_cons
                                                                                                                                                                        ()
                                                                                                                                                                        (__toplevel_cons
                                                                                                                                                                           ()
                                                                                                                                                                           (__toplevel_cons
                                                                                                                                                                              ()
                                                                                                                                                                              (__toplevel_cons
                                                                                                                                                                                 ()
                                                                                                                                                                                 (__toplevel_cons
                                                                                                                                                                                    ()
                                                                                                                                                                                    (__toplevel_cons
                                                                                                                                                                                       ()
                                                                                                                                                                                       (__toplevel_cons
                                                                                                                                                                                          ()
                                                                                                                                                                                          (__toplevel_cons
                                                                                                                                                                                             ()
                                                                                                                                                                                             (__toplevel_cons
                                                                                                                                                                                                ()
                                                                                                                                                                                                (__toplevel_cons
                                                                                                                                                                                                   ()
                                                                                                                                                                                                   (__toplevel_cons
                                                                                                                                                                                                      ()
                                                                                                                                                                                                      (__toplevel_cons
                                                                                                                                                                                                         ()
                                                                                                                                                                                                         (__toplevel_cons
                                                                                                                                                                                                            ()
                                                                                                                                                                                                            (__toplevel_cons
                                                                                                                                                                                                               ()
                                                                                                                                                                                                               (__toplevel_cons
                                                                                                                                                                                                                  ()
                                                                                                                                                                                                                  (__toplevel_cons
                                                                                                                                                                                                                     ()
                                                                                                                                                                                                                     (__toplevel_cons
                                                                                                                                                                                                                        ()
                                                                                                                                                                                                                        (__toplevel_cons
                                                                                                                                                                                                                           ()
                                                                                                                                                                                                                           (__toplevel_cons
                                                                                                                                                                                                                              ()
                                                                                                                                                                                                                              (__toplevel_cons
                                                                                                                                                                                                                                 ()
                                                                                                                                                                                                                                 (__toplevel_cons
                                                                                                                                                                                                                                    ()
                                                                                                                                                                                                                                    (__toplevel_cons
                                                                                                                                                                                                                                       ()
                                                                                                                                                                                                                                       (__toplevel_cons
                                                                                                                                                                                                                                          ()
                                                                                                                                                                                                                                          (__toplevel_cons
                                                                                                                                                                                                                                             ()
                                                                                                                                                                                                                                             (__toplevel_cons
                                                                                                                                                                                                                                                ()
                                                                                                                                                                                                                                                (__toplevel_cons
                                                                                                                                                                                                                                                   ()
                                                                                                                                                                                                                                                   (__toplevel_cons
                                                                                                                                                                                                                                                      ()
                                                                                                                                                                                                                                                      (__toplevel_cons
                                                                                                                                                                                                                                                         ()
                                                                                                                                                                                                                                                         (__toplevel_cons
                                                                                                                                                                                                                                                            ()
                                                                                                                                                                                                                                                            (__toplevel_cons
                                                                                                                                                                                                                                                               ()
                                                                                                                                                                                                                                                               (__toplevel_cons
                                                                                                                                                                                                                                                                  ()
                                                                                                                                                                                                                                                                  (__toplevel_cons
                                                                                                                                                                                                                                                                     ()
                                                                                                                                                                                                                                                                     (__toplevel_cons
                                                                                                                                                                                                                                                                        ()
                                                                                                                                                                                                                                                                        (__toplevel_cons
                                                                                                                                                                                                                                                                           ()
                                                                                                                                                                                                                                                                           (__toplevel_cons
                                                                                                                                                                                                                                                                              ()
                                                                                                                                                                                                                                                                              (__toplevel_cons
                                                                                                                                                                                                                                                                                 ()
                                                                                                                                                                                                                                                                                 (__toplevel_cons
                                                                                                                                                                                                                                                                                    ()
                                                                                                                                                                                                                                                                                    (__toplevel_cons
                                                                                                                                                                                                                                                                                       ()
                                                                                                                                                                                                                                                                                       (__toplevel_cons
                                                                                                                                                                                                                                                                                          ()
                                                                                                                                                                                                                                                                                          (__toplevel_cons
                                                                                                                                                                                                                                                                                             ()
                                                                                                                                                                                                                                                                                             (__toplevel_cons
                                                                                                                                                                                                                                                                                                ()
                                                                                                                                                                                                                                                                                                (__toplevel_cons
                                                                                                                                                                                                                                                                                                   ()
                                                                                                                                                                                                                                                                                                   (__toplevel_cons () (__toplevel_cons () (__toplevel_cons () (__toplevel_cons () ())))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
 
