;; renamed lambdas/lets: 2
 
(define day-of-week 'Sunday)
 
(define make-greeting (<change>
      (lambda ()
         (__toplevel_cons
            'Welcome
            (__toplevel_cons
               'to
               (__toplevel_cons
                  'the
                  (__toplevel_cons
                     'FooBar
                     (__toplevel_cons
                        'system!
                        (__toplevel_cons
                           'We
                           (__toplevel_cons
                              'hope
                              (__toplevel_cons
                                 'you
                                 (__toplevel_cons
                                    'enjoy
                                    (__toplevel_cons
                                       'your
                                       (__toplevel_cons
                                          'visit
                                          (__toplevel_cons
                                             'on
                                             (__toplevel_cons 'this (__toplevel_cons 'fine (__toplevel_cons day-of-week ()))))))))))))))))
      (lambda ()
         (__toplevel_cons
            'Welcome
            (__toplevel_cons
               'to
               (__toplevel_cons
                  'the
                  (__toplevel_cons
                     'FooBar
                     (__toplevel_cons
                        'system!
                        (__toplevel_cons
                           'We
                           (__toplevel_cons
                              'hope
                              (__toplevel_cons
                                 'you
                                 (__toplevel_cons
                                    'enjoy
                                    (__toplevel_cons
                                       'your
                                       (__toplevel_cons
                                          'visit
                                          (__toplevel_cons
                                             'on
                                             (__toplevel_cons 'this (__toplevel_cons 'fine (__toplevel_cons day-of-week ()))))))))))))))))))
 
(<change>
   (let ((exp1 (__toplevel_cons 'list (__toplevel_cons 3 (__toplevel_cons 4 ()))))
         (res1a (__toplevel_cons 'list (__toplevel_cons (+ 1 2) (__toplevel_cons 4 ()))))
         (res1b (__toplevel_cons 'list (__toplevel_cons (+ 1 2) (__toplevel_cons 4 ()))))
         (exp2 5)
         (res2a (+ 2 3))
         (res2b (+ 2 3))
         (exp3 (__toplevel_cons
                 'Welcome
                 (__toplevel_cons
                    'to
                    (__toplevel_cons
                       'the
                       (__toplevel_cons
                          'FooBar
                          (__toplevel_cons
                             'system!
                             (__toplevel_cons
                                'We
                                (__toplevel_cons
                                   'hope
                                   (__toplevel_cons
                                      'you
                                      (__toplevel_cons
                                         'enjoy
                                         (__toplevel_cons
                                            'your
                                            (__toplevel_cons
                                               'visit
                                               (__toplevel_cons 'on (__toplevel_cons 'this (__toplevel_cons 'fine (__toplevel_cons 'Sunday ()))))))))))))))))
         (res3 (make-greeting))
         (exp4 (__toplevel_cons '+ (__toplevel_cons 2 (__toplevel_cons 3 ()))))
         (res4a (__toplevel_cons '+ (__toplevel_cons 2 (__toplevel_cons 3 ()))))
         (res4b (__toplevel_cons '+ (__toplevel_cons 2 (__toplevel_cons 3 ()))))
         (exp5 (__toplevel_cons '+ (__toplevel_cons 2 (__toplevel_cons 12 ()))))
         (res5a (__toplevel_cons '+ (__toplevel_cons 2 (__toplevel_cons (* 3 4) ()))))
         (res5b (__toplevel_cons '+ (__toplevel_cons 2 (__toplevel_cons (* 3 4) ())))))
      (if (equal? exp1 res1a)
         (if (equal? exp1 res1b)
            (if (= exp2 res2a)
               (if (= exp2 res2b)
                  (if (equal? exp3 res3)
                     (if (equal? exp4 res4a)
                        (if (equal? exp4 res4b)
                           (if (equal? exp5 res5a) (equal? exp5 res5b) #f)
                           #f)
                        #f)
                     #f)
                  #f)
               #f)
            #f)
         #f))
   (let ((_exp10 (__toplevel_cons 'list (__toplevel_cons 3 (__toplevel_cons 4 ()))))
         (_res1a0 (__toplevel_cons 'list (__toplevel_cons (+ 1 2) (__toplevel_cons 4 ()))))
         (_res1b0 (__toplevel_cons 'list (__toplevel_cons (+ 1 2) (__toplevel_cons 4 ()))))
         (_exp20 5)
         (_res2a0 (+ 2 3))
         (_res2b0 (+ 2 3))
         (_exp30 (__toplevel_cons
                   'Welcome
                   (__toplevel_cons
                      'to
                      (__toplevel_cons
                         'the
                         (__toplevel_cons
                            'FooBar
                            (__toplevel_cons
                               'system!
                               (__toplevel_cons
                                  'We
                                  (__toplevel_cons
                                     'hope
                                     (__toplevel_cons
                                        'you
                                        (__toplevel_cons
                                           'enjoy
                                           (__toplevel_cons
                                              'your
                                              (__toplevel_cons
                                                 'visit
                                                 (__toplevel_cons 'on (__toplevel_cons 'this (__toplevel_cons 'fine (__toplevel_cons 'Sunday ()))))))))))))))))
         (_res30 (make-greeting))
         (_exp40 (__toplevel_cons '+ (__toplevel_cons 2 (__toplevel_cons 3 ()))))
         (_res4a0 (__toplevel_cons '+ (__toplevel_cons 2 (__toplevel_cons 3 ()))))
         (_res4b0 (__toplevel_cons '+ (__toplevel_cons 2 (__toplevel_cons 3 ()))))
         (_exp50 (__toplevel_cons '+ (__toplevel_cons 2 (__toplevel_cons 12 ()))))
         (_res5a0 (__toplevel_cons '+ (__toplevel_cons 2 (__toplevel_cons (* 3 4) ()))))
         (_res5b0 (__toplevel_cons '+ (__toplevel_cons 2 (__toplevel_cons (* 3 4) ())))))
      (if (equal? _exp10 _res1a0)
         (if (equal? _exp10 _res1b0)
            (if (= _exp20 _res2a0)
               (if (= _exp20 _res2b0)
                  (if (equal? _exp30 _res30)
                     (if (equal? _exp40 _res4a0)
                        (if (equal? _exp40 _res4b0)
                           (if (equal? _exp50 _res5a0)
                              (equal? _exp50 _res5b0)
                              #f)
                           #f)
                        #f)
                     #f)
                  #f)
               #f)
            #f)
         #f)))
 
