;; renamed lambdas/lets: 3
 
(define result ())
 
(define output (<change>
      (lambda (i)
         (set! result (cons i result)))
      (lambda (_i0)
         (set! result (cons _i0 result)))))
 
(define weird (<change>
      (lambda (x)
         (if (= x 1)
            1
            (if (even? x)
               (weird (/ x 2))
               (weird (+ (* 3 x) 1)))))
      (lambda (_x0)
         (if (= _x0 1)
            1
            (if (even? _x0)
               (weird (/ _x0 2))
               (weird (+ (* 3 _x0) 1)))))))
 
(define depth-weird (<change>
      (lambda (x)
         (if (= x 1)
            0
            (if (even? x)
               (+ 1 (depth-weird (/ x 2)))
               (+ (depth-weird (+ (* 3 x) 1)) 1))))
      (lambda (_x0)
         (if (= _x0 1)
            0
            (if (even? _x0)
               (+ 1 (depth-weird (/ _x0 2)))
               (+ (depth-weird (+ (* 3 _x0) 1)) 1))))))
 
(define weird-table (lambda (min max)
      (if (< min max)
         (begin
            (for-each output (list min "\t" (depth-weird min) "\n"))
            (weird-table (+ min 1) max))
         #f)))
 
(weird-table 1 10)
 
(if (= (weird 15) 1)
   (if (= (depth-weird 15) 17)
      (equal?
         result
         (__toplevel_cons
            "\n"
            (__toplevel_cons
               19
               (__toplevel_cons
                  "\t"
                  (__toplevel_cons
                     9
                     (__toplevel_cons
                        "\n"
                        (__toplevel_cons
                           3
                           (__toplevel_cons
                              "\t"
                              (__toplevel_cons
                                 8
                                 (__toplevel_cons
                                    "\n"
                                    (__toplevel_cons
                                       16
                                       (__toplevel_cons
                                          "\t"
                                          (__toplevel_cons
                                             7
                                             (__toplevel_cons
                                                "\n"
                                                (__toplevel_cons
                                                   8
                                                   (__toplevel_cons
                                                      "\t"
                                                      (__toplevel_cons
                                                         6
                                                         (__toplevel_cons
                                                            "\n"
                                                            (__toplevel_cons
                                                               5
                                                               (__toplevel_cons
                                                                  "\t"
                                                                  (__toplevel_cons
                                                                     5
                                                                     (__toplevel_cons
                                                                        "\n"
                                                                        (__toplevel_cons
                                                                           2
                                                                           (__toplevel_cons
                                                                              "\t"
                                                                              (__toplevel_cons
                                                                                 4
                                                                                 (__toplevel_cons
                                                                                    "\n"
                                                                                    (__toplevel_cons
                                                                                       7
                                                                                       (__toplevel_cons
                                                                                          "\t"
                                                                                          (__toplevel_cons
                                                                                             3
                                                                                             (__toplevel_cons
                                                                                                "\n"
                                                                                                (__toplevel_cons
                                                                                                   1
                                                                                                   (__toplevel_cons
                                                                                                      "\t"
                                                                                                      (__toplevel_cons
                                                                                                         2
                                                                                                         (__toplevel_cons "\n" (__toplevel_cons 0 (__toplevel_cons "\t" (__toplevel_cons 1 ())))))))))))))))))))))))))))))))))))))
      #f)
   #f)
 
