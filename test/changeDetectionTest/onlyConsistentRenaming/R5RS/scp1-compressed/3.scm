;; renamed lambdas/lets: 24
 
(define 1- (lambda (x)
      (- x 1)))
 
(define 1+ (<change>
      (lambda (x)
         (+ 1 x))
      (lambda (_x0)
         (+ 1 _x0))))
 
(define rec-add (<change>
      (lambda (a b)
         (if (= b 0) a (1+ (rec-add a (1- b)))))
      (lambda (_a0 _b0)
         (if (= _b0 0) _a0 (1+ (rec-add _a0 (1- _b0)))))))
 
(define iter-add (<change>
      (lambda (a b)
         (if (= a 0)
            b
            (if (< a 0)
               (iter-add (1+ a) (1- b))
               (if (> a 0) (iter-add (1- a) (1+ b)) #f))))
      (lambda (_a0 _b0)
         (if (= _a0 0)
            _b0
            (if (< _a0 0)
               (iter-add (1+ _a0) (1- _b0))
               (if (> _a0 0) (iter-add (1- _a0) (1+ _b0)) #f))))))
 
(= 9 (rec-add 4 5) (iter-add 4 5))
 
(define double (<change>
      (lambda (x)
         (+ x x))
      (lambda (_x0)
         (+ _x0 _x0))))
 
(define halve (<change>
      (lambda (x)
         (/ x 2))
      (lambda (_x0)
         (/ _x0 2))))
 
(define rec-fast-multiply (<change>
      (lambda (a b)
         (if (zero? b)
            0
            (if (even? b)
               (rec-fast-multiply (double a) (halve b))
               (+ a (rec-fast-multiply a (- b 1))))))
      (lambda (_a0 _b0)
         (if (zero? _b0)
            0
            (if (even? _b0)
               (rec-fast-multiply (double _a0) (halve _b0))
               (+ _a0 (rec-fast-multiply _a0 (- _b0 1))))))))
 
(define iter-fast-multiply (<change>
      (lambda (a b)
         (define iter (lambda (a b acc)
               (if (zero? b)
                  acc
                  (if (even? b)
                     (iter (double a) (halve b) acc)
                     (iter a (- b 1) (+ acc a))))))
         (iter a b 0))
      (lambda (_a0 _b0)
         (define iter (lambda (_a1 _b1 _acc0)
               (if (zero? _b1)
                  _acc0
                  (if (even? _b1)
                     (iter (double _a1) (halve _b1) _acc0)
                     (iter _a1 (- _b1 1) (+ _acc0 _a1))))))
         (iter _a0 _b0 0))))
 
(if (= (rec-fast-multiply 3 4) 12)
   (if (= (rec-fast-multiply 100 200) 20000)
      (if (= (iter-fast-multiply 3 4) 12)
         (= (iter-fast-multiply 100 200) 20000)
         #f)
      #f)
   #f)
 
(define rec-multiply (<change>
      (lambda (a b)
         (if (zero? b) 0 (+ a (rec-multiply a (- b 1)))))
      (lambda (_a0 _b0)
         (if (zero? _b0)
            0
            (+ _a0 (rec-multiply _a0 (- _b0 1)))))))
 
(define iter-multiply (<change>
      (lambda (a b)
         (define iter (lambda (result counter)
               (if (zero? counter)
                  result
                  (iter (+ result a) (- counter 1)))))
         (iter 0 b))
      (lambda (_a0 _b0)
         (define iter (lambda (_result0 _counter0)
               (if (zero? _counter0)
                  _result0
                  (iter (+ _result0 _a0) (- _counter0 1)))))
         (iter 0 _b0))))
 
(= 10 (rec-multiply 5 2) (iter-multiply 5 2))
 
(define calc-e-iter (<change>
      (lambda (n)
         (define iter (lambda (ctr res fac-prev)
               (if (> ctr n)
                  res
                  (let ((new-fac (* ctr fac-prev)))
                     (iter (+ ctr 1) (+ res (/ 1 new-fac)) new-fac)))))
         (iter 1 1 1))
      (lambda (_n0)
         (define iter (lambda (_ctr0 _res0 _fac-prev0)
               (if (> _ctr0 _n0)
                  _res0
                  (let ((_new-fac0 (* _ctr0 _fac-prev0)))
                     (iter (+ _ctr0 1) (+ _res0 (/ 1 _new-fac0)) _new-fac0)))))
         (iter 1 1 1))))
 
(define calc-cos (<change>
      (lambda (x n)
         (define iter (lambda (ctr acc fac xpow sign)
               (if (>= ctr n)
                  acc
                  (let* ((i (* 2 ctr))
                         (newfac (* fac (- i 1) i))
                         (newxpow (* xpow x x))
                         (newsign (- sign)))
                     (iter (+ ctr 1) (+ acc (/ (* newsign newxpow) newfac)) newfac newxpow newsign)))))
         (iter 1 1 1 1 1))
      (lambda (_x0 _n0)
         (define iter (lambda (_ctr0 _acc0 _fac0 _xpow0 _sign0)
               (if (>= _ctr0 _n0)
                  _acc0
                  (let* ((_i0 (* 2 _ctr0))
                         (_newfac0 (* _fac0 (- _i0 1) _i0))
                         (_newxpow0 (* _xpow0 _x0 _x0))
                         (_newsign0 (- _sign0)))
                     (iter (+ _ctr0 1) (+ _acc0 (/ (* _newsign0 _newxpow0) _newfac0)) _newfac0 _newxpow0 _newsign0)))))
         (iter 1 1 1 1 1))))
 
(define close-to (<change>
      (lambda (x y)
         (< (abs (- x y)) 1.000000e-08))
      (lambda (_x0 _y0)
         (< (abs (- _x0 _y0)) 1.000000e-08))))
 
(if (close-to (exact->inexact (calc-e-iter 10)) 2.718282e+00)
   (if (close-to (calc-cos 0 10) 1)
      (if (close-to (calc-cos (/ 3.141500e+00 2) 10) 4.632679e-05)
         (close-to (calc-cos 3.141500e+00 10) -1.000000e+00)
         #f)
      #f)
   #f)
 
(define result2 ())
 
(define display2 (lambda (i)
      (set! result2 (cons i result2))))
 
(define count1 (<change>
      (lambda (x)
         (if (= 0 x)
            (display2 x)
            (begin
               (display2 x)
               (count1 (- x 1)))))
      (lambda (_x0)
         (if (= 0 _x0)
            (display2 _x0)
            (begin
               (display2 _x0)
               (count1 (- _x0 1)))))))
 
(define count2 (<change>
      (lambda (x)
         (if (= 0 x)
            (display2 x)
            (begin
               (count2 (- x 1))
               (display2 x))))
      (lambda (_x0)
         (if (= 0 _x0)
            (display2 _x0)
            (begin
               (count2 (- _x0 1))
               (display2 _x0))))))
 
(count1 4)
 
(count2 4)
 
(equal?
   result2
   (__toplevel_cons
      4
      (__toplevel_cons
         3
         (__toplevel_cons
            2
            (__toplevel_cons
               1
               (__toplevel_cons
                  0
                  (__toplevel_cons
                     0
                     (__toplevel_cons 1 (__toplevel_cons 2 (__toplevel_cons 3 (__toplevel_cons 4 ())))))))))))
 
(define result3 ())
 
(define display3 (<change>
      (lambda (i)
         (set! result3 (cons i result3)))
      (lambda (_i0)
         (set! result3 (cons _i0 result3)))))
 
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
 
(define weird-table (<change>
      (lambda (min max)
         (if (< min max)
            (begin
               (for-each display3 (list min "\t" (depth-weird min) "\n"))
               (weird-table (+ min 1) max))
            #f))
      (lambda (_min0 _max0)
         (if (< _min0 _max0)
            (begin
               (for-each display3 (list _min0 "\t" (depth-weird _min0) "\n"))
               (weird-table (+ _min0 1) _max0))
            #f))))
 
(weird-table 1 10)
 
(if (= (weird 15) 1)
   (if (= (depth-weird 15) 17)
      (equal?
         result3
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
 
(define sim-multiply (<change>
      (lambda (a b)
         (if (zero? b) 1 (+ 1 (sim-multiply a (- b 1)))))
      (lambda (_a0 _b0)
         (if (zero? _b0)
            1
            (+ 1 (sim-multiply _a0 (- _b0 1)))))))
 
(define sim-fast-multiply (<change>
      (lambda (a b)
         (if (zero? b)
            1
            (if (even? b)
               (+ 1 (sim-fast-multiply (double a) (halve b)))
               (+ 1 (sim-fast-multiply a (- b 1))))))
      (lambda (_a0 _b0)
         (if (zero? _b0)
            1
            (if (even? _b0)
               (+ 1 (sim-fast-multiply (double _a0) (halve _b0)))
               (+ 1 (sim-fast-multiply _a0 (- _b0 1))))))))
 
(if (= (sim-multiply 14 2365) 2366)
   (= (sim-fast-multiply 14 2365) 19)
   #f)
 
(define result4 ())
 
(define display4 (<change>
      (lambda (i)
         (set! result4 (cons i result4)))
      (lambda (_i0)
         (set! result4 (cons _i0 result4)))))
 
(define newline4 (<change>
      (lambda ()
         (set! result4 (cons 'newline result4)))
      (lambda ()
         (set! result4 (cons 'newline result4)))))
 
(define display-n (<change>
      (lambda (n x)
         (if (> n 0)
            (begin
               (display4 x)
               (display-n (- n 1) x))
            #f))
      (lambda (_n0 _x0)
         (if (> _n0 0)
            (begin
               (display4 _x0)
               (display-n (- _n0 1) _x0))
            #f))))
 
(define parasol (<change>
      (lambda (n)
         (define triangle (lambda (i)
               (if (< i n)
                  (begin
                     (display-n (- n i 1) " ")
                     (display-n (+ (* 2 i) 1) "*")
                     (newline4)
                     (triangle (+ i 1)))
                  #f)))
         (define stick (lambda (i)
               (if (< i 3)
                  (begin
                     (display-n (- n 1) " ")
                     (display4 "*")
                     (newline4)
                     (stick (+ i 1)))
                  #f)))
         (triangle 0)
         (stick 0))
      (lambda (_n0)
         (define triangle (lambda (_i0)
               (if (< _i0 _n0)
                  (begin
                     (display-n (- _n0 _i0 1) " ")
                     (display-n (+ (* 2 _i0) 1) "*")
                     (newline4)
                     (triangle (+ _i0 1)))
                  #f)))
         (define stick (lambda (_i1)
               (if (< _i1 3)
                  (begin
                     (display-n (- _n0 1) " ")
                     (display4 "*")
                     (newline4)
                     (stick (+ _i1 1)))
                  #f)))
         (triangle 0)
         (stick 0))))
 
(parasol 10)
 
(equal?
   result4
   (__toplevel_cons
      'newline
      (__toplevel_cons
         "*"
         (__toplevel_cons
            " "
            (__toplevel_cons
               " "
               (__toplevel_cons
                  " "
                  (__toplevel_cons
                     " "
                     (__toplevel_cons
                        " "
                        (__toplevel_cons
                           " "
                           (__toplevel_cons
                              " "
                              (__toplevel_cons
                                 " "
                                 (__toplevel_cons
                                    " "
                                    (__toplevel_cons
                                       'newline
                                       (__toplevel_cons
                                          "*"
                                          (__toplevel_cons
                                             " "
                                             (__toplevel_cons
                                                " "
                                                (__toplevel_cons
                                                   " "
                                                   (__toplevel_cons
                                                      " "
                                                      (__toplevel_cons
                                                         " "
                                                         (__toplevel_cons
                                                            " "
                                                            (__toplevel_cons
                                                               " "
                                                               (__toplevel_cons
                                                                  " "
                                                                  (__toplevel_cons
                                                                     " "
                                                                     (__toplevel_cons
                                                                        'newline
                                                                        (__toplevel_cons
                                                                           "*"
                                                                           (__toplevel_cons
                                                                              " "
                                                                              (__toplevel_cons
                                                                                 " "
                                                                                 (__toplevel_cons
                                                                                    " "
                                                                                    (__toplevel_cons
                                                                                       " "
                                                                                       (__toplevel_cons
                                                                                          " "
                                                                                          (__toplevel_cons
                                                                                             " "
                                                                                             (__toplevel_cons
                                                                                                " "
                                                                                                (__toplevel_cons
                                                                                                   " "
                                                                                                   (__toplevel_cons
                                                                                                      " "
                                                                                                      (__toplevel_cons
                                                                                                         'newline
                                                                                                         (__toplevel_cons
                                                                                                            "*"
                                                                                                            (__toplevel_cons
                                                                                                               "*"
                                                                                                               (__toplevel_cons
                                                                                                                  "*"
                                                                                                                  (__toplevel_cons
                                                                                                                     "*"
                                                                                                                     (__toplevel_cons
                                                                                                                        "*"
                                                                                                                        (__toplevel_cons
                                                                                                                           "*"
                                                                                                                           (__toplevel_cons
                                                                                                                              "*"
                                                                                                                              (__toplevel_cons
                                                                                                                                 "*"
                                                                                                                                 (__toplevel_cons
                                                                                                                                    "*"
                                                                                                                                    (__toplevel_cons
                                                                                                                                       "*"
                                                                                                                                       (__toplevel_cons
                                                                                                                                          "*"
                                                                                                                                          (__toplevel_cons
                                                                                                                                             "*"
                                                                                                                                             (__toplevel_cons
                                                                                                                                                "*"
                                                                                                                                                (__toplevel_cons
                                                                                                                                                   "*"
                                                                                                                                                   (__toplevel_cons
                                                                                                                                                      "*"
                                                                                                                                                      (__toplevel_cons
                                                                                                                                                         "*"
                                                                                                                                                         (__toplevel_cons
                                                                                                                                                            "*"
                                                                                                                                                            (__toplevel_cons
                                                                                                                                                               "*"
                                                                                                                                                               (__toplevel_cons
                                                                                                                                                                  "*"
                                                                                                                                                                  (__toplevel_cons
                                                                                                                                                                     'newline
                                                                                                                                                                     (__toplevel_cons
                                                                                                                                                                        "*"
                                                                                                                                                                        (__toplevel_cons
                                                                                                                                                                           "*"
                                                                                                                                                                           (__toplevel_cons
                                                                                                                                                                              "*"
                                                                                                                                                                              (__toplevel_cons
                                                                                                                                                                                 "*"
                                                                                                                                                                                 (__toplevel_cons
                                                                                                                                                                                    "*"
                                                                                                                                                                                    (__toplevel_cons
                                                                                                                                                                                       "*"
                                                                                                                                                                                       (__toplevel_cons
                                                                                                                                                                                          "*"
                                                                                                                                                                                          (__toplevel_cons
                                                                                                                                                                                             "*"
                                                                                                                                                                                             (__toplevel_cons
                                                                                                                                                                                                "*"
                                                                                                                                                                                                (__toplevel_cons
                                                                                                                                                                                                   "*"
                                                                                                                                                                                                   (__toplevel_cons
                                                                                                                                                                                                      "*"
                                                                                                                                                                                                      (__toplevel_cons
                                                                                                                                                                                                         "*"
                                                                                                                                                                                                         (__toplevel_cons
                                                                                                                                                                                                            "*"
                                                                                                                                                                                                            (__toplevel_cons
                                                                                                                                                                                                               "*"
                                                                                                                                                                                                               (__toplevel_cons
                                                                                                                                                                                                                  "*"
                                                                                                                                                                                                                  (__toplevel_cons
                                                                                                                                                                                                                     "*"
                                                                                                                                                                                                                     (__toplevel_cons
                                                                                                                                                                                                                        "*"
                                                                                                                                                                                                                        (__toplevel_cons
                                                                                                                                                                                                                           " "
                                                                                                                                                                                                                           (__toplevel_cons
                                                                                                                                                                                                                              'newline
                                                                                                                                                                                                                              (__toplevel_cons
                                                                                                                                                                                                                                 "*"
                                                                                                                                                                                                                                 (__toplevel_cons
                                                                                                                                                                                                                                    "*"
                                                                                                                                                                                                                                    (__toplevel_cons
                                                                                                                                                                                                                                       "*"
                                                                                                                                                                                                                                       (__toplevel_cons
                                                                                                                                                                                                                                          "*"
                                                                                                                                                                                                                                          (__toplevel_cons
                                                                                                                                                                                                                                             "*"
                                                                                                                                                                                                                                             (__toplevel_cons
                                                                                                                                                                                                                                                "*"
                                                                                                                                                                                                                                                (__toplevel_cons
                                                                                                                                                                                                                                                   "*"
                                                                                                                                                                                                                                                   (__toplevel_cons
                                                                                                                                                                                                                                                      "*"
                                                                                                                                                                                                                                                      (__toplevel_cons
                                                                                                                                                                                                                                                         "*"
                                                                                                                                                                                                                                                         (__toplevel_cons
                                                                                                                                                                                                                                                            "*"
                                                                                                                                                                                                                                                            (__toplevel_cons
                                                                                                                                                                                                                                                               "*"
                                                                                                                                                                                                                                                               (__toplevel_cons
                                                                                                                                                                                                                                                                  "*"
                                                                                                                                                                                                                                                                  (__toplevel_cons
                                                                                                                                                                                                                                                                     "*"
                                                                                                                                                                                                                                                                     (__toplevel_cons
                                                                                                                                                                                                                                                                        "*"
                                                                                                                                                                                                                                                                        (__toplevel_cons
                                                                                                                                                                                                                                                                           "*"
                                                                                                                                                                                                                                                                           (__toplevel_cons
                                                                                                                                                                                                                                                                              " "
                                                                                                                                                                                                                                                                              (__toplevel_cons
                                                                                                                                                                                                                                                                                 " "
                                                                                                                                                                                                                                                                                 (__toplevel_cons
                                                                                                                                                                                                                                                                                    'newline
                                                                                                                                                                                                                                                                                    (__toplevel_cons
                                                                                                                                                                                                                                                                                       "*"
                                                                                                                                                                                                                                                                                       (__toplevel_cons
                                                                                                                                                                                                                                                                                          "*"
                                                                                                                                                                                                                                                                                          (__toplevel_cons
                                                                                                                                                                                                                                                                                             "*"
                                                                                                                                                                                                                                                                                             (__toplevel_cons
                                                                                                                                                                                                                                                                                                "*"
                                                                                                                                                                                                                                                                                                (__toplevel_cons
                                                                                                                                                                                                                                                                                                   "*"
                                                                                                                                                                                                                                                                                                   (__toplevel_cons
                                                                                                                                                                                                                                                                                                      "*"
                                                                                                                                                                                                                                                                                                      (__toplevel_cons
                                                                                                                                                                                                                                                                                                         "*"
                                                                                                                                                                                                                                                                                                         (__toplevel_cons
                                                                                                                                                                                                                                                                                                            "*"
                                                                                                                                                                                                                                                                                                            (__toplevel_cons
                                                                                                                                                                                                                                                                                                               "*"
                                                                                                                                                                                                                                                                                                               (__toplevel_cons
                                                                                                                                                                                                                                                                                                                  "*"
                                                                                                                                                                                                                                                                                                                  (__toplevel_cons
                                                                                                                                                                                                                                                                                                                     "*"
                                                                                                                                                                                                                                                                                                                     (__toplevel_cons
                                                                                                                                                                                                                                                                                                                        "*"
                                                                                                                                                                                                                                                                                                                        (__toplevel_cons
                                                                                                                                                                                                                                                                                                                           "*"
                                                                                                                                                                                                                                                                                                                           (__toplevel_cons
                                                                                                                                                                                                                                                                                                                              " "
                                                                                                                                                                                                                                                                                                                              (__toplevel_cons
                                                                                                                                                                                                                                                                                                                                 " "
                                                                                                                                                                                                                                                                                                                                 (__toplevel_cons
                                                                                                                                                                                                                                                                                                                                    " "
                                                                                                                                                                                                                                                                                                                                    (__toplevel_cons
                                                                                                                                                                                                                                                                                                                                       'newline
                                                                                                                                                                                                                                                                                                                                       (__toplevel_cons
                                                                                                                                                                                                                                                                                                                                          "*"
                                                                                                                                                                                                                                                                                                                                          (__toplevel_cons
                                                                                                                                                                                                                                                                                                                                             "*"
                                                                                                                                                                                                                                                                                                                                             (__toplevel_cons
                                                                                                                                                                                                                                                                                                                                                "*"
                                                                                                                                                                                                                                                                                                                                                (__toplevel_cons
                                                                                                                                                                                                                                                                                                                                                   "*"
                                                                                                                                                                                                                                                                                                                                                   (__toplevel_cons
                                                                                                                                                                                                                                                                                                                                                      "*"
                                                                                                                                                                                                                                                                                                                                                      (__toplevel_cons
                                                                                                                                                                                                                                                                                                                                                         "*"
                                                                                                                                                                                                                                                                                                                                                         (__toplevel_cons
                                                                                                                                                                                                                                                                                                                                                            "*"
                                                                                                                                                                                                                                                                                                                                                            (__toplevel_cons
                                                                                                                                                                                                                                                                                                                                                               "*"
                                                                                                                                                                                                                                                                                                                                                               (__toplevel_cons
                                                                                                                                                                                                                                                                                                                                                                  "*"
                                                                                                                                                                                                                                                                                                                                                                  (__toplevel_cons
                                                                                                                                                                                                                                                                                                                                                                     "*"
                                                                                                                                                                                                                                                                                                                                                                     (__toplevel_cons
                                                                                                                                                                                                                                                                                                                                                                        "*"
                                                                                                                                                                                                                                                                                                                                                                        (__toplevel_cons
                                                                                                                                                                                                                                                                                                                                                                           " "
                                                                                                                                                                                                                                                                                                                                                                           (__toplevel_cons
                                                                                                                                                                                                                                                                                                                                                                              " "
                                                                                                                                                                                                                                                                                                                                                                              (__toplevel_cons
                                                                                                                                                                                                                                                                                                                                                                                 " "
                                                                                                                                                                                                                                                                                                                                                                                 (__toplevel_cons
                                                                                                                                                                                                                                                                                                                                                                                    " "
                                                                                                                                                                                                                                                                                                                                                                                    (__toplevel_cons
                                                                                                                                                                                                                                                                                                                                                                                       'newline
                                                                                                                                                                                                                                                                                                                                                                                       (__toplevel_cons
                                                                                                                                                                                                                                                                                                                                                                                          "*"
                                                                                                                                                                                                                                                                                                                                                                                          (__toplevel_cons
                                                                                                                                                                                                                                                                                                                                                                                             "*"
                                                                                                                                                                                                                                                                                                                                                                                             (__toplevel_cons
                                                                                                                                                                                                                                                                                                                                                                                                "*"
                                                                                                                                                                                                                                                                                                                                                                                                (__toplevel_cons
                                                                                                                                                                                                                                                                                                                                                                                                   "*"
                                                                                                                                                                                                                                                                                                                                                                                                   (__toplevel_cons
                                                                                                                                                                                                                                                                                                                                                                                                      "*"
                                                                                                                                                                                                                                                                                                                                                                                                      (__toplevel_cons
                                                                                                                                                                                                                                                                                                                                                                                                         "*"
                                                                                                                                                                                                                                                                                                                                                                                                         (__toplevel_cons
                                                                                                                                                                                                                                                                                                                                                                                                            "*"
                                                                                                                                                                                                                                                                                                                                                                                                            (__toplevel_cons
                                                                                                                                                                                                                                                                                                                                                                                                               "*"
                                                                                                                                                                                                                                                                                                                                                                                                               (__toplevel_cons
                                                                                                                                                                                                                                                                                                                                                                                                                  "*"
                                                                                                                                                                                                                                                                                                                                                                                                                  (__toplevel_cons
                                                                                                                                                                                                                                                                                                                                                                                                                     " "
                                                                                                                                                                                                                                                                                                                                                                                                                     (__toplevel_cons
                                                                                                                                                                                                                                                                                                                                                                                                                        " "
                                                                                                                                                                                                                                                                                                                                                                                                                        (__toplevel_cons
                                                                                                                                                                                                                                                                                                                                                                                                                           " "
                                                                                                                                                                                                                                                                                                                                                                                                                           (__toplevel_cons
                                                                                                                                                                                                                                                                                                                                                                                                                              " "
                                                                                                                                                                                                                                                                                                                                                                                                                              (__toplevel_cons
                                                                                                                                                                                                                                                                                                                                                                                                                                 " "
                                                                                                                                                                                                                                                                                                                                                                                                                                 (__toplevel_cons
                                                                                                                                                                                                                                                                                                                                                                                                                                    'newline
                                                                                                                                                                                                                                                                                                                                                                                                                                    (__toplevel_cons
                                                                                                                                                                                                                                                                                                                                                                                                                                       "*"
                                                                                                                                                                                                                                                                                                                                                                                                                                       (__toplevel_cons
                                                                                                                                                                                                                                                                                                                                                                                                                                          "*"
                                                                                                                                                                                                                                                                                                                                                                                                                                          (__toplevel_cons
                                                                                                                                                                                                                                                                                                                                                                                                                                             "*"
                                                                                                                                                                                                                                                                                                                                                                                                                                             (__toplevel_cons
                                                                                                                                                                                                                                                                                                                                                                                                                                                "*"
                                                                                                                                                                                                                                                                                                                                                                                                                                                (__toplevel_cons
                                                                                                                                                                                                                                                                                                                                                                                                                                                   "*"
                                                                                                                                                                                                                                                                                                                                                                                                                                                   (__toplevel_cons
                                                                                                                                                                                                                                                                                                                                                                                                                                                      "*"
                                                                                                                                                                                                                                                                                                                                                                                                                                                      (__toplevel_cons
                                                                                                                                                                                                                                                                                                                                                                                                                                                         "*"
                                                                                                                                                                                                                                                                                                                                                                                                                                                         (__toplevel_cons
                                                                                                                                                                                                                                                                                                                                                                                                                                                            " "
                                                                                                                                                                                                                                                                                                                                                                                                                                                            (__toplevel_cons
                                                                                                                                                                                                                                                                                                                                                                                                                                                               " "
                                                                                                                                                                                                                                                                                                                                                                                                                                                               (__toplevel_cons
                                                                                                                                                                                                                                                                                                                                                                                                                                                                  " "
                                                                                                                                                                                                                                                                                                                                                                                                                                                                  (__toplevel_cons
                                                                                                                                                                                                                                                                                                                                                                                                                                                                     " "
                                                                                                                                                                                                                                                                                                                                                                                                                                                                     (__toplevel_cons
                                                                                                                                                                                                                                                                                                                                                                                                                                                                        " "
                                                                                                                                                                                                                                                                                                                                                                                                                                                                        (__toplevel_cons
                                                                                                                                                                                                                                                                                                                                                                                                                                                                           " "
                                                                                                                                                                                                                                                                                                                                                                                                                                                                           (__toplevel_cons
                                                                                                                                                                                                                                                                                                                                                                                                                                                                              'newline
                                                                                                                                                                                                                                                                                                                                                                                                                                                                              (__toplevel_cons
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 "*"
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 (__toplevel_cons
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    "*"
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    (__toplevel_cons
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       "*"
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       (__toplevel_cons
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          "*"
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          (__toplevel_cons
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             "*"
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             (__toplevel_cons
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                " "
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                (__toplevel_cons
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   " "
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   (__toplevel_cons
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      " "
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      (__toplevel_cons
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         " "
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         (__toplevel_cons
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            " "
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            (__toplevel_cons
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               " "
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               (__toplevel_cons
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  " "
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  (__toplevel_cons
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     'newline
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     (__toplevel_cons
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        "*"
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        (__toplevel_cons
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           "*"
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           (__toplevel_cons
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              "*"
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              (__toplevel_cons
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 " "
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 (__toplevel_cons
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    " "
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    (__toplevel_cons
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       " "
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       (__toplevel_cons
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          " "
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          (__toplevel_cons
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             " "
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             (__toplevel_cons
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                " "
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                (__toplevel_cons
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   " "
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   (__toplevel_cons
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      " "
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      (__toplevel_cons
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         'newline
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         (__toplevel_cons
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            "*"
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            (__toplevel_cons
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               " "
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               (__toplevel_cons
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  " "
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  (__toplevel_cons
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     " "
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     (__toplevel_cons
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        " "
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        (__toplevel_cons
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           " "
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           (__toplevel_cons " " (__toplevel_cons " " (__toplevel_cons " " (__toplevel_cons " " ())))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
 
