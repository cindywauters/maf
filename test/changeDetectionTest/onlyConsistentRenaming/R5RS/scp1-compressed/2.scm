;; renamed lambdas/lets: 7
 
(define sign (lambda (number)
      (if (zero? number) 0 (if (> number 0) 1 -1))))
 
(define divides? (<change>
      (lambda (deler deeltal)
         (= 0 (modulo deeltal deler)))
      (lambda (_deler0 _deeltal0)
         (= 0 (modulo _deeltal0 _deler0)))))
 
(define leap-year? (<change>
      (lambda (year)
         (if (divides? 4 year)
            (if (divides? 100 year) (divides? 400 year) #t)
            #f))
      (lambda (_year0)
         (if (divides? 4 _year0)
            (if (divides? 100 _year0)
               (divides? 400 _year0)
               #t)
            #f))))
 
(define leap-year2? (<change>
      (lambda (year)
         (if (divides? 400 year)
            #t
            (if (divides? 100 year)
               #f
               (if (divides? 4 year) #t #f))))
      (lambda (_year0)
         (if (divides? 400 _year0)
            #t
            (if (divides? 100 _year0)
               #f
               (if (divides? 4 _year0) #t #f))))))
 
(define leap-year3? (<change>
      (lambda (year)
         (if (divides? 400 year)
            #t
            (if (divides? 100 year) #f (divides? 4 year))))
      (lambda (_year0)
         (if (divides? 400 _year0)
            #t
            (if (divides? 100 _year0) #f (divides? 4 _year0))))))
 
(define leap-year4? (<change>
      (lambda (year)
         (let ((__or_res (divides? 400 year)))
            (if __or_res
               __or_res
               (if (divides? 4 year)
                  (not (divides? 100 year))
                  #f))))
      (lambda (_year0)
         (let ((___or_res0 (divides? 400 _year0)))
            (if ___or_res0
               ___or_res0
               (if (divides? 4 _year0)
                  (not (divides? 100 _year0))
                  #f))))))
 
(if (not (<change> (let ((__or_res (leap-year? 1989))) (if __or_res __or_res (leap-year? 1900))) (let ((___or_res0 (leap-year? 1989))) (if ___or_res0 ___or_res0 (leap-year? 1900)))))
   (if (leap-year? 2000)
      (if (= -1 (sign -5))
         (if (= 1 (sign 1.728000e+01)) (= 0 (sign 0)) #f)
         #f)
      #f)
   #f)
 
(define derde-machtswortel (<change>
      (lambda (x)
         (define epsilon 1.000000e-02)
         (define hulp-derde-machtswortel (lambda (y)
               (if (< (abs (- (* y y y) x)) epsilon)
                  y
                  (hulp-derde-machtswortel (/ (+ (/ x (* y y)) y y) 3)))))
         (hulp-derde-machtswortel (/ x 3)))
      (lambda (_x0)
         (define epsilon 1.000000e-02)
         (define hulp-derde-machtswortel (lambda (_y0)
               (if (< (abs (- (* _y0 _y0 _y0) _x0)) epsilon)
                  _y0
                  (hulp-derde-machtswortel (/ (+ (/ _x0 (* _y0 _y0)) _y0 _y0) 3)))))
         (hulp-derde-machtswortel (/ _x0 3)))))
 
(= 3.000000e+00 (exact->inexact (derde-machtswortel 27)))
 
