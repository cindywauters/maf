;; renamed lambdas/lets: 3
 
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
 
(define leap-year2? (lambda (year)
      (if (divides? 400 year)
         #t
         (if (divides? 100 year)
            #f
            (if (divides? 4 year) #t #f)))))
 
(define leap-year3? (lambda (year)
      (if (divides? 400 year)
         #t
         (if (divides? 100 year) #f (divides? 4 year)))))
 
(define leap-year4? (lambda (year)
      (<change>
         (let ((__or_res (divides? 400 year)))
            (if __or_res
               __or_res
               (if (divides? 4 year)
                  (not (divides? 100 year))
                  #f)))
         (let ((___or_res0 (divides? 400 year)))
            (if ___or_res0
               ___or_res0
               (if (divides? 4 year)
                  (not (divides? 100 year))
                  #f))))))
 
(if (not (let ((__or_res (leap-year? 1989))) (if __or_res __or_res (leap-year? 1900))))
   (if (leap-year? 2000)
      (if (= -1 (sign -5))
         (if (= 1 (sign 1.728000e+01)) (= 0 (sign 0)) #f)
         #f)
      #f)
   #f)
 
