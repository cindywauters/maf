;; renamed lambdas/lets: 2
 
(define double (lambda (x)
      (+ x x)))
 
(define halve (<change>
      (lambda (x)
         (/ x 2))
      (lambda (_x0)
         (/ _x0 2))))
 
(define rec-fast-multiply (lambda (a b)
      (if (zero? b)
         0
         (if (even? b)
            (rec-fast-multiply (double a) (halve b))
            (+ a (rec-fast-multiply a (- b 1)))))))
 
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
 
