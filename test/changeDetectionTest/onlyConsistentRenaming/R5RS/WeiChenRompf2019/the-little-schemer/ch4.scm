;; renamed lambdas/lets: 12
 
(define atom? (lambda (x)
      (if (not (pair? x)) (not (null? x)) #f)))
 
(define add1 (lambda (x)
      (+ x 1)))
 
(define sub1 (<change>
      (lambda (x)
         (- x 1))
      (lambda (_x0)
         (- _x0 1))))
 
(define o+ (<change>
      (lambda (n m)
         (if (zero? m) n (add1 (o+ n (sub1 m)))))
      (lambda (_n0 _m0)
         (if (zero? _m0) _n0 (add1 (o+ _n0 (sub1 _m0)))))))
 
(o+ 3 2)
 
(add1 (o+ 3 1))
 
(add1 (add1 (o+ 3 0)))
 
(add1 (add1 3))
 
(define o- (<change>
      (lambda (n m)
         (if (zero? m) n (sub1 (o- n (sub1 m)))))
      (lambda (_n0 _m0)
         (if (zero? _m0) _n0 (sub1 (o- _n0 (sub1 _m0)))))))
 
(define addtup (<change>
      (lambda (tup)
         (if (null? tup)
            0
            (o+ (car tup) (addtup (cdr tup)))))
      (lambda (_tup0)
         (if (null? _tup0)
            0
            (o+ (car _tup0) (addtup (cdr _tup0)))))))
 
(addtup (__toplevel_cons 1 (__toplevel_cons 2 (__toplevel_cons 3 ()))))
 
(o+ 1 (addtup (__toplevel_cons 2 (__toplevel_cons 3 ()))))
 
(o+ 1 (o+ 2 (addtup (__toplevel_cons 3 ()))))
 
(o+ 1 (o+ 2 (o+ 3 (addtup ()))))
 
(define o* (<change>
      (lambda (n m)
         (if (zero? m) 0 (o+ n (o* n (sub1 m)))))
      (lambda (_n0 _m0)
         (if (zero? _m0) 0 (o+ _n0 (o* _n0 (sub1 _m0)))))))
 
(o* 5 3)
 
(o+ 5 (o* 5 (sub1 3)))
 
(o+ 5 (o* 5 2))
 
(o+ 5 (o+ 5 (o* 5 (sub1 2))))
 
(o+ 5 (o+ 5 (o* 5 1)))
 
(o+ 5 (o+ 5 (o+ 5 (o* 5 (sub1 1)))))
 
(o+ 5 (o+ 5 (o+ 5 0)))
 
(define tup+ (lambda (tup1 tup2)
      (if (null? tup1)
         tup2
         (if (null? tup2)
            tup1
            (cons (+ (car tup1) (car tup2)) (tup+ (cdr tup1) (cdr tup2)))))))
 
(tup+ (__toplevel_cons 3 (__toplevel_cons 7 ())) (__toplevel_cons 4 (__toplevel_cons 6 ())))
 
(define o> (lambda (n m)
      (if (zero? n)
         #f
         (if (zero? m) #t (o> (sub1 n) (sub1 m))))))
 
(define o< (<change>
      (lambda (n m)
         (if (zero? m)
            #f
            (if (zero? n) #t (o< (sub1 n) (sub1 m)))))
      (lambda (_n0 _m0)
         (if (zero? _m0)
            #f
            (if (zero? _n0) #t (o< (sub1 _n0) (sub1 _m0)))))))
 
(define o= (lambda (n m)
      (if (o> n m) #f (if (o< n m) #t #t))))
 
(define ^ (<change>
      (lambda (n m)
         (if (zero? m) 1 (o* n (^ n (sub1 m)))))
      (lambda (_n0 _m0)
         (if (zero? _m0) 1 (o* _n0 (^ _n0 (sub1 _m0)))))))
 
(^ 2 3)
 
(o* 2 (^ 2 (sub1 3)))
 
(o* 2 (^ 2 2))
 
(o* 2 (o* 2 (^ 2 (sub1 2))))
 
(o* 2 (o* 2 (^ 2 1)))
 
(o* 2 (o* 2 (o* 2 (^ 2 (sub1 1)))))
 
(o* 2 (o* 2 (o* 2 (^ 2 0))))
 
(o* 2 (o* 2 (o* 2 1)))
 
(define ??? (<change>
      (lambda (n m)
         (if (o< n m) 0 (add1 (??? (o- n m) m))))
      (lambda (_n0 _m0)
         (if (o< _n0 _m0) 0 (add1 (??? (o- _n0 _m0) _m0))))))
 
(define o/ (<change>
      (lambda (n m)
         (if (o< n m) 0 (add1 (o/ (o- n m) m))))
      (lambda (_n0 _m0)
         (if (o< _n0 _m0) 0 (add1 (o/ (o- _n0 _m0) _m0))))))
 
(o/ 7 2)
 
(add1 (o/ (o- 7 2) 2))
 
(add1 (o/ 5 2))
 
(add1 (add1 (o/ (o- 5 2) 2)))
 
(add1 (add1 (o/ 3 2)))
 
(add1 (add1 (add1 (o/ (o- 3 2) 2))))
 
(add1 (add1 (add1 (o/ 1 2))))
 
(add1 (add1 (add1 0)))
 
(define my-length (lambda (lat)
      (if (null? lat) 0 (add1 (length (cdr lat))))))
 
(define pick (lambda (n lat)
      (if (zero? (sub1 n))
         (car lat)
         (pick (sub1 n) (cdr lat)))))
 
(define no-nums (<change>
      (lambda (lat)
         (if (null? lat)
            ()
            (if (number? (car lat))
               (no-nums (cdr lat))
               (cons (car lat) (no-nums (cdr lat))))))
      (lambda (_lat0)
         (if (null? _lat0)
            ()
            (if (number? (car _lat0))
               (no-nums (cdr _lat0))
               (cons (car _lat0) (no-nums (cdr _lat0))))))))
 
(define all-nums (lambda (lat)
      (if (null? lat)
         ()
         (if (number? (car lat))
            (cons (car lat) (all-nums (cdr lat)))
            (all-nums (cdr lat))))))
 
(define eqan? (<change>
      (lambda (a1 a2)
         (if (if (number? a1) (number? a2) #f)
            (= a1 a2)
            (if (let ((__or_res (number? a1))) (if __or_res __or_res (number? a2)))
               #f
               (eq? a1 a2))))
      (lambda (_a10 _a20)
         (if (if (number? _a10) (number? _a20) #f)
            (= _a10 _a20)
            (if (let ((___or_res0 (number? _a10))) (if ___or_res0 ___or_res0 (number? _a20)))
               #f
               (eq? _a10 _a20))))))
 
(define occur (<change>
      (lambda (a lat)
         (if (null? lat)
            0
            (if (eqan? a (car lat))
               (add1 (occur a (cdr lat)))
               (occur a (cdr lat)))))
      (lambda (_a0 _lat0)
         (if (null? _lat0)
            0
            (if (eqan? _a0 (car _lat0))
               (add1 (occur _a0 (cdr _lat0)))
               (occur _a0 (cdr _lat0)))))))
 
(define one? (lambda (n)
      (= n 1)))
 
