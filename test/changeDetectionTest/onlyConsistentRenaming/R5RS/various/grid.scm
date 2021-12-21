;; renamed lambdas/lets: 3
 
(define make-grid (<change>
      (lambda (start dims)
         (let ((v (make-vector (car dims) start)))
            (if (not (null? (cdr dims)))
               (letrec ((loop (lambda (i)
                                (if (>= i (car dims))
                                   #t
                                   (begin
                                      (vector-set! v i (make-grid start (cdr dims)))
                                      (loop (+ i 1)))))))
                  (loop 0))
               #t)
            v))
      (lambda (_start0 _dims0)
         (let ((_v0 (make-vector (car _dims0) _start0)))
            (if (not (null? (cdr _dims0)))
               (letrec ((_loop0 (lambda (_i0)
                                  (if (>= _i0 (car _dims0))
                                     #t
                                     (begin
                                        (vector-set! _v0 _i0 (make-grid _start0 (cdr _dims0)))
                                        (_loop0 (+ _i0 1)))))))
                  (_loop0 0))
               #t)
            _v0))))
 
(define grid-ref (<change>
      (lambda (g n)
         (if (null? (cdr n))
            (vector-ref g (car n))
            (grid-ref (vector-ref g (car n)) (cdr n))))
      (lambda (_g0 _n0)
         (if (null? (cdr _n0))
            (vector-ref _g0 (car _n0))
            (grid-ref (vector-ref _g0 (car _n0)) (cdr _n0))))))
 
(define grid-set! (<change>
      (lambda (g v n)
         (if (null? (cdr n))
            (vector-set! g (car n) v)
            (grid-set! (vector-ref g (car n)) v (cdr n))))
      (lambda (_g0 _v0 _n0)
         (if (null? (cdr _n0))
            (vector-set! _g0 (car _n0) _v0)
            (grid-set! (vector-ref _g0 (car _n0)) _v0 (cdr _n0))))))
 
(define t (make-grid 0 (__toplevel_cons 4 (__toplevel_cons 5 (__toplevel_cons 6 ())))))
 
(define u (make-grid #f (__toplevel_cons 2 (__toplevel_cons 2 ()))))
 
(if (equal? (grid-ref t (__toplevel_cons 2 (__toplevel_cons 2 (__toplevel_cons 3 ())))) 0)
   (if (begin (grid-set! t 24 (__toplevel_cons 2 (__toplevel_cons 2 (__toplevel_cons 3 ())))) (equal? (grid-ref t (__toplevel_cons 2 (__toplevel_cons 2 (__toplevel_cons 3 ())))) 24))
      (if (equal? (grid-ref t (__toplevel_cons 1 (__toplevel_cons 0 ()))) (make-vector 6 0))
         (begin
            (grid-set! t #t (__toplevel_cons 1 (__toplevel_cons 0 ())))
            (equal? (grid-ref t (__toplevel_cons 1 (__toplevel_cons 0 ()))) #t))
         #f)
      #f)
   #f)
 
