;; renamed lambdas/lets: 4
 
(define create-x (<change>
      (lambda (n)
         (define result (make-vector n 0))
         (letrec ((__do_loop (lambda (i)
                               (if (>= i n)
                                  result
                                  (begin
                                     (vector-set! result i i)
                                     (__do_loop (+ i 1)))))))
            (__do_loop 0)))
      (lambda (_n0)
         (define result (make-vector _n0 0))
         (letrec ((___do_loop0 (lambda (_i0)
                                 (if (>= _i0 _n0)
                                    result
                                    (begin
                                       (vector-set! result _i0 _i0)
                                       (___do_loop0 (+ _i0 1)))))))
            (___do_loop0 0)))))
 
(define create-y (lambda (x)
      (let* ((n (vector-length x))
             (result (make-vector n 0)))
         (<change>
            (letrec ((__do_loop (lambda (i)
                                  (if (< i 0)
                                     result
                                     (begin
                                        (vector-set! result i (vector-ref x i))
                                        (__do_loop (- i 1)))))))
               (__do_loop (- n 1)))
            (letrec ((___do_loop0 (lambda (_i0)
                                    (if (< _i0 0)
                                       result
                                       (begin
                                          (vector-set! result _i0 (vector-ref x _i0))
                                          (___do_loop0 (- _i0 1)))))))
               (___do_loop0 (- n 1)))))))
 
(define my-try (<change>
      (lambda (n)
         (vector-length (create-y (create-x n))))
      (lambda (_n0)
         (vector-length (create-y (create-x _n0))))))
 
(define go (lambda (n)
      ((<change>
         (letrec ((loop (lambda (repeat result)
                          (if (> repeat 0)
                             (loop (- repeat 1) (my-try n))
                             result))))
            loop)
         (letrec ((_loop0 (lambda (_repeat0 _result0)
                            (if (> _repeat0 0)
                               (_loop0 (- _repeat0 1) (my-try n))
                               _result0))))
            _loop0))
         100
         ())))
 
(= 200 (go 200))
 
