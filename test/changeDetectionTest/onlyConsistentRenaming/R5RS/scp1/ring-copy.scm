;; renamed lambdas/lets: 3
 
(define result ())
 
(define output (lambda (i)
      (set! result (cons i result))))
 
(define make-ring (lambda (n)
      (<change>
         (let ((last (cons 0 ())))
            (define build-list (lambda (n)
                  (if (= n 0) last (cons n (build-list (- n 1))))))
            (let ((ring (build-list n)))
               (set-cdr! last ring)
               ring))
         (let ((_last0 (cons 0 ())))
            (define build-list (lambda (_n0)
                  (if (= _n0 0)
                     _last0
                     (cons _n0 (build-list (- _n0 1))))))
            (let ((_ring0 (build-list n)))
               (set-cdr! _last0 _ring0)
               _ring0)))))
 
(define print-ring (<change>
      (lambda (r)
         (define aux (lambda (l)
               (if (not (null? l))
                  (if (eq? (cdr l) r)
                     (begin
                        (display " ")
                        (display (car l))
                        (display "..."))
                     (begin
                        (display " ")
                        (display (car l))
                        (aux (cdr l))))
                  #f)))
         (aux r)
         #t)
      (lambda (_r0)
         (define aux (lambda (_l0)
               (if (not (null? _l0))
                  (if (eq? (cdr _l0) _r0)
                     (begin
                        (display " ")
                        (display (car _l0))
                        (display "..."))
                     (begin
                        (display " ")
                        (display (car _l0))
                        (aux (cdr _l0))))
                  #f)))
         (aux _r0)
         #t)))
 
(define copy-ring (lambda (r)
      (define last ())
      (define aux (lambda (l)
            (if (eq? (cdr l) r)
               (begin
                  (set! last (cons (car l) ()))
                  last)
               (cons (car l) (aux (cdr l))))))
      (<change>
         (let ((first (aux r)))
            (set-cdr! last first)
            first)
         (let ((_first0 (aux r)))
            (set-cdr! last _first0)
            _first0))))
 
(define r (make-ring 3))
 
(define s (copy-ring r))
 
(print-ring s)
 
(set-car! s 999)
 
(print-ring s)
 
(print-ring r)
 
(equal?
   result
   (__toplevel_cons
      "..."
      (__toplevel_cons
         0
         (__toplevel_cons
            " "
            (__toplevel_cons
               1
               (__toplevel_cons
                  " "
                  (__toplevel_cons
                     2
                     (__toplevel_cons
                        " "
                        (__toplevel_cons
                           3
                           (__toplevel_cons
                              " "
                              (__toplevel_cons
                                 "..."
                                 (__toplevel_cons
                                    0
                                    (__toplevel_cons
                                       " "
                                       (__toplevel_cons
                                          1
                                          (__toplevel_cons
                                             " "
                                             (__toplevel_cons
                                                2
                                                (__toplevel_cons
                                                   " "
                                                   (__toplevel_cons
                                                      999
                                                      (__toplevel_cons
                                                         " "
                                                         (__toplevel_cons
                                                            "..."
                                                            (__toplevel_cons
                                                               0
                                                               (__toplevel_cons
                                                                  " "
                                                                  (__toplevel_cons
                                                                     1
                                                                     (__toplevel_cons
                                                                        " "
                                                                        (__toplevel_cons 2 (__toplevel_cons " " (__toplevel_cons 3 (__toplevel_cons " " ()))))))))))))))))))))))))))))
 
