;; renamed lambdas/lets: 7
 
(define result ())
 
(define output (<change>
      (lambda (i)
         (set! result (cons i result)))
      (lambda (_i0)
         (set! result (cons _i0 result)))))
 
(define make-ring (lambda (n)
      (let ((last (cons 0 ())))
         (define build-list (<change>
               (lambda (n)
                  (if (= n 0) last (cons n (build-list (- n 1)))))
               (lambda (_n0)
                  (if (= _n0 0)
                     last
                     (cons _n0 (build-list (- _n0 1)))))))
         (<change>
            (let ((ring (build-list n)))
               (set-cdr! last ring)
               ring)
            (let ((_ring0 (build-list n)))
               (set-cdr! last _ring0)
               _ring0)))))
 
(define print-ring (<change>
      (lambda (r)
         (define aux (lambda (l)
               (if (not (null? l))
                  (if (eq? (cdr l) r)
                     (begin
                        (output " ")
                        (output (car l))
                        (output "..."))
                     (begin
                        (output " ")
                        (output (car l))
                        (aux (cdr l))))
                  #f)))
         (aux r)
         #t)
      (lambda (_r0)
         (define aux (lambda (_l0)
               (if (not (null? _l0))
                  (if (eq? (cdr _l0) _r0)
                     (begin
                        (output " ")
                        (output (car _l0))
                        (output "..."))
                     (begin
                        (output " ")
                        (output (car _l0))
                        (aux (cdr _l0))))
                  #f)))
         (aux _r0)
         #t)))
 
(define copy-ring (<change>
      (lambda (r)
         (define last ())
         (define aux (lambda (l)
               (if (eq? (cdr l) r)
                  (begin
                     (set! last (cons (car l) ()))
                     last)
                  (cons (car l) (aux (cdr l))))))
         (let ((first (aux r)))
            (set-cdr! last first)
            first))
      (lambda (_r0)
         (define last ())
         (define aux (lambda (_l0)
               (if (eq? (cdr _l0) _r0)
                  (begin
                     (set! last (cons (car _l0) ()))
                     last)
                  (cons (car _l0) (aux (cdr _l0))))))
         (let ((_first0 (aux _r0)))
            (set-cdr! last _first0)
            _first0))))
 
(define right-rotate (lambda (r)
      (define iter (<change>
            (lambda (l)
               (if (eq? (cdr l) r) l (iter (cdr l))))
            (lambda (_l0)
               (if (eq? (cdr _l0) r) _l0 (iter (cdr _l0))))))
      (iter r)))
 
(define Josephus (lambda (r n)
      (define remove-nth! (<change>
            (lambda (l n)
               (if (<= n 2)
                  (begin
                     (set-cdr! l (cddr l))
                     (cdr l))
                  (remove-nth! (cdr l) (- n 1))))
            (lambda (_l0 _n0)
               (if (<= _n0 2)
                  (begin
                     (set-cdr! _l0 (cddr _l0))
                     (cdr _l0))
                  (remove-nth! (cdr _l0) (- _n0 1))))))
      (define iter (lambda (l)
            (print-ring l)
            (if (eq? l (cdr l))
               (car l)
               (iter (remove-nth! l n)))))
      (if (= n 1)
         (car (right-rotate r))
         (iter (copy-ring r)))))
 
(define ring (make-ring 5))
 
(Josephus ring 5)
 
(print-ring ring)
 
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
                                 4
                                 (__toplevel_cons
                                    " "
                                    (__toplevel_cons
                                       5
                                       (__toplevel_cons
                                          " "
                                          (__toplevel_cons
                                             "..."
                                             (__toplevel_cons
                                                5
                                                (__toplevel_cons
                                                   " "
                                                   (__toplevel_cons
                                                      "..."
                                                      (__toplevel_cons
                                                         5
                                                         (__toplevel_cons
                                                            " "
                                                            (__toplevel_cons
                                                               3
                                                               (__toplevel_cons
                                                                  " "
                                                                  (__toplevel_cons
                                                                     "..."
                                                                     (__toplevel_cons
                                                                        3
                                                                        (__toplevel_cons
                                                                           " "
                                                                           (__toplevel_cons
                                                                              4
                                                                              (__toplevel_cons
                                                                                 " "
                                                                                 (__toplevel_cons
                                                                                    5
                                                                                    (__toplevel_cons
                                                                                       " "
                                                                                       (__toplevel_cons
                                                                                          "..."
                                                                                          (__toplevel_cons
                                                                                             3
                                                                                             (__toplevel_cons
                                                                                                " "
                                                                                                (__toplevel_cons
                                                                                                   4
                                                                                                   (__toplevel_cons
                                                                                                      " "
                                                                                                      (__toplevel_cons
                                                                                                         5
                                                                                                         (__toplevel_cons
                                                                                                            " "
                                                                                                            (__toplevel_cons
                                                                                                               0
                                                                                                               (__toplevel_cons
                                                                                                                  " "
                                                                                                                  (__toplevel_cons
                                                                                                                     "..."
                                                                                                                     (__toplevel_cons
                                                                                                                        2
                                                                                                                        (__toplevel_cons
                                                                                                                           " "
                                                                                                                           (__toplevel_cons
                                                                                                                              3
                                                                                                                              (__toplevel_cons
                                                                                                                                 " "
                                                                                                                                 (__toplevel_cons
                                                                                                                                    4
                                                                                                                                    (__toplevel_cons
                                                                                                                                       " "
                                                                                                                                       (__toplevel_cons
                                                                                                                                          5
                                                                                                                                          (__toplevel_cons
                                                                                                                                             " "
                                                                                                                                             (__toplevel_cons
                                                                                                                                                0
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
                                                                                                                                                                           3
                                                                                                                                                                           (__toplevel_cons
                                                                                                                                                                              " "
                                                                                                                                                                              (__toplevel_cons 4 (__toplevel_cons " " (__toplevel_cons 5 (__toplevel_cons " " ()))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
 
