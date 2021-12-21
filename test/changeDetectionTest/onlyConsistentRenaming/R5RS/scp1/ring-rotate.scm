;; renamed lambdas/lets: 3
 
(define result ())
 
(define output (lambda (i)
      (set! result (cons i result))))
 
(define make-ring (<change>
      (lambda (n)
         (let ((last (cons 0 ())))
            (define build-list (lambda (n)
                  (if (= n 0) last (cons n (build-list (- n 1))))))
            (let ((ring (build-list n)))
               (set-cdr! last ring)
               ring)))
      (lambda (_n0)
         (let ((_last0 (cons 0 ())))
            (define build-list (lambda (_n1)
                  (if (= _n1 0)
                     _last0
                     (cons _n1 (build-list (- _n1 1))))))
            (let ((_ring0 (build-list _n0)))
               (set-cdr! _last0 _ring0)
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
 
(define right-rotate (<change>
      (lambda (r)
         (define iter (lambda (l)
               (if (eq? (cdr l) r) l (iter (cdr l)))))
         (iter r))
      (lambda (_r0)
         (define iter (lambda (_l0)
               (if (eq? (cdr _l0) _r0) _l0 (iter (cdr _l0)))))
         (iter _r0))))
 
(define r (make-ring 3))
 
(print-ring (right-rotate r))
 
(equal?
   result
   (__toplevel_cons
      "..."
      (__toplevel_cons
         1
         (__toplevel_cons
            " "
            (__toplevel_cons
               2
               (__toplevel_cons
                  " "
                  (__toplevel_cons 3 (__toplevel_cons " " (__toplevel_cons 0 (__toplevel_cons " " ()))))))))))
 
