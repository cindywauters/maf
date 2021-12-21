;; renamed lambdas/lets: 1
 
(define result ())
 
(define output (<change>
      (lambda (i)
         (set! result (cons i result)))
      (lambda (_i0)
         (set! result (cons _i0 result)))))
 
(define kw-lijst (lambda (lst)
      (define loop (lambda (l)
            (let ((rest (cdr l))
                  (n (list (* (car l) (car l)))))
               (set-cdr! l n)
               (set-cdr! n rest)
               (if (not (eq? rest lst)) (loop rest) #f))))
      (loop lst)
      lst))
 
(define print-ring (lambda (r)
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
      #t))
 
(define last-cons (cons 3 ()))
 
(define test-lst (cons 1 (cons 4 last-cons)))
 
(set-cdr! last-cons test-lst)
 
(print-ring (kw-lijst test-lst))
 
(equal?
   result
   (__toplevel_cons
      "..."
      (__toplevel_cons
         9
         (__toplevel_cons
            " "
            (__toplevel_cons
               3
               (__toplevel_cons
                  " "
                  (__toplevel_cons
                     16
                     (__toplevel_cons
                        " "
                        (__toplevel_cons
                           4
                           (__toplevel_cons
                              " "
                              (__toplevel_cons 1 (__toplevel_cons " " (__toplevel_cons 1 (__toplevel_cons " " ()))))))))))))))
 
