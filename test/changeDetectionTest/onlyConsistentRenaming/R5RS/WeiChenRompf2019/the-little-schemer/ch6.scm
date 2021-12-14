;; renamed lambdas/lets: 2
 
(define atom? (<change>
      (lambda (x)
         (if (not (pair? x)) (not (null? x)) #f))
      (lambda (_x0)
         (if (not (pair? _x0)) (not (null? _x0)) #f))))
 
(define numbered? (lambda (aexp)
      (if (atom? aexp)
         (number? aexp)
         (if (numbered? (car aexp))
            (numbered? (car (cdr (cdr aexp))))
            #f))))
 
(define ^ (lambda (n m)
      (if (zero? m) 1 (* n (^ n (- m 1))))))
 
(define value (<change>
      (lambda (nexp)
         (if (atom? nexp)
            nexp
            (if (eq? (car (cdr nexp)) '+)
               (+ (value (car nexp)) (value (car (cdr (cdr nexp)))))
               (if (eq? (car (cdr nexp)) '*)
                  (* (value (car nexp)) (value (car (cdr (cdr nexp)))))
                  (^ (value (car nexp)) (value (car (cdr (cdr nexp)))))))))
      (lambda (_nexp0)
         (if (atom? _nexp0)
            _nexp0
            (if (eq? (car (cdr _nexp0)) '+)
               (+ (value (car _nexp0)) (value (car (cdr (cdr _nexp0)))))
               (if (eq? (car (cdr _nexp0)) '*)
                  (* (value (car _nexp0)) (value (car (cdr (cdr _nexp0)))))
                  (^ (value (car _nexp0)) (value (car (cdr (cdr _nexp0)))))))))))
 
(value
   (__toplevel_cons
      (__toplevel_cons 5 (__toplevel_cons '^ (__toplevel_cons 1 ())))
      (__toplevel_cons
         '*
         (__toplevel_cons (__toplevel_cons 3 (__toplevel_cons '+ (__toplevel_cons 3 ()))) ()))))
 
