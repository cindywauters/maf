;; renamed lambdas/lets: 1
;; Of which consistent renamings: 0

(define deriv (lambda (a)
      (if (not (pair? a))
         (if (eq? a 'x) 1 0)
         (if (eq? (car a) '+)
            (cons '+ (map deriv (cdr a)))
            (if (eq? (car a) '-)
               (cons '- (map deriv (cdr a)))
               (if (eq? (car a) '*)
                  (list
                     '*
                     a
                     (cons
                        '+
                        (map
                           (<change>
                              (lambda (a)
                                 (list '/ (deriv a) a))
                              (lambda (_a0)
                                 (list '+ (deriv _a0) _a0))) ;; NOT RENAMING: / -> +
                           (cdr a))))
                  (if (eq? (car a) '/)
                     (list
                        '-
                        (list '/ (deriv (cadr a)) (caddr a))
                        (list '/ (cadr a) (list '* (caddr a) (caddr a) (deriv (caddr a)))))
                     (error "No derivation method available"))))))))
 
(define res (equal?
      (deriv
         (__toplevel_cons
            '+
            (__toplevel_cons
               (__toplevel_cons '* (__toplevel_cons 3 (__toplevel_cons 'x (__toplevel_cons 'x ()))))
               (__toplevel_cons
                  (__toplevel_cons '* (__toplevel_cons 'a (__toplevel_cons 'x (__toplevel_cons 'x ()))))
                  (__toplevel_cons
                     (__toplevel_cons '* (__toplevel_cons 'b (__toplevel_cons 'x ())))
                     (__toplevel_cons 5 ()))))))
      (__toplevel_cons
         '+
         (__toplevel_cons
            (__toplevel_cons
               '*
               (__toplevel_cons
                  (__toplevel_cons '* (__toplevel_cons 3 (__toplevel_cons 'x (__toplevel_cons 'x ()))))
                  (__toplevel_cons
                     (__toplevel_cons
                        '+
                        (__toplevel_cons
                           (__toplevel_cons '/ (__toplevel_cons 0 (__toplevel_cons 3 ())))
                           (__toplevel_cons
                              (__toplevel_cons '/ (__toplevel_cons 1 (__toplevel_cons 'x ())))
                              (__toplevel_cons (__toplevel_cons '/ (__toplevel_cons 1 (__toplevel_cons 'x ()))) ()))))
                     ())))
            (__toplevel_cons
               (__toplevel_cons
                  '*
                  (__toplevel_cons
                     (__toplevel_cons '* (__toplevel_cons 'a (__toplevel_cons 'x (__toplevel_cons 'x ()))))
                     (__toplevel_cons
                        (__toplevel_cons
                           '+
                           (__toplevel_cons
                              (__toplevel_cons '/ (__toplevel_cons 0 (__toplevel_cons 'a ())))
                              (__toplevel_cons
                                 (__toplevel_cons '/ (__toplevel_cons 1 (__toplevel_cons 'x ())))
                                 (__toplevel_cons (__toplevel_cons '/ (__toplevel_cons 1 (__toplevel_cons 'x ()))) ()))))
                        ())))
               (__toplevel_cons
                  (__toplevel_cons
                     '*
                     (__toplevel_cons
                        (__toplevel_cons '* (__toplevel_cons 'b (__toplevel_cons 'x ())))
                        (__toplevel_cons
                           (__toplevel_cons
                              '+
                              (__toplevel_cons
                                 (__toplevel_cons '/ (__toplevel_cons 0 (__toplevel_cons 'b ())))
                                 (__toplevel_cons (__toplevel_cons '/ (__toplevel_cons 1 (__toplevel_cons 'x ()))) ())))
                           ())))
                  (__toplevel_cons 0 ())))))))
 
res
 
