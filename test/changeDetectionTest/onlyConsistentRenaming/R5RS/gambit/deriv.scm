;; renamed lambdas/lets: 1
 
(define deriv (<change>
      (lambda (a)
         (if (not (pair? a))
            (if (eq? a 'x) 1 0)
            (if (eq? (car a) '+)
               (cons '+ (map deriv (cdr a)))
               (if (eq? (car a) '-)
                  (cons '- (map deriv (cdr a)))
                  (if (eq? (car a) '*)
                     (list '* a (cons '+ (map (lambda (a) (list '/ (deriv a) a)) (cdr a))))
                     (if (eq? (car a) '/)
                        (list
                           '-
                           (list '/ (deriv (cadr a)) (caddr a))
                           (list '/ (cadr a) (list '* (caddr a) (caddr a) (deriv (caddr a)))))
                        (error "No derivation method available")))))))
      (lambda (_a0)
         (if (not (pair? _a0))
            (if (eq? _a0 'x) 1 0)
            (if (eq? (car _a0) '+)
               (cons '+ (map deriv (cdr _a0)))
               (if (eq? (car _a0) '-)
                  (cons '- (map deriv (cdr _a0)))
                  (if (eq? (car _a0) '*)
                     (list '* _a0 (cons '+ (map (lambda (_a1) (list '/ (deriv _a1) _a1)) (cdr _a0))))
                     (if (eq? (car _a0) '/)
                        (list
                           '-
                           (list '/ (deriv (cadr _a0)) (caddr _a0))
                           (list '/ (cadr _a0) (list '* (caddr _a0) (caddr _a0) (deriv (caddr _a0)))))
                        (error "No derivation method available")))))))))
 
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
 
