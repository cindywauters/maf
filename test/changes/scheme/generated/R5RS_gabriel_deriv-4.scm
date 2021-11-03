; Changes:
; * removed: 1
; * added: 0
; * swaps: 1
; * negated predicates: 0
(letrec ((deriv (lambda (a)
                  (<change>
                     @sensitivity:FA
                     ())
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
                                       (lambda (a)
                                          (<change>
                                             @sensitivity:FA
                                             (list '/ (deriv a) a))
                                          (<change>
                                             (list '/ (deriv a) a)
                                             @sensitivity:FA))
                                       (cdr a))))
                              (if (eq? (car a) '/)
                                 (list
                                    '-
                                    (list '/ (deriv (cadr a)) (caddr a))
                                    (list '/ (cadr a) (list '* (caddr a) (caddr a) (deriv (caddr a)))))
                                 (error "No derivation method available"))))))))
         (result (deriv
                   (__toplevel_cons
                      '+
                      (__toplevel_cons
                         (__toplevel_cons '* (__toplevel_cons 3 (__toplevel_cons 'x (__toplevel_cons 'x ()))))
                         (__toplevel_cons
                            (__toplevel_cons '* (__toplevel_cons 'a (__toplevel_cons 'x (__toplevel_cons 'x ()))))
                            (__toplevel_cons
                               (__toplevel_cons '* (__toplevel_cons 'b (__toplevel_cons 'x ())))
                               (__toplevel_cons 5 ()))))))))
   (equal?
      result
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