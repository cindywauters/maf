; Changes:
; * removed: 0
; * added: 3
; * swaps: 0
; * negated predicates: 2
(letrec ((find-last (lambda (lijst)
                      (if (null? lijst)
                         (error "find-last -- lijst heeft geen laatste element")
                         (let ((next (cdr lijst)))
                            (if (null? next) lijst (find-last next))))))
         (flatten! (lambda (lijst)
                     (if (null? lijst)
                        ()
                        (let* ((sublist (car lijst))
                               (restlist (flatten! (cdr lijst))))
                           (if (null? sublist)
                              restlist
                              (let ((last (find-last sublist)))
                                 (set-cdr! last restlist)
                                 (<change>
                                    ()
                                    last)
                                 sublist))))))
         (atom? (lambda (x)
                  (not (pair? x))))
         (flatten2! (lambda (lijst)
                      (let ((hulpcel (cons 'dummy lijst)))
                         (letrec ((flatten-aux! (lambda (prev current)
                                                  (if (<change> (null? current) (not (null? current)))
                                                     (cdr hulpcel)
                                                     (if (null? (car current))
                                                        (begin
                                                           (<change>
                                                              ()
                                                              (display set-cdr!))
                                                           (set-cdr! prev (cdr current))
                                                           (flatten-aux! prev (cdr current)))
                                                        (if (pair? (car current))
                                                           (begin
                                                              (set-cdr! prev (flatten2! (car current)))
                                                              (flatten-aux! (find-last prev) (cdr current)))
                                                           (if (null? (cdr prev))
                                                              (begin
                                                                 (set-cdr! prev current)
                                                                 (flatten-aux! (cdr prev) (cdr current)))
                                                              (if (<change> (atom? (car current)) (not (atom? (car current))))
                                                                 (flatten-aux! (cdr prev) (cdr current))
                                                                 #f))))))))
                            (flatten-aux! hulpcel lijst)
                            (<change>
                               ()
                               hulpcel)
                            (cdr hulpcel)))))
         (res (if (equal? (flatten! (__toplevel_cons (__toplevel_cons 1 (__toplevel_cons 2 ())) (__toplevel_cons (__toplevel_cons 3 (__toplevel_cons 4 (__toplevel_cons 5 ()))) (__toplevel_cons (__toplevel_cons 6 ()) (__toplevel_cons (__toplevel_cons 7 (__toplevel_cons 8 ())) ()))))) (__toplevel_cons 1 (__toplevel_cons 2 (__toplevel_cons 3 (__toplevel_cons 4 (__toplevel_cons 5 (__toplevel_cons 6 (__toplevel_cons 7 (__toplevel_cons 8 ())))))))))
                (if (equal? (flatten! (__toplevel_cons () (__toplevel_cons (__toplevel_cons 1 (__toplevel_cons 2 ())) (__toplevel_cons (__toplevel_cons 3 (__toplevel_cons 4 (__toplevel_cons 5 ()))) (__toplevel_cons () (__toplevel_cons (__toplevel_cons 6 ()) (__toplevel_cons (__toplevel_cons 7 (__toplevel_cons 8 ())) ()))))))) (__toplevel_cons 1 (__toplevel_cons 2 (__toplevel_cons 3 (__toplevel_cons 4 (__toplevel_cons 5 (__toplevel_cons 6 (__toplevel_cons 7 (__toplevel_cons 8 ())))))))))
                   (if (equal? (flatten2! (__toplevel_cons (__toplevel_cons 1 (__toplevel_cons (__toplevel_cons 2 (__toplevel_cons 3 ())) (__toplevel_cons 4 ()))) (__toplevel_cons 5 (__toplevel_cons 6 (__toplevel_cons (__toplevel_cons 7 (__toplevel_cons 8 ())) ()))))) (__toplevel_cons 1 (__toplevel_cons 2 (__toplevel_cons 3 (__toplevel_cons 4 (__toplevel_cons 5 (__toplevel_cons 6 (__toplevel_cons 7 (__toplevel_cons 8 ())))))))))
                      (if (equal? (flatten2! (__toplevel_cons (__toplevel_cons 1 (__toplevel_cons 2 ())) (__toplevel_cons (__toplevel_cons 3 (__toplevel_cons 4 (__toplevel_cons 5 ()))) (__toplevel_cons (__toplevel_cons 6 ()) (__toplevel_cons (__toplevel_cons 7 (__toplevel_cons 8 ())) ()))))) (__toplevel_cons 1 (__toplevel_cons 2 (__toplevel_cons 3 (__toplevel_cons 4 (__toplevel_cons 5 (__toplevel_cons 6 (__toplevel_cons 7 (__toplevel_cons 8 ())))))))))
                         (if (equal? (flatten2! (__toplevel_cons () (__toplevel_cons (__toplevel_cons 1 (__toplevel_cons 2 ())) (__toplevel_cons (__toplevel_cons 3 (__toplevel_cons 4 (__toplevel_cons 5 ()))) (__toplevel_cons () (__toplevel_cons (__toplevel_cons 6 ()) (__toplevel_cons (__toplevel_cons 7 (__toplevel_cons 8 ())) ()))))))) (__toplevel_cons 1 (__toplevel_cons 2 (__toplevel_cons 3 (__toplevel_cons 4 (__toplevel_cons 5 (__toplevel_cons 6 (__toplevel_cons 7 (__toplevel_cons 8 ())))))))))
                            (equal?
                               (flatten2!
                                  (__toplevel_cons
                                     1
                                     (__toplevel_cons
                                        2
                                        (__toplevel_cons
                                           (__toplevel_cons
                                              3
                                              (__toplevel_cons
                                                 (__toplevel_cons 4 (__toplevel_cons 5 ()))
                                                 (__toplevel_cons
                                                    6
                                                    (__toplevel_cons (__toplevel_cons 7 (__toplevel_cons 8 ())) (__toplevel_cons 9 ())))))
                                           (__toplevel_cons 10 ())))))
                               (__toplevel_cons
                                  1
                                  (__toplevel_cons
                                     2
                                     (__toplevel_cons
                                        3
                                        (__toplevel_cons
                                           4
                                           (__toplevel_cons
                                              5
                                              (__toplevel_cons
                                                 6
                                                 (__toplevel_cons 7 (__toplevel_cons 8 (__toplevel_cons 9 (__toplevel_cons 10 ())))))))))))
                            #f)
                         #f)
                      #f)
                   #f)
                #f)))
   res)