; Changes:
; * removed: 1
; * added: 1
; * swaps: 0
; * negated predicates: 0
; * swapped branches: 0
; * calls to id fun: 2
(letrec ((result ())
         (output (lambda (i)
                   (<change>
                      (set! result (cons i result))
                      ((lambda (x) x) (set! result (cons i result))))))
         (make-ring (lambda (n)
                      (let ((last (cons 0 ())))
                         (letrec ((build-list (lambda (n)
                                                (if (= n 0) last (cons n (build-list (- n 1)))))))
                            (let ((ring (build-list n)))
                               (<change>
                                  ()
                                  ring)
                               (set-cdr! last ring)
                               ring)))))
         (print-ring (lambda (r)
                       (letrec ((aux (lambda (l)
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
                                          #f))))
                          (aux r)
                          #t)))
         (copy-ring (lambda (r)
                      (letrec ((last ())
                               (aux (lambda (l)
                                      (if (eq? (cdr l) r)
                                         (begin
                                            (set! last (cons (car l) ()))
                                            last)
                                         (cons (car l) (aux (cdr l)))))))
                         (let ((first (aux r)))
                            (set-cdr! last first)
                            first))))
         (r (make-ring 3))
         (s (copy-ring r)))
   (<change>
      (print-ring s)
      ())
   (set-car! s 999)
   (print-ring s)
   (print-ring r)
   (<change>
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
      ((lambda (x) x)
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
                                                                                 (__toplevel_cons 2 (__toplevel_cons " " (__toplevel_cons 3 (__toplevel_cons " " ())))))))))))))))))))))))))))))))