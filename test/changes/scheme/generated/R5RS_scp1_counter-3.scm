; Changes:
; * removed: 0
; * added: 2
; * swaps: 0
; * negated predicates: 0
(letrec ((result ())
         (output (lambda (i)
                   (set! result (cons i result))))
         (count1 (lambda (x)
                   (if (= 0 x)
                      (display x)
                      (begin
                         (display x)
                         (count1 (- x 1))))))
         (count2 (lambda (x)
                   (<change>
                      ()
                      -)
                   (if (= 0 x)
                      (display x)
                      (begin
                         (count2 (- x 1))
                         (display x))))))
   (<change>
      ()
      (__toplevel_cons
         2
         (__toplevel_cons
            1
            (__toplevel_cons
               0
               (__toplevel_cons
                  0
                  (__toplevel_cons 1 (__toplevel_cons 2 (__toplevel_cons 3 (__toplevel_cons 4 ())))))))))
   (count1 4)
   (count2 4)
   (equal?
      result
      (__toplevel_cons
         4
         (__toplevel_cons
            3
            (__toplevel_cons
               2
               (__toplevel_cons
                  1
                  (__toplevel_cons
                     0
                     (__toplevel_cons
                        0
                        (__toplevel_cons 1 (__toplevel_cons 2 (__toplevel_cons 3 (__toplevel_cons 4 ()))))))))))))