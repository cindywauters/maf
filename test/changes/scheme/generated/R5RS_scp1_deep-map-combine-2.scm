; Changes:
; * removed: 0
; * added: 0
; * swaps: 0
; * negated predicates: 1
(letrec ((atom? (lambda (x)
                  (not (pair? x))))
         (deep-combine (lambda (combiner null-value l)
                         (if (null? l)
                            null-value
                            (if (atom? l)
                               l
                               (combiner (deep-combine combiner null-value (car l)) (deep-combine combiner null-value (cdr l)))))))
         (deep-map (lambda (f l)
                     (if (<change> (null? l) (not (null? l)))
                        ()
                        (if (atom? l)
                           (f l)
                           (cons (deep-map f (car l)) (deep-map f (cdr l))))))))
   (if (= (deep-combine + 0 (__toplevel_cons (__toplevel_cons (__toplevel_cons (__toplevel_cons 1 (__toplevel_cons 2 ())) (__toplevel_cons (__toplevel_cons 3 (__toplevel_cons 4 ())) ())) (__toplevel_cons (__toplevel_cons (__toplevel_cons 5 (__toplevel_cons 6 ())) (__toplevel_cons (__toplevel_cons 7 (__toplevel_cons 8 ())) ())) ())) (__toplevel_cons 9 ()))) 45)
      (equal?
         (deep-map
            (lambda (x)
               (* x x))
            (__toplevel_cons
               (__toplevel_cons
                  (__toplevel_cons
                     (__toplevel_cons 1 2)
                     (__toplevel_cons (__toplevel_cons 3 (__toplevel_cons 4 ())) ()))
                  (__toplevel_cons
                     (__toplevel_cons
                        (__toplevel_cons 5 (__toplevel_cons 6 ()))
                        (__toplevel_cons (__toplevel_cons 7 (__toplevel_cons 8 ())) ()))
                     ()))
               9))
         (__toplevel_cons
            (__toplevel_cons
               (__toplevel_cons
                  (__toplevel_cons 1 4)
                  (__toplevel_cons (__toplevel_cons 9 (__toplevel_cons 16 ())) ()))
               (__toplevel_cons
                  (__toplevel_cons
                     (__toplevel_cons 25 (__toplevel_cons 36 ()))
                     (__toplevel_cons (__toplevel_cons 49 (__toplevel_cons 64 ())) ()))
                  ()))
            81))
      #f))