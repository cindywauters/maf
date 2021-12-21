;; renamed lambdas/lets: 1
 
(define show (lambda (namen punten test?)
      (if (null? namen)
         ()
         (let ((res (show (cdr namen) (cdr punten) test?)))
            (if (test? (car punten))
               (cons (car namen) res)
               res)))))
 
(define one (<change>
      (lambda (namen punten)
         (define één-buis? (lambda (punten)
               (if (null? punten)
                  #f
                  (let ((punt (car punten))
                        (rest (cdr punten)))
                     (if (< punt 10)
                        (geen-buis? rest)
                        (één-buis? rest))))))
         (define geen-buis? (lambda (punten)
               (if (null? punten)
                  #t
                  (let ((punt (car punten))
                        (rest (cdr punten)))
                     (if (< punt 10) #f (geen-buis? rest))))))
         (show namen punten één-buis?))
      (lambda (_namen0 _punten0)
         (define één-buis? (lambda (_punten1)
               (if (null? _punten1)
                  #f
                  (let ((_punt0 (car _punten1))
                        (_rest0 (cdr _punten1)))
                     (if (< _punt0 10)
                        (geen-buis? _rest0)
                        (één-buis? _rest0))))))
         (define geen-buis? (lambda (_punten2)
               (if (null? _punten2)
                  #t
                  (let ((_punt1 (car _punten2))
                        (_rest1 (cdr _punten2)))
                     (if (< _punt1 10) #f (geen-buis? _rest1))))))
         (show _namen0 _punten0 één-buis?))))
 
(equal?
   (one
      (__toplevel_cons
         'wendy
         (__toplevel_cons 'dirk (__toplevel_cons 'kris (__toplevel_cons 'jan (__toplevel_cons 'eef ())))))
      (__toplevel_cons
         (__toplevel_cons 12 (__toplevel_cons 13 (__toplevel_cons 15 (__toplevel_cons 18 ()))))
         (__toplevel_cons
            (__toplevel_cons 7 (__toplevel_cons 10 (__toplevel_cons 14 (__toplevel_cons 17 ()))))
            (__toplevel_cons
               (__toplevel_cons 13 (__toplevel_cons 8 (__toplevel_cons 7 (__toplevel_cons 11 ()))))
               (__toplevel_cons
                  (__toplevel_cons 9 (__toplevel_cons 12 (__toplevel_cons 11 (__toplevel_cons 10 ()))))
                  (__toplevel_cons
                     (__toplevel_cons 18 (__toplevel_cons 14 (__toplevel_cons 17 (__toplevel_cons 19 ()))))
                     ()))))))
   (__toplevel_cons 'dirk (__toplevel_cons 'jan ())))
 
