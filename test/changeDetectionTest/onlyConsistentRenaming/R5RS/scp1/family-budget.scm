;; renamed lambdas/lets: 4
 
(define familieboom (__toplevel_cons
      'jan
      (__toplevel_cons
         (__toplevel_cons
            'piet
            (__toplevel_cons
               (__toplevel_cons
                  'frans
                  (__toplevel_cons (__toplevel_cons 'tom ()) (__toplevel_cons (__toplevel_cons 'roel ()) ())))
               (__toplevel_cons (__toplevel_cons 'mie ()) ())))
         (__toplevel_cons
            (__toplevel_cons
               'bram
               (__toplevel_cons
                  (__toplevel_cons
                     'inge
                     (__toplevel_cons
                        (__toplevel_cons
                           'bert
                           (__toplevel_cons (__toplevel_cons 'ina ()) (__toplevel_cons (__toplevel_cons 'ilse ()) ())))
                        (__toplevel_cons (__toplevel_cons 'bart ()) ())))
                  (__toplevel_cons (__toplevel_cons 'iris ()) ())))
            (__toplevel_cons
               (__toplevel_cons
                  'joost
                  (__toplevel_cons (__toplevel_cons 'else (__toplevel_cons (__toplevel_cons 'ilse ()) ())) ()))
               ())))))
 
(define familiehoofd (lambda (fam)
      (car fam)))
 
(define kinderen (lambda (fam)
      (cdr fam)))
 
(define laatste-nakomeling? (<change>
      (lambda (fam)
         (null? (kinderen fam)))
      (lambda (_fam0)
         (null? (kinderen _fam0)))))
 
(define verdeel-democratisch (<change>
      (lambda (boom budget)
         (define verdeel (lambda (boom)
               (if (laatste-nakomeling? boom)
                  1
                  (+ 1 (verdeel-in (kinderen boom))))))
         (define verdeel-in (lambda (lst)
               (if (null? lst)
                  0
                  (+ (verdeel (car lst)) (verdeel-in (cdr lst))))))
         (/ budget (verdeel-in (kinderen boom))))
      (lambda (_boom0 _budget0)
         (define verdeel (lambda (_boom1)
               (if (laatste-nakomeling? _boom1)
                  1
                  (+ 1 (verdeel-in (kinderen _boom1))))))
         (define verdeel-in (lambda (_lst0)
               (if (null? _lst0)
                  0
                  (+ (verdeel (car _lst0)) (verdeel-in (cdr _lst0))))))
         (/ _budget0 (verdeel-in (kinderen _boom0))))))
 
(define budget (lambda (boom budget-list)
      (define budget-hulp (lambda (boom budget-list)
            (+ (car budget-list) (budget-hulp-in (kinderen boom) (cdr budget-list)))))
      (define budget-hulp-in (<change>
            (lambda (bomen budget-list)
               (if (let ((__or_res (null? bomen))) (if __or_res __or_res (null? budget-list)))
                  0
                  (+ (budget-hulp (car bomen) budget-list) (budget-hulp-in (cdr bomen) budget-list))))
            (lambda (_bomen0 _budget-list0)
               (if (let ((___or_res0 (null? _bomen0))) (if ___or_res0 ___or_res0 (null? _budget-list0)))
                  0
                  (+ (budget-hulp (car _bomen0) _budget-list0) (budget-hulp-in (cdr _bomen0) _budget-list0))))))
      (budget-hulp-in (kinderen boom) budget-list)))
 
(define verdeel (lambda (boom budget)
      (if (laatste-nakomeling? boom)
         (list (list (familiehoofd boom) budget))
         (let* ((rest (kinderen boom))
                (new-budget (/ budget (length rest))))
            (verdeel-in rest new-budget)))))
 
(define verdeel-in (<change>
      (lambda (bomen budget)
         (if (null? bomen)
            ()
            (append (verdeel (car bomen) budget) (verdeel-in (cdr bomen) budget))))
      (lambda (_bomen0 _budget0)
         (if (null? _bomen0)
            ()
            (append (verdeel (car _bomen0) _budget0) (verdeel-in (cdr _bomen0) _budget0))))))
 
(if (= (verdeel-democratisch familieboom 1500) 100)
   (if (= (budget familieboom (__toplevel_cons 100 (__toplevel_cons 50 (__toplevel_cons 20 ())))) 650)
      (equal?
         (verdeel familieboom 3000)
         (__toplevel_cons
            (__toplevel_cons 'tom (__toplevel_cons 250 ()))
            (__toplevel_cons
               (__toplevel_cons 'roel (__toplevel_cons 250 ()))
               (__toplevel_cons
                  (__toplevel_cons 'mie (__toplevel_cons 500 ()))
                  (__toplevel_cons
                     (__toplevel_cons 'ina (__toplevel_cons 125 ()))
                     (__toplevel_cons
                        (__toplevel_cons 'ilse (__toplevel_cons 125 ()))
                        (__toplevel_cons
                           (__toplevel_cons 'bart (__toplevel_cons 250 ()))
                           (__toplevel_cons
                              (__toplevel_cons 'iris (__toplevel_cons 500 ()))
                              (__toplevel_cons (__toplevel_cons 'ilse (__toplevel_cons 1000 ())) ())))))))))
      #f)
   #f)
 
