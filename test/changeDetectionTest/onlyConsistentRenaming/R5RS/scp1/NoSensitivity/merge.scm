;; renamed lambdas/lets: 4
 
(define first-el (<change>
      (lambda (best)
         (if (not (null? best)) (caar best) #f))
      (lambda (_best0)
         (if (not (null? _best0)) (caar _best0) #f))))
 
(define smaller? (<change>
      (lambda (el1 el2)
         (string<? (symbol->string el1) (symbol->string el2)))
      (lambda (_el10 _el20)
         (string<? (symbol->string _el10) (symbol->string _el20)))))
 
(define same? (<change>
      (lambda (el1 el2)
         (equal? el1 el2))
      (lambda (_el10 _el20)
         (equal? _el10 _el20))))
 
(define merge (<change>
      (lambda (best1 best2)
         (define merge-in (lambda (curr1 curr2 prev)
               (if (null? curr1)
                  (set-cdr! prev curr2)
                  (if (null? curr2)
                     (set-cdr! prev curr1)
                     (if (same? (first-el curr1) (first-el curr2))
                        (begin
                           (set-cdr! prev curr1)
                           (merge-in (cdr curr1) (cdr curr2) curr1))
                        (if (smaller? (first-el curr1) (first-el curr2))
                           (begin
                              (set-cdr! prev curr1)
                              (merge-in (cdr curr1) curr2 curr1))
                           (begin
                              (set-cdr! prev curr2)
                              (merge-in curr1 (cdr curr2) curr2))))))))
         (let* ((result (if (smaller? (first-el best1) (first-el best2))
                          best1
                          best2))
                (curr1 (if (eq? result best1) (cdr best1) best1))
                (curr2 (if (eq? result best2) (cdr best2) best2)))
            (merge-in curr1 curr2 result)
            result))
      (lambda (_best10 _best20)
         (define merge-in (lambda (_curr10 _curr20 _prev0)
               (if (null? _curr10)
                  (set-cdr! _prev0 _curr20)
                  (if (null? _curr20)
                     (set-cdr! _prev0 _curr10)
                     (if (same? (first-el _curr10) (first-el _curr20))
                        (begin
                           (set-cdr! _prev0 _curr10)
                           (merge-in (cdr _curr10) (cdr _curr20) _curr10))
                        (if (smaller? (first-el _curr10) (first-el _curr20))
                           (begin
                              (set-cdr! _prev0 _curr10)
                              (merge-in (cdr _curr10) _curr20 _curr10))
                           (begin
                              (set-cdr! _prev0 _curr20)
                              (merge-in _curr10 (cdr _curr20) _curr20))))))))
         (let* ((_result0 (if (smaller? (first-el _best10) (first-el _best20))
                            _best10
                            _best20))
                (_curr11 (if (eq? _result0 _best10) (cdr _best10) _best10))
                (_curr21 (if (eq? _result0 _best20) (cdr _best20) _best20)))
            (merge-in _curr11 _curr21 _result0)
            _result0))))
 
(define best1 (__toplevel_cons
      (__toplevel_cons
         'ann
         (__toplevel_cons
            (__toplevel_cons
               'meiboomstraat
               (__toplevel_cons 12 (__toplevel_cons 1820 (__toplevel_cons 'Eppegem ()))))
            ()))
      (__toplevel_cons
         (__toplevel_cons
            'bert
            (__toplevel_cons
               (__toplevel_cons
                  'populierendreef
                  (__toplevel_cons 7 (__toplevel_cons 1050 (__toplevel_cons 'Brussel ()))))
               ()))
         (__toplevel_cons
            (__toplevel_cons
               'kurt
               (__toplevel_cons
                  (__toplevel_cons
                     'Mechelsesteenweg
                     (__toplevel_cons 50 (__toplevel_cons 1800 (__toplevel_cons 'Vilvoorde ()))))
                  ()))
            ()))))
 
(define best2 (__toplevel_cons
      (__toplevel_cons
         'bert
         (__toplevel_cons
            (__toplevel_cons
               'populierendreef
               (__toplevel_cons 7 (__toplevel_cons 1050 (__toplevel_cons 'Brussel ()))))
            ()))
      (__toplevel_cons
         (__toplevel_cons
            'jan
            (__toplevel_cons
               (__toplevel_cons 'eikestraat (__toplevel_cons 1 (__toplevel_cons 9000 (__toplevel_cons 'Gent ()))))
               ()))
         (__toplevel_cons
            (__toplevel_cons
               'sofie
               (__toplevel_cons
                  (__toplevel_cons
                     'boerendreef
                     (__toplevel_cons 5 (__toplevel_cons 2800 (__toplevel_cons 'Mechelen ()))))
                  ()))
            ()))))
 
(equal?
   (merge best1 best2)
   (__toplevel_cons
      (__toplevel_cons
         'ann
         (__toplevel_cons
            (__toplevel_cons
               'meiboomstraat
               (__toplevel_cons 12 (__toplevel_cons 1820 (__toplevel_cons 'Eppegem ()))))
            ()))
      (__toplevel_cons
         (__toplevel_cons
            'bert
            (__toplevel_cons
               (__toplevel_cons
                  'populierendreef
                  (__toplevel_cons 7 (__toplevel_cons 1050 (__toplevel_cons 'Brussel ()))))
               ()))
         (__toplevel_cons
            (__toplevel_cons
               'jan
               (__toplevel_cons
                  (__toplevel_cons 'eikestraat (__toplevel_cons 1 (__toplevel_cons 9000 (__toplevel_cons 'Gent ()))))
                  ()))
            (__toplevel_cons
               (__toplevel_cons
                  'kurt
                  (__toplevel_cons
                     (__toplevel_cons
                        'Mechelsesteenweg
                        (__toplevel_cons 50 (__toplevel_cons 1800 (__toplevel_cons 'Vilvoorde ()))))
                     ()))
               (__toplevel_cons
                  (__toplevel_cons
                     'sofie
                     (__toplevel_cons
                        (__toplevel_cons
                           'boerendreef
                           (__toplevel_cons 5 (__toplevel_cons 2800 (__toplevel_cons 'Mechelen ()))))
                        ()))
                  ()))))))
 
