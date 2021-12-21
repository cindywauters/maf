;; renamed lambdas/lets: 4
 
(define organigram (__toplevel_cons
      'directeur
      (__toplevel_cons
         (__toplevel_cons
            'hoofd-verkoop
            (__toplevel_cons
               (__toplevel_cons 'verkoopsleider-vlaanderen ())
               (__toplevel_cons (__toplevel_cons 'verkoopsleider-brussel ()) ())))
         (__toplevel_cons
            (__toplevel_cons
               'hoofd-productie
               (__toplevel_cons
                  (__toplevel_cons
                     'hoofd-inkoop
                     (__toplevel_cons
                        (__toplevel_cons 'bediende1 ())
                        (__toplevel_cons
                           (__toplevel_cons 'bediende2 ())
                           (__toplevel_cons (__toplevel_cons 'bediende3 ()) ()))))
                  (__toplevel_cons (__toplevel_cons 'hoofd-fakturen ()) ())))
            (__toplevel_cons
               (__toplevel_cons
                  'hoofd-administratie
                  (__toplevel_cons
                     (__toplevel_cons 'hoofd-personeel ())
                     (__toplevel_cons (__toplevel_cons 'hoofd-boekhouding ()) ())))
               ())))))
 
(define baas (<change>
      (lambda (organigram)
         (car organigram))
      (lambda (_organigram0)
         (car _organigram0))))
 
(define sub-organigrammen (<change>
      (lambda (organigram)
         (cdr organigram))
      (lambda (_organigram0)
         (cdr _organigram0))))
 
(define hierarchisch? (lambda (p1 p2 organigram)
      (define hierarchisch?-in (lambda (path organigrammen)
            (if (null? organigrammen)
               #f
               (let ((__or_res (hierarchisch? path (car organigrammen))))
                  (if __or_res
                     __or_res
                     (hierarchisch?-in path (cdr organigrammen)))))))
      (define hierarchisch? (<change>
            (lambda (path organigram)
               (if (if (eq? p1 (baas organigram)) (member p2 path) #f)
                  #t
                  (if (if (eq? p2 (baas organigram)) (member p1 path) #f)
                     #t
                     (hierarchisch?-in (cons (baas organigram) path) (sub-organigrammen organigram)))))
            (lambda (_path0 _organigram0)
               (if (if (eq? p1 (baas _organigram0)) (member p2 _path0) #f)
                  #t
                  (if (if (eq? p2 (baas _organigram0)) (member p1 _path0) #f)
                     #t
                     (hierarchisch?-in (cons (baas _organigram0) _path0) (sub-organigrammen _organigram0)))))))
      (hierarchisch? () organigram)))
 
(define collegas (<change>
      (lambda (p organigram)
         (define collegas-in (lambda (oversten organigrammen)
               (if (null? organigrammen)
                  #f
                  (let ((__or_res (collegas oversten (car organigrammen))))
                     (if __or_res
                        __or_res
                        (collegas-in oversten (cdr organigrammen)))))))
         (define werknemers-in (lambda (organigrammen)
               (if (null? organigrammen)
                  ()
                  (append (werknemers (car organigrammen)) (werknemers-in (cdr organigrammen))))))
         (define werknemers (lambda (organigram)
               (cons (baas organigram) (werknemers-in (sub-organigrammen organigram)))))
         (define collegas (lambda (oversten organigram)
               (if (eq? p (baas organigram))
                  (append oversten (werknemers-in (sub-organigrammen organigram)))
                  (collegas-in (cons (baas organigram) oversten) (sub-organigrammen organigram)))))
         (collegas () organigram))
      (lambda (_p0 _organigram0)
         (define collegas-in (lambda (_oversten0 _organigrammen0)
               (if (null? _organigrammen0)
                  #f
                  (let ((___or_res0 (collegas _oversten0 (car _organigrammen0))))
                     (if ___or_res0
                        ___or_res0
                        (collegas-in _oversten0 (cdr _organigrammen0)))))))
         (define werknemers-in (lambda (_organigrammen1)
               (if (null? _organigrammen1)
                  ()
                  (append (werknemers (car _organigrammen1)) (werknemers-in (cdr _organigrammen1))))))
         (define werknemers (lambda (_organigram1)
               (cons (baas _organigram1) (werknemers-in (sub-organigrammen _organigram1)))))
         (define collegas (lambda (_oversten1 _organigram2)
               (if (eq? _p0 (baas _organigram2))
                  (append _oversten1 (werknemers-in (sub-organigrammen _organigram2)))
                  (collegas-in (cons (baas _organigram2) _oversten1) (sub-organigrammen _organigram2)))))
         (collegas () _organigram0))))
 
(if (hierarchisch? 'directeur 'verkoopsleider-brussel organigram)
   (if (hierarchisch? 'bediende1 'hoofd-productie organigram)
      (if (not (hierarchisch? 'hoofd-personeel 'bediende3 organigram))
         (equal?
            (collegas 'hoofd-inkoop organigram)
            (__toplevel_cons
               'hoofd-productie
               (__toplevel_cons
                  'directeur
                  (__toplevel_cons 'bediende1 (__toplevel_cons 'bediende2 (__toplevel_cons 'bediende3 ()))))))
         #f)
      #f)
   #f)
 
