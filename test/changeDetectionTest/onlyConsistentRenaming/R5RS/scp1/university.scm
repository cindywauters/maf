;; renamed lambdas/lets: 11
 
(define result ())
 
(define display2 (<change>
      (lambda (i)
         (set! result (cons i result)))
      (lambda (_i0)
         (set! result (cons _i0 result)))))
 
(define newline2 (<change>
      (lambda ()
         (set! result (cons 'newline result)))
      (lambda ()
         (set! result (cons 'newline result)))))
 
(define VUBOrganigram (__toplevel_cons
      'VUB
      (__toplevel_cons
         (__toplevel_cons
            'academisch
            (__toplevel_cons
               (__toplevel_cons 'rectoraat ())
               (__toplevel_cons
                  (__toplevel_cons
                     'faculteiten
                     (__toplevel_cons
                        (__toplevel_cons
                           'rechten
                           (__toplevel_cons
                              (__toplevel_cons
                                 'bachelor
                                 (__toplevel_cons
                                    (__toplevel_cons 'ba-rechten ())
                                    (__toplevel_cons (__toplevel_cons 'ba-criminologie ()) ())))
                              (__toplevel_cons
                                 (__toplevel_cons
                                    'master
                                    (__toplevel_cons
                                       (__toplevel_cons 'ma-rechten ())
                                       (__toplevel_cons (__toplevel_cons 'ma-criminologie ()) ())))
                                 ())))
                        (__toplevel_cons
                           (__toplevel_cons 'economie ())
                           (__toplevel_cons
                              (__toplevel_cons
                                 'wetenschappen
                                 (__toplevel_cons
                                    (__toplevel_cons
                                       'bachelor
                                       (__toplevel_cons
                                          (__toplevel_cons 'ba-wiskunde ())
                                          (__toplevel_cons (__toplevel_cons 'ba-fysica ()) (__toplevel_cons (__toplevel_cons 'ba-cw ()) ()))))
                                    (__toplevel_cons
                                       (__toplevel_cons
                                          'master
                                          (__toplevel_cons
                                             (__toplevel_cons 'ma-wiskunde ())
                                             (__toplevel_cons (__toplevel_cons 'ma-fysica ()) (__toplevel_cons (__toplevel_cons 'ma-cw ()) ()))))
                                       ())))
                              ()))))
                  ())))
         (__toplevel_cons
            (__toplevel_cons
               'administratief
               (__toplevel_cons
                  (__toplevel_cons 'personeel ())
                  (__toplevel_cons (__toplevel_cons 'financien ()) ())))
            ()))))
 
(define display-n (<change>
      (lambda (n d)
         (if (> n 0)
            (begin
               (display2 d)
               (display-n (- n 1) d))
            #f))
      (lambda (_n0 _d0)
         (if (> _n0 0)
            (begin
               (display2 _d0)
               (display-n (- _n0 1) _d0))
            #f))))
 
(define print-lijn (<change>
      (lambda (aantalblanco tekst)
         (display-n aantalblanco " ")
         (display2 tekst)
         (newline2))
      (lambda (_aantalblanco0 _tekst0)
         (display-n _aantalblanco0 " ")
         (display2 _tekst0)
         (newline2))))
 
(define label (<change>
      (lambda (organigram)
         (car organigram))
      (lambda (_organigram0)
         (car _organigram0))))
 
(define takken (<change>
      (lambda (organigram)
         (cdr organigram))
      (lambda (_organigram0)
         (cdr _organigram0))))
 
(define organigram-member-in (lambda (een-label organigrammen)
      (if (null? organigrammen)
         #f
         (<change>
            (let ((__or_res (organigram-member een-label (car organigrammen))))
               (if __or_res
                  __or_res
                  (organigram-member-in een-label (cdr organigrammen))))
            (let ((___or_res0 (organigram-member een-label (car organigrammen))))
               (if ___or_res0
                  ___or_res0
                  (organigram-member-in een-label (cdr organigrammen))))))))
 
(define organigram-member (<change>
      (lambda (een-label organigram)
         (if (eq? een-label (label organigram))
            organigram
            (organigram-member-in een-label (takken organigram))))
      (lambda (_een-label0 _organigram0)
         (if (eq? _een-label0 (label _organigram0))
            _organigram0
            (organigram-member-in _een-label0 (takken _organigram0))))))
 
(define print (<change>
      (lambda (organigram)
         (define print (lambda (diepte organigram)
               (print-lijn diepte (label organigram))
               (for-each (lambda (organigram) (print (+ diepte 1) organigram)) (takken organigram))))
         (print 0 organigram))
      (lambda (_organigram0)
         (define print (lambda (_diepte0 _organigram1)
               (print-lijn _diepte0 (label _organigram1))
               (for-each (lambda (_organigram2) (print (+ _diepte0 1) _organigram2)) (takken _organigram1))))
         (print 0 _organigram0))))
 
(define print-vanaf (<change>
      (lambda (organigram label)
         (let ((res (organigram-member label organigram)))
            (if res (print res) #f)))
      (lambda (_organigram0 _label0)
         (let ((_res0 (organigram-member _label0 _organigram0)))
            (if _res0 (print _res0) #f)))))
 
(print-vanaf VUBOrganigram 'rechten)
 
(define print-tot (<change>
      (lambda (organigram niveau)
         (define print-tot (lambda (organigram niveau max-niveau)
               (if (<= niveau max-niveau)
                  (begin
                     (print-lijn niveau (label organigram))
                     (for-each (lambda (organigram) (print-tot organigram (+ niveau 1) max-niveau)) (takken organigram)))
                  #f)))
         (print-tot organigram 0 niveau))
      (lambda (_organigram0 _niveau0)
         (define print-tot (lambda (_organigram1 _niveau1 _max-niveau0)
               (if (<= _niveau1 _max-niveau0)
                  (begin
                     (print-lijn _niveau1 (label _organigram1))
                     (for-each
                        (lambda (_organigram2)
                           (print-tot _organigram2 (+ _niveau1 1) _max-niveau0))
                        (takken _organigram1)))
                  #f)))
         (print-tot _organigram0 0 _niveau0))))
 
(print-tot VUBOrganigram 2)
 
(equal?
   result
   (__toplevel_cons
      'newline
      (__toplevel_cons
         'financien
         (__toplevel_cons
            " "
            (__toplevel_cons
               " "
               (__toplevel_cons
                  'newline
                  (__toplevel_cons
                     'personeel
                     (__toplevel_cons
                        " "
                        (__toplevel_cons
                           " "
                           (__toplevel_cons
                              'newline
                              (__toplevel_cons
                                 'administratief
                                 (__toplevel_cons
                                    " "
                                    (__toplevel_cons
                                       'newline
                                       (__toplevel_cons
                                          'faculteiten
                                          (__toplevel_cons
                                             " "
                                             (__toplevel_cons
                                                " "
                                                (__toplevel_cons
                                                   'newline
                                                   (__toplevel_cons
                                                      'rectoraat
                                                      (__toplevel_cons
                                                         " "
                                                         (__toplevel_cons
                                                            " "
                                                            (__toplevel_cons
                                                               'newline
                                                               (__toplevel_cons
                                                                  'academisch
                                                                  (__toplevel_cons
                                                                     " "
                                                                     (__toplevel_cons
                                                                        'newline
                                                                        (__toplevel_cons
                                                                           'VUB
                                                                           (__toplevel_cons
                                                                              'newline
                                                                              (__toplevel_cons
                                                                                 'ma-criminologie
                                                                                 (__toplevel_cons
                                                                                    " "
                                                                                    (__toplevel_cons
                                                                                       " "
                                                                                       (__toplevel_cons
                                                                                          'newline
                                                                                          (__toplevel_cons
                                                                                             'ma-rechten
                                                                                             (__toplevel_cons
                                                                                                " "
                                                                                                (__toplevel_cons
                                                                                                   " "
                                                                                                   (__toplevel_cons
                                                                                                      'newline
                                                                                                      (__toplevel_cons
                                                                                                         'master
                                                                                                         (__toplevel_cons
                                                                                                            " "
                                                                                                            (__toplevel_cons
                                                                                                               'newline
                                                                                                               (__toplevel_cons
                                                                                                                  'ba-criminologie
                                                                                                                  (__toplevel_cons
                                                                                                                     " "
                                                                                                                     (__toplevel_cons
                                                                                                                        " "
                                                                                                                        (__toplevel_cons
                                                                                                                           'newline
                                                                                                                           (__toplevel_cons
                                                                                                                              'ba-rechten
                                                                                                                              (__toplevel_cons
                                                                                                                                 " "
                                                                                                                                 (__toplevel_cons
                                                                                                                                    " "
                                                                                                                                    (__toplevel_cons
                                                                                                                                       'newline
                                                                                                                                       (__toplevel_cons
                                                                                                                                          'bachelor
                                                                                                                                          (__toplevel_cons " " (__toplevel_cons 'newline (__toplevel_cons 'rechten ())))))))))))))))))))))))))))))))))))))))))))))))))
 
