;; renamed lambdas/lets: 6
 
(define result ())
 
(define display2 (lambda (item)
      (set! result (cons item result))))
 
(define newline2 (<change>
      (lambda ()
         (set! result (cons 'newline result)))
      (lambda ()
         (set! result (cons 'newline result)))))
 
(define make-row (<change>
      (lambda (key name age wage)
         (vector key name age wage))
      (lambda (_key0 _name0 _age0 _wage0)
         (vector _key0 _name0 _age0 _wage0))))
 
(define key-ref (lambda (row)
      (vector-ref row 0)))
 
(define name-ref (lambda (row)
      (vector-ref row 1)))
 
(define age-ref (lambda (row)
      (vector-ref row 2)))
 
(define wage-ref (lambda (row)
      (vector-ref row 3)))
 
(define key-set! (lambda (row value)
      (vector-set! row 0 value)))
 
(define name-set! (lambda (row value)
      (vector-set! row 1 value)))
 
(define age-set! (<change>
      (lambda (row value)
         (vector-set! row 2 value))
      (lambda (_row0 _value0)
         (vector-set! _row0 2 _value0))))
 
(define wage-set! (lambda (row value)
      (vector-set! row 3 value)))
 
(define show-row (<change>
      (lambda (row)
         (display2 "[Sleutel:")
         (display2 (key-ref row))
         (display2 "]")
         (display2 "[Naam:")
         (display2 (name-ref row))
         (display2 "]")
         (display2 "[Leeftijd:")
         (display2 (age-ref row))
         (display2 "]")
         (display2 "[Salaris:")
         (display2 (wage-ref row))
         (display2 "]"))
      (lambda (_row0)
         (display2 "[Sleutel:")
         (display2 (key-ref _row0))
         (display2 "]")
         (display2 "[Naam:")
         (display2 (name-ref _row0))
         (display2 "]")
         (display2 "[Leeftijd:")
         (display2 (age-ref _row0))
         (display2 "]")
         (display2 "[Salaris:")
         (display2 (wage-ref _row0))
         (display2 "]"))))
 
(define make-table (<change>
      (lambda (rows)
         (make-vector rows 0))
      (lambda (_rows0)
         (make-vector _rows0 0))))
 
(define table-size (lambda (table)
      (vector-length table)))
 
(define row-ref (lambda (table pos)
      (if (< pos (table-size table))
         (vector-ref table pos)
         #f)))
 
(define row-set! (lambda (table pos row)
      (if (< pos (table-size table))
         (vector-set! table pos row)
         #f)))
 
(define show-table (<change>
      (lambda (table)
         (define iter (lambda (index)
               (if (= index (table-size table))
                  (newline2)
                  (begin
                     (show-row (row-ref table index))
                     (newline2)
                     (iter (+ index 1))))))
         (iter 0))
      (lambda (_table0)
         (define iter (lambda (_index0)
               (if (= _index0 (table-size _table0))
                  (newline2)
                  (begin
                     (show-row (row-ref _table0 _index0))
                     (newline2)
                     (iter (+ _index0 1))))))
         (iter 0))))
 
(define table (make-table 10))
 
(row-set! table 0 (make-row 8 'Bernard 45 120000))
 
(row-set! table 1 (make-row 3 'Dirk 26 93000))
 
(row-set! table 2 (make-row 6 'George 48 130000))
 
(row-set! table 3 (make-row 6 'Greet 27 75000))
 
(row-set! table 4 (make-row 1 'Kaat 18 69000))
 
(row-set! table 5 (make-row 5 'Mauranne 21 69000))
 
(row-set! table 6 (make-row 4 'Peter 33 80000))
 
(row-set! table 7 (make-row 2 'Piet 25 96000))
 
(row-set! table 8 (make-row 9 'Tom 26 96000))
 
(row-set! table 9 (make-row 6 'Veronique 36 115000))
 
(define expected-result (__toplevel_cons
      'newline
      (__toplevel_cons
         'newline
         (__toplevel_cons
            "]"
            (__toplevel_cons
               115000
               (__toplevel_cons
                  "[Salaris:"
                  (__toplevel_cons
                     "]"
                     (__toplevel_cons
                        36
                        (__toplevel_cons
                           "[Leeftijd:"
                           (__toplevel_cons
                              "]"
                              (__toplevel_cons
                                 'Veronique
                                 (__toplevel_cons
                                    "[Naam:"
                                    (__toplevel_cons
                                       "]"
                                       (__toplevel_cons
                                          6
                                          (__toplevel_cons
                                             "[Sleutel:"
                                             (__toplevel_cons
                                                'newline
                                                (__toplevel_cons
                                                   "]"
                                                   (__toplevel_cons
                                                      96000
                                                      (__toplevel_cons
                                                         "[Salaris:"
                                                         (__toplevel_cons
                                                            "]"
                                                            (__toplevel_cons
                                                               26
                                                               (__toplevel_cons
                                                                  "[Leeftijd:"
                                                                  (__toplevel_cons
                                                                     "]"
                                                                     (__toplevel_cons
                                                                        'Tom
                                                                        (__toplevel_cons
                                                                           "[Naam:"
                                                                           (__toplevel_cons
                                                                              "]"
                                                                              (__toplevel_cons
                                                                                 9
                                                                                 (__toplevel_cons
                                                                                    "[Sleutel:"
                                                                                    (__toplevel_cons
                                                                                       'newline
                                                                                       (__toplevel_cons
                                                                                          "]"
                                                                                          (__toplevel_cons
                                                                                             96000
                                                                                             (__toplevel_cons
                                                                                                "[Salaris:"
                                                                                                (__toplevel_cons
                                                                                                   "]"
                                                                                                   (__toplevel_cons
                                                                                                      25
                                                                                                      (__toplevel_cons
                                                                                                         "[Leeftijd:"
                                                                                                         (__toplevel_cons
                                                                                                            "]"
                                                                                                            (__toplevel_cons
                                                                                                               'Piet
                                                                                                               (__toplevel_cons
                                                                                                                  "[Naam:"
                                                                                                                  (__toplevel_cons
                                                                                                                     "]"
                                                                                                                     (__toplevel_cons
                                                                                                                        2
                                                                                                                        (__toplevel_cons
                                                                                                                           "[Sleutel:"
                                                                                                                           (__toplevel_cons
                                                                                                                              'newline
                                                                                                                              (__toplevel_cons
                                                                                                                                 "]"
                                                                                                                                 (__toplevel_cons
                                                                                                                                    80000
                                                                                                                                    (__toplevel_cons
                                                                                                                                       "[Salaris:"
                                                                                                                                       (__toplevel_cons
                                                                                                                                          "]"
                                                                                                                                          (__toplevel_cons
                                                                                                                                             33
                                                                                                                                             (__toplevel_cons
                                                                                                                                                "[Leeftijd:"
                                                                                                                                                (__toplevel_cons
                                                                                                                                                   "]"
                                                                                                                                                   (__toplevel_cons
                                                                                                                                                      'Peter
                                                                                                                                                      (__toplevel_cons
                                                                                                                                                         "[Naam:"
                                                                                                                                                         (__toplevel_cons
                                                                                                                                                            "]"
                                                                                                                                                            (__toplevel_cons
                                                                                                                                                               4
                                                                                                                                                               (__toplevel_cons
                                                                                                                                                                  "[Sleutel:"
                                                                                                                                                                  (__toplevel_cons
                                                                                                                                                                     'newline
                                                                                                                                                                     (__toplevel_cons
                                                                                                                                                                        "]"
                                                                                                                                                                        (__toplevel_cons
                                                                                                                                                                           69000
                                                                                                                                                                           (__toplevel_cons
                                                                                                                                                                              "[Salaris:"
                                                                                                                                                                              (__toplevel_cons
                                                                                                                                                                                 "]"
                                                                                                                                                                                 (__toplevel_cons
                                                                                                                                                                                    21
                                                                                                                                                                                    (__toplevel_cons
                                                                                                                                                                                       "[Leeftijd:"
                                                                                                                                                                                       (__toplevel_cons
                                                                                                                                                                                          "]"
                                                                                                                                                                                          (__toplevel_cons
                                                                                                                                                                                             'Mauranne
                                                                                                                                                                                             (__toplevel_cons
                                                                                                                                                                                                "[Naam:"
                                                                                                                                                                                                (__toplevel_cons
                                                                                                                                                                                                   "]"
                                                                                                                                                                                                   (__toplevel_cons
                                                                                                                                                                                                      5
                                                                                                                                                                                                      (__toplevel_cons
                                                                                                                                                                                                         "[Sleutel:"
                                                                                                                                                                                                         (__toplevel_cons
                                                                                                                                                                                                            'newline
                                                                                                                                                                                                            (__toplevel_cons
                                                                                                                                                                                                               "]"
                                                                                                                                                                                                               (__toplevel_cons
                                                                                                                                                                                                                  69000
                                                                                                                                                                                                                  (__toplevel_cons
                                                                                                                                                                                                                     "[Salaris:"
                                                                                                                                                                                                                     (__toplevel_cons
                                                                                                                                                                                                                        "]"
                                                                                                                                                                                                                        (__toplevel_cons
                                                                                                                                                                                                                           18
                                                                                                                                                                                                                           (__toplevel_cons
                                                                                                                                                                                                                              "[Leeftijd:"
                                                                                                                                                                                                                              (__toplevel_cons
                                                                                                                                                                                                                                 "]"
                                                                                                                                                                                                                                 (__toplevel_cons
                                                                                                                                                                                                                                    'Kaat
                                                                                                                                                                                                                                    (__toplevel_cons
                                                                                                                                                                                                                                       "[Naam:"
                                                                                                                                                                                                                                       (__toplevel_cons
                                                                                                                                                                                                                                          "]"
                                                                                                                                                                                                                                          (__toplevel_cons
                                                                                                                                                                                                                                             1
                                                                                                                                                                                                                                             (__toplevel_cons
                                                                                                                                                                                                                                                "[Sleutel:"
                                                                                                                                                                                                                                                (__toplevel_cons
                                                                                                                                                                                                                                                   'newline
                                                                                                                                                                                                                                                   (__toplevel_cons
                                                                                                                                                                                                                                                      "]"
                                                                                                                                                                                                                                                      (__toplevel_cons
                                                                                                                                                                                                                                                         75000
                                                                                                                                                                                                                                                         (__toplevel_cons
                                                                                                                                                                                                                                                            "[Salaris:"
                                                                                                                                                                                                                                                            (__toplevel_cons
                                                                                                                                                                                                                                                               "]"
                                                                                                                                                                                                                                                               (__toplevel_cons
                                                                                                                                                                                                                                                                  27
                                                                                                                                                                                                                                                                  (__toplevel_cons
                                                                                                                                                                                                                                                                     "[Leeftijd:"
                                                                                                                                                                                                                                                                     (__toplevel_cons
                                                                                                                                                                                                                                                                        "]"
                                                                                                                                                                                                                                                                        (__toplevel_cons
                                                                                                                                                                                                                                                                           'Greet
                                                                                                                                                                                                                                                                           (__toplevel_cons
                                                                                                                                                                                                                                                                              "[Naam:"
                                                                                                                                                                                                                                                                              (__toplevel_cons
                                                                                                                                                                                                                                                                                 "]"
                                                                                                                                                                                                                                                                                 (__toplevel_cons
                                                                                                                                                                                                                                                                                    6
                                                                                                                                                                                                                                                                                    (__toplevel_cons
                                                                                                                                                                                                                                                                                       "[Sleutel:"
                                                                                                                                                                                                                                                                                       (__toplevel_cons
                                                                                                                                                                                                                                                                                          'newline
                                                                                                                                                                                                                                                                                          (__toplevel_cons
                                                                                                                                                                                                                                                                                             "]"
                                                                                                                                                                                                                                                                                             (__toplevel_cons
                                                                                                                                                                                                                                                                                                130000
                                                                                                                                                                                                                                                                                                (__toplevel_cons
                                                                                                                                                                                                                                                                                                   "[Salaris:"
                                                                                                                                                                                                                                                                                                   (__toplevel_cons
                                                                                                                                                                                                                                                                                                      "]"
                                                                                                                                                                                                                                                                                                      (__toplevel_cons
                                                                                                                                                                                                                                                                                                         48
                                                                                                                                                                                                                                                                                                         (__toplevel_cons
                                                                                                                                                                                                                                                                                                            "[Leeftijd:"
                                                                                                                                                                                                                                                                                                            (__toplevel_cons
                                                                                                                                                                                                                                                                                                               "]"
                                                                                                                                                                                                                                                                                                               (__toplevel_cons
                                                                                                                                                                                                                                                                                                                  'George
                                                                                                                                                                                                                                                                                                                  (__toplevel_cons
                                                                                                                                                                                                                                                                                                                     "[Naam:"
                                                                                                                                                                                                                                                                                                                     (__toplevel_cons
                                                                                                                                                                                                                                                                                                                        "]"
                                                                                                                                                                                                                                                                                                                        (__toplevel_cons
                                                                                                                                                                                                                                                                                                                           6
                                                                                                                                                                                                                                                                                                                           (__toplevel_cons
                                                                                                                                                                                                                                                                                                                              "[Sleutel:"
                                                                                                                                                                                                                                                                                                                              (__toplevel_cons
                                                                                                                                                                                                                                                                                                                                 'newline
                                                                                                                                                                                                                                                                                                                                 (__toplevel_cons
                                                                                                                                                                                                                                                                                                                                    "]"
                                                                                                                                                                                                                                                                                                                                    (__toplevel_cons
                                                                                                                                                                                                                                                                                                                                       93000
                                                                                                                                                                                                                                                                                                                                       (__toplevel_cons
                                                                                                                                                                                                                                                                                                                                          "[Salaris:"
                                                                                                                                                                                                                                                                                                                                          (__toplevel_cons
                                                                                                                                                                                                                                                                                                                                             "]"
                                                                                                                                                                                                                                                                                                                                             (__toplevel_cons
                                                                                                                                                                                                                                                                                                                                                26
                                                                                                                                                                                                                                                                                                                                                (__toplevel_cons
                                                                                                                                                                                                                                                                                                                                                   "[Leeftijd:"
                                                                                                                                                                                                                                                                                                                                                   (__toplevel_cons
                                                                                                                                                                                                                                                                                                                                                      "]"
                                                                                                                                                                                                                                                                                                                                                      (__toplevel_cons
                                                                                                                                                                                                                                                                                                                                                         'Dirk
                                                                                                                                                                                                                                                                                                                                                         (__toplevel_cons
                                                                                                                                                                                                                                                                                                                                                            "[Naam:"
                                                                                                                                                                                                                                                                                                                                                            (__toplevel_cons
                                                                                                                                                                                                                                                                                                                                                               "]"
                                                                                                                                                                                                                                                                                                                                                               (__toplevel_cons
                                                                                                                                                                                                                                                                                                                                                                  3
                                                                                                                                                                                                                                                                                                                                                                  (__toplevel_cons
                                                                                                                                                                                                                                                                                                                                                                     "[Sleutel:"
                                                                                                                                                                                                                                                                                                                                                                     (__toplevel_cons
                                                                                                                                                                                                                                                                                                                                                                        'newline
                                                                                                                                                                                                                                                                                                                                                                        (__toplevel_cons
                                                                                                                                                                                                                                                                                                                                                                           "]"
                                                                                                                                                                                                                                                                                                                                                                           (__toplevel_cons
                                                                                                                                                                                                                                                                                                                                                                              120000
                                                                                                                                                                                                                                                                                                                                                                              (__toplevel_cons
                                                                                                                                                                                                                                                                                                                                                                                 "[Salaris:"
                                                                                                                                                                                                                                                                                                                                                                                 (__toplevel_cons
                                                                                                                                                                                                                                                                                                                                                                                    "]"
                                                                                                                                                                                                                                                                                                                                                                                    (__toplevel_cons
                                                                                                                                                                                                                                                                                                                                                                                       45
                                                                                                                                                                                                                                                                                                                                                                                       (__toplevel_cons
                                                                                                                                                                                                                                                                                                                                                                                          "[Leeftijd:"
                                                                                                                                                                                                                                                                                                                                                                                          (__toplevel_cons
                                                                                                                                                                                                                                                                                                                                                                                             "]"
                                                                                                                                                                                                                                                                                                                                                                                             (__toplevel_cons
                                                                                                                                                                                                                                                                                                                                                                                                'Bernard
                                                                                                                                                                                                                                                                                                                                                                                                (__toplevel_cons
                                                                                                                                                                                                                                                                                                                                                                                                   "[Naam:"
                                                                                                                                                                                                                                                                                                                                                                                                   (__toplevel_cons "]" (__toplevel_cons 8 (__toplevel_cons "[Sleutel:" ()))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
 
(show-table table)
 
(equal? expected-result result)
 
