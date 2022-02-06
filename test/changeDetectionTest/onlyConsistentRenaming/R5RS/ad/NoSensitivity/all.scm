;; renamed lambdas/lets: 37
 
(define bubble-sort (lambda (vector)
      (define swap (<change>
            (lambda (vector index1 index2)
               (let ((temp (vector-ref vector index1)))
                  (vector-set! vector index1 (vector-ref vector index2))
                  (vector-set! vector index2 temp)))
            (lambda (_vector0 _index10 _index20)
               (let ((_temp0 (vector-ref _vector0 _index10)))
                  (vector-set! _vector0 _index10 (vector-ref _vector0 _index20))
                  (vector-set! _vector0 _index20 _temp0)))))
      (define bubble (<change>
            (lambda (index)
               (define bubble-iter (lambda (index1 changed)
                     (if (<= index1 index)
                        (begin
                           (if (> (vector-ref vector index1) (vector-ref vector (+ index1 1)))
                              (begin
                                 (swap vector index1 (+ index1 1))
                                 (set! changed #t))
                              #f)
                           (bubble-iter (+ index1 1) changed))
                        changed)))
               (bubble-iter 0 #f))
            (lambda (_index0)
               (define bubble-iter (lambda (_index10 _changed0)
                     (if (<= _index10 _index0)
                        (begin
                           (if (> (vector-ref vector _index10) (vector-ref vector (+ _index10 1)))
                              (begin
                                 (swap vector _index10 (+ _index10 1))
                                 (set! _changed0 #t))
                              #f)
                           (bubble-iter (+ _index10 1) _changed0))
                        _changed0)))
               (bubble-iter 0 #f))))
      (define bubble-sort-iter (<change>
            (lambda (index)
               (if (>= index 0)
                  (if (bubble index)
                     (bubble-sort-iter (- index 1))
                     #f)
                  #f))
            (lambda (_index0)
               (if (>= _index0 0)
                  (if (bubble _index0)
                     (bubble-sort-iter (- _index0 1))
                     #f)
                  #f))))
      (bubble-sort-iter (- (vector-length vector) 2))))
 
(define vect (vector 9 5 1 7 8 9 4 6 2 3))
 
(bubble-sort vect)
 
(equal? vect (vector 1 2 3 4 5 6 7 8 9 9))
 
(define selection-sort (<change>
      (lambda (vector)
         (define swap (lambda (vector index1 index2)
               (let ((temp (vector-ref vector index1)))
                  (vector-set! vector index1 (vector-ref vector index2))
                  (vector-set! vector index2 temp))))
         (define pos-of-min (lambda (vector low high)
               (define min-iter (lambda (index pos-of-min-so-far)
                     (if (<= index high)
                        (if (< (vector-ref vector index) (vector-ref vector pos-of-min-so-far))
                           (min-iter (+ index 1) index)
                           (min-iter (+ index 1) pos-of-min-so-far))
                        pos-of-min-so-far)))
               (min-iter (+ low 1) low)))
         (let ((high (- (vector-length vector) 1)))
            (define selection-sort-iter (lambda (index)
                  (if (< index high)
                     (begin
                        (swap vector index (pos-of-min vector index high))
                        (selection-sort-iter (+ index 1)))
                     #f)))
            (selection-sort-iter 0)))
      (lambda (_vector0)
         (define swap (lambda (_vector1 _index10 _index20)
               (let ((_temp0 (vector-ref _vector1 _index10)))
                  (vector-set! _vector1 _index10 (vector-ref _vector1 _index20))
                  (vector-set! _vector1 _index20 _temp0))))
         (define pos-of-min (lambda (_vector2 _low0 _high0)
               (define min-iter (lambda (_index0 _pos-of-min-so-far0)
                     (if (<= _index0 _high0)
                        (if (< (vector-ref _vector2 _index0) (vector-ref _vector2 _pos-of-min-so-far0))
                           (min-iter (+ _index0 1) _index0)
                           (min-iter (+ _index0 1) _pos-of-min-so-far0))
                        _pos-of-min-so-far0)))
               (min-iter (+ _low0 1) _low0)))
         (let ((_high1 (- (vector-length _vector0) 1)))
            (define selection-sort-iter (lambda (_index1)
                  (if (< _index1 _high1)
                     (begin
                        (swap _vector0 _index1 (pos-of-min _vector0 _index1 _high1))
                        (selection-sort-iter (+ _index1 1)))
                     #f)))
            (selection-sort-iter 0)))))
 
(define vect2 (vector 5 7 0 9 6 4 3 8 2 1))
 
(selection-sort vect2)
 
(equal? vect2 (vector 0 1 2 3 4 5 6 7 8 9))
 
(define result ())
 
(define display2 (lambda (item)
      (set! result (cons item result))))
 
(define newline2 (<change>
      (lambda ()
         (set! result (cons 'newline result)))
      (lambda ()
         (set! result (cons 'newline result)))))
 
(define make-row (lambda (key name age wage)
      (vector key name age wage)))
 
(define key-ref (<change>
      (lambda (row)
         (vector-ref row 0))
      (lambda (_row0)
         (vector-ref _row0 0))))
 
(define name-ref (<change>
      (lambda (row)
         (vector-ref row 1))
      (lambda (_row0)
         (vector-ref _row0 1))))
 
(define age-ref (lambda (row)
      (vector-ref row 2)))
 
(define wage-ref (<change>
      (lambda (row)
         (vector-ref row 3))
      (lambda (_row0)
         (vector-ref _row0 3))))
 
(define key-set! (<change>
      (lambda (row value)
         (vector-set! row 0 value))
      (lambda (_row0 _value0)
         (vector-set! _row0 0 _value0))))
 
(define name-set! (<change>
      (lambda (row value)
         (vector-set! row 1 value))
      (lambda (_row0 _value0)
         (vector-set! _row0 1 _value0))))
 
(define age-set! (lambda (row value)
      (vector-set! row 2 value)))
 
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
 
(define row-ref (<change>
      (lambda (table pos)
         (if (< pos (table-size table))
            (vector-ref table pos)
            #f))
      (lambda (_table0 _pos0)
         (if (< _pos0 (table-size _table0))
            (vector-ref _table0 _pos0)
            #f))))
 
(define row-set! (<change>
      (lambda (table pos row)
         (if (< pos (table-size table))
            (vector-set! table pos row)
            #f))
      (lambda (_table0 _pos0 _row0)
         (if (< _pos0 (table-size _table0))
            (vector-set! _table0 _pos0 _row0)
            #f))))
 
(define show-table (lambda (table)
      (define iter (lambda (index)
            (if (= index (table-size table))
               (newline2)
               (begin
                  (show-row (row-ref table index))
                  (newline2)
                  (iter (+ index 1))))))
      (iter 0)))
 
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
 
(define create-dictionary (<change>
      (lambda ()
         (let ((content ()))
            (define empty? (lambda ()
                  (null? content)))
            (define insert (lambda (key info)
                  (let ((temp (assoc key content)))
                     (if temp
                        (set-cdr! temp info)
                        (set! content (cons (cons key info) content))))
                  #t))
            (define delete (lambda (key)
                  (define remove-iter (lambda (current prev)
                        (if (null? current)
                           #f
                           (if (eq? key (caar current))
                              (begin
                                 (if (null? prev)
                                    (set! content (cdr content))
                                    (set-cdr! prev (cdr current)))
                                 #t)
                              (remove-iter (cdr current) current)))))
                  (remove-iter content ())))
            (define lookup (lambda (key)
                  (let ((temp (assoc key content)))
                     (if temp (cdr temp) #f))))
            (define map (lambda (a-function)
                  (define map-iter (lambda (the-current result)
                        (if (null? the-current)
                           (reverse result)
                           (map-iter (cdr the-current) (cons (a-function (caar the-current) (cdar the-current)) result)))))
                  (map-iter content ())))
            (define foreach (lambda (a-action)
                  (define foreach-iter (lambda (the-current)
                        (if (null? the-current)
                           #t
                           (begin
                              (a-action (caar the-current) (cdar the-current))
                              (foreach-iter (cdr the-current))))))
                  (foreach-iter content)
                  #t))
            (define display-dict (lambda ()
                  (foreach (lambda (key info) (display key) (display " ") (display info) (newline)))))
            (define dispatch (lambda (msg args)
                  (if (eq? msg 'empty?)
                     (empty?)
                     (if (eq? msg 'insert)
                        (insert (car args) (cadr args))
                        (if (eq? msg 'delete)
                           (delete (car args))
                           (if (eq? msg 'lookup)
                              (lookup (car args))
                              (if (eq? msg 'map)
                                 (map (car args))
                                 (if (eq? msg 'foreach)
                                    (foreach (car args))
                                    (if (eq? msg 'display)
                                       (display-dict)
                                       (error "unknown request -- create-dictionary" msg))))))))))
            dispatch))
      (lambda ()
         (let ((_content0 ()))
            (define empty? (lambda ()
                  (null? _content0)))
            (define insert (lambda (_key0 _info0)
                  (let ((_temp0 (assoc _key0 _content0)))
                     (if _temp0
                        (set-cdr! _temp0 _info0)
                        (set! _content0 (cons (cons _key0 _info0) _content0))))
                  #t))
            (define delete (lambda (_key1)
                  (define remove-iter (lambda (_current0 _prev0)
                        (if (null? _current0)
                           #f
                           (if (eq? _key1 (caar _current0))
                              (begin
                                 (if (null? _prev0)
                                    (set! _content0 (cdr _content0))
                                    (set-cdr! _prev0 (cdr _current0)))
                                 #t)
                              (remove-iter (cdr _current0) _current0)))))
                  (remove-iter _content0 ())))
            (define lookup (lambda (_key2)
                  (let ((_temp1 (assoc _key2 _content0)))
                     (if _temp1 (cdr _temp1) #f))))
            (define map (lambda (_a-function0)
                  (define map-iter (lambda (_the-current0 _result0)
                        (if (null? _the-current0)
                           (reverse _result0)
                           (map-iter
                              (cdr _the-current0)
                              (cons (_a-function0 (caar _the-current0) (cdar _the-current0)) _result0)))))
                  (map-iter _content0 ())))
            (define foreach (lambda (_a-action0)
                  (define foreach-iter (lambda (_the-current1)
                        (if (null? _the-current1)
                           #t
                           (begin
                              (_a-action0 (caar _the-current1) (cdar _the-current1))
                              (foreach-iter (cdr _the-current1))))))
                  (foreach-iter _content0)
                  #t))
            (define display-dict (lambda ()
                  (foreach (lambda (_key3 _info1) (display _key3) (display " ") (display _info1) (newline)))))
            (define dispatch (lambda (_msg0 _args0)
                  (if (eq? _msg0 'empty?)
                     (empty?)
                     (if (eq? _msg0 'insert)
                        (insert (car _args0) (cadr _args0))
                        (if (eq? _msg0 'delete)
                           (delete (car _args0))
                           (if (eq? _msg0 'lookup)
                              (lookup (car _args0))
                              (if (eq? _msg0 'map)
                                 (map (car _args0))
                                 (if (eq? _msg0 'foreach)
                                    (foreach (car _args0))
                                    (if (eq? _msg0 'display)
                                       (display-dict)
                                       (error "unknown request -- create-dictionary" _msg0))))))))))
            dispatch))))
 
(define nl->fr (create-dictionary))
 
(nl->fr 'insert (__toplevel_cons 'fiets (__toplevel_cons (__toplevel_cons 'bicyclette ()) ())))
 
(nl->fr 'insert (__toplevel_cons 'auto (__toplevel_cons (__toplevel_cons 'voiture ()) ())))
 
(nl->fr 'insert (__toplevel_cons 'huis (__toplevel_cons (__toplevel_cons 'maison ()) ())))
 
(nl->fr 'insert (__toplevel_cons 'vrachtwagen (__toplevel_cons (__toplevel_cons 'camion ()) ())))
 
(nl->fr 'insert (__toplevel_cons 'tientonner (__toplevel_cons (__toplevel_cons 'camion ()) ())))
 
(nl->fr 'lookup (__toplevel_cons 'fiets ()))
 
(nl->fr 'display ())
 
(define fr->eng (create-dictionary))
 
(fr->eng 'insert (__toplevel_cons 'bicyclette (__toplevel_cons (__toplevel_cons 'bike ()) ())))
 
(fr->eng 'insert (__toplevel_cons 'voiture (__toplevel_cons (__toplevel_cons 'car ()) ())))
 
(fr->eng
   'insert
   (__toplevel_cons 'maison (__toplevel_cons (__toplevel_cons 'house (__toplevel_cons 'home ())) ())))
 
(fr->eng 'insert (__toplevel_cons 'camion (__toplevel_cons (__toplevel_cons 'truck ()) ())))
 
(fr->eng 'lookup (__toplevel_cons 'bicyclette ()))
 
#t
 
(define my-++ (lambda (n)
      (+ n 1)))
 
(define my--- (<change>
      (lambda (n)
         (- n 1))
      (lambda (_n0)
         (- _n0 1))))
 
(define false #f)
 
(define true #t)
 
(define nil ())
 
(define key (lambda (x)
      x))
 
(define make-heap (<change>
      (lambda (a-vector nr-of-elements)
         (define iter (lambda (index)
               (if (> index 0)
                  (begin
                     (sift-down a-vector index nr-of-elements)
                     (iter (my--- index)))
                  #f)))
         (iter (quotient nr-of-elements 2)))
      (lambda (_a-vector0 _nr-of-elements0)
         (define iter (lambda (_index0)
               (if (> _index0 0)
                  (begin
                     (sift-down _a-vector0 _index0 _nr-of-elements0)
                     (iter (my--- _index0)))
                  #f)))
         (iter (quotient _nr-of-elements0 2)))))
 
(define sift-down (lambda (heap from to)
      (define smallest-child (<change>
            (lambda (parent)
               (let* ((child1 (* 2 parent))
                      (child2 (my-++ child1)))
                  (if (> child1 to)
                     false
                     (if (> child2 to)
                        child1
                        (if (< (key (vector-ref heap child1)) (key (vector-ref heap child2)))
                           child1
                           child2)))))
            (lambda (_parent0)
               (let* ((_child10 (* 2 _parent0))
                      (_child20 (my-++ _child10)))
                  (if (> _child10 to)
                     false
                     (if (> _child20 to)
                        _child10
                        (if (< (key (vector-ref heap _child10)) (key (vector-ref heap _child20)))
                           _child10
                           _child20)))))))
      (define iter (lambda (parent)
            (let ((child (smallest-child parent)))
               (if child
                  (if (> (key (vector-ref heap parent)) (key (vector-ref heap child)))
                     (begin
                        (swap heap child parent)
                        (iter child))
                     #f)
                  #f))))
      (iter from)))
 
(define swap (lambda (a-vector i1 i2)
      (<change>
         (let ((temp (vector-ref a-vector i1)))
            (vector-set! a-vector i1 (vector-ref a-vector i2))
            (vector-set! a-vector i2 temp))
         (let ((_temp0 (vector-ref a-vector i1)))
            (vector-set! a-vector i1 (vector-ref a-vector i2))
            (vector-set! a-vector i2 _temp0)))))
 
(define sift-up (lambda (heap from)
      (define iter (<change>
            (lambda (child)
               (let ((parent (quotient child 2)))
                  (if (> parent 0)
                     (if (> (key (vector-ref heap parent)) (key (vector-ref heap child)))
                        (begin
                           (swap heap child parent)
                           (iter parent))
                        #f)
                     #f)))
            (lambda (_child0)
               (let ((_parent0 (quotient _child0 2)))
                  (if (> _parent0 0)
                     (if (> (key (vector-ref heap _parent0)) (key (vector-ref heap _child0)))
                        (begin
                           (swap heap _child0 _parent0)
                           (iter _parent0))
                        #f)
                     #f)))))
      (iter from)))
 
(define create-heap (lambda (size)
      (cons 0 (make-vector (my-++ size)))))
 
(define is-empty? (<change>
      (lambda (heap)
         (eq? (car heap) 0))
      (lambda (_heap0)
         (eq? (car _heap0) 0))))
 
(define insert (<change>
      (lambda (heap item)
         (let* ((content (cdr heap))
                (new-nr-of-elements (my-++ (car heap)))
                (size (my--- (vector-length content))))
            (display "insert    ")
            (if (> new-nr-of-elements size)
               false
               (begin
                  (vector-set! content new-nr-of-elements item)
                  (sift-up content new-nr-of-elements)
                  (set-car! heap new-nr-of-elements)))
            (display heap)
            (newline)))
      (lambda (_heap0 _item0)
         (let* ((_content0 (cdr _heap0))
                (_new-nr-of-elements0 (my-++ (car _heap0)))
                (_size0 (my--- (vector-length _content0))))
            (display "insert    ")
            (if (> _new-nr-of-elements0 _size0)
               false
               (begin
                  (vector-set! _content0 _new-nr-of-elements0 _item0)
                  (sift-up _content0 _new-nr-of-elements0)
                  (set-car! _heap0 _new-nr-of-elements0)))
            (display _heap0)
            (newline)))))
 
(define v (vector 'lol 5 8 1 3 9 10 2 0))
 
(make-heap v 8)
 
(equal? v (vector 'lol 0 3 1 5 9 10 2 8))
 
(define quick-sort (<change>
      (lambda (a-list)
         (define rearrange (lambda (pivot some-list)
               (define rearrange-iter (lambda (rest result)
                     (if (null? rest)
                        result
                        (if (<= (car rest) pivot)
                           (rearrange-iter (cdr rest) (cons (cons (car rest) (car result)) (cdr result)))
                           (rearrange-iter (cdr rest) (cons (car result) (cons (car rest) (cdr result))))))))
               (rearrange-iter some-list (cons () ()))))
         (if (<= (length a-list) 1)
            a-list
            (let* ((pivot (car a-list))
                   (sub-lists (rearrange pivot (cdr a-list))))
               (append (quick-sort (car sub-lists)) (append (list pivot) (quick-sort (cdr sub-lists)))))))
      (lambda (_a-list0)
         (define rearrange (lambda (_pivot0 _some-list0)
               (define rearrange-iter (lambda (_rest0 _result0)
                     (if (null? _rest0)
                        _result0
                        (if (<= (car _rest0) _pivot0)
                           (rearrange-iter (cdr _rest0) (cons (cons (car _rest0) (car _result0)) (cdr _result0)))
                           (rearrange-iter (cdr _rest0) (cons (car _result0) (cons (car _rest0) (cdr _result0))))))))
               (rearrange-iter _some-list0 (cons () ()))))
         (if (<= (length _a-list0) 1)
            _a-list0
            (let* ((_pivot1 (car _a-list0))
                   (_sub-lists0 (rearrange _pivot1 (cdr _a-list0))))
               (append (quick-sort (car _sub-lists0)) (append (list _pivot1) (quick-sort (cdr _sub-lists0)))))))))
 
(equal?
   (quick-sort
      (__toplevel_cons
         9
         (__toplevel_cons
            8
            (__toplevel_cons
               7
               (__toplevel_cons
                  6
                  (__toplevel_cons
                     5
                     (__toplevel_cons
                        4
                        (__toplevel_cons
                           3
                           (__toplevel_cons 2 (__toplevel_cons 1 (__toplevel_cons 0 (__toplevel_cons 9 ()))))))))))))
   (__toplevel_cons
      0
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
                        (__toplevel_cons 7 (__toplevel_cons 8 (__toplevel_cons 9 (__toplevel_cons 9 ()))))))))))))
 
(define insertion-sort (<change>
      (lambda (vector)
         (let ((high (- (vector-length vector) 1)))
            (define shift-left (lambda (vector index)
                  (vector-set! vector (- index 1) (vector-ref vector index))))
            (define insert-sort-iter (lambda (index1)
                  (define insert (lambda (index1)
                        (let ((insert-value (vector-ref vector (- index1 1))))
                           (define insert-iter (lambda (index2)
                                 (if (if (<= index2 high) (< (vector-ref vector index2) insert-value) #f)
                                    (begin
                                       (shift-left vector index2)
                                       (insert-iter (+ index2 1)))
                                    (vector-set! vector (- index2 1) insert-value))))
                           (insert-iter index1))))
                  (if (> index1 0)
                     (begin
                        (insert index1)
                        (insert-sort-iter (- index1 1)))
                     #f)))
            (insert-sort-iter high)))
      (lambda (_vector0)
         (let ((_high0 (- (vector-length _vector0) 1)))
            (define shift-left (lambda (_vector1 _index0)
                  (vector-set! _vector1 (- _index0 1) (vector-ref _vector1 _index0))))
            (define insert-sort-iter (lambda (_index10)
                  (define insert (lambda (_index11)
                        (let ((_insert-value0 (vector-ref _vector0 (- _index11 1))))
                           (define insert-iter (lambda (_index20)
                                 (if (if (<= _index20 _high0) (< (vector-ref _vector0 _index20) _insert-value0) #f)
                                    (begin
                                       (shift-left _vector0 _index20)
                                       (insert-iter (+ _index20 1)))
                                    (vector-set! _vector0 (- _index20 1) _insert-value0))))
                           (insert-iter _index11))))
                  (if (> _index10 0)
                     (begin
                        (insert _index10)
                        (insert-sort-iter (- _index10 1)))
                     #f)))
            (insert-sort-iter _high0)))))
 
(define vect3 (vector 5 2 7 1 0 9 8 6 3 4))
 
(insertion-sort vect3)
 
(equal? vect3 (vector 0 1 2 3 4 5 6 7 8 9))
 
(define make-item (lambda (priority element)
      (cons priority element)))
 
(define get-priority (<change>
      (lambda (item)
         (car item))
      (lambda (_item0)
         (car _item0))))
 
(define get-element (<change>
      (lambda (item)
         (cdr item))
      (lambda (_item0)
         (cdr _item0))))
 
(define create-priority-queue (<change>
      (lambda ()
         (let ((front (cons 'boe ())))
            (define content (lambda ()
                  (cdr front)))
            (define insert-after! (lambda (cell item)
                  (let ((new-cell (cons item ())))
                     (set-cdr! new-cell (cdr cell))
                     (set-cdr! cell new-cell))))
            (define find-prev-cell (lambda (priority)
                  (define find-iter (lambda (rest prev)
                        (if (null? rest)
                           prev
                           (if (> (get-priority (car rest)) priority)
                              (find-iter (cdr rest) rest)
                              prev))))
                  (find-iter (content) front)))
            (define empty? (lambda ()
                  (null? (content))))
            (define enqueue (lambda (priority element)
                  (insert-after! (find-prev-cell priority) (make-item priority element))
                  true))
            (define dequeue (lambda ()
                  (if (null? (content))
                     false
                     (let ((temp (car (content))))
                        (set-cdr! front (cdr (content)))
                        (get-element temp)))))
            (define serve (lambda ()
                  (if (null? (content))
                     false
                     (get-element (car (content))))))
            (define dispatch (lambda (m)
                  (if (eq? m 'empty?)
                     empty?
                     (if (eq? m 'enqueue)
                        enqueue
                        (if (eq? m 'dequeue)
                           dequeue
                           (if (eq? m 'serve)
                              serve
                              (error "unknown request
                 -- create-priority-queue" m)))))))
            dispatch))
      (lambda ()
         (let ((_front0 (cons 'boe ())))
            (define content (lambda ()
                  (cdr _front0)))
            (define insert-after! (lambda (_cell0 _item0)
                  (let ((_new-cell0 (cons _item0 ())))
                     (set-cdr! _new-cell0 (cdr _cell0))
                     (set-cdr! _cell0 _new-cell0))))
            (define find-prev-cell (lambda (_priority0)
                  (define find-iter (lambda (_rest0 _prev0)
                        (if (null? _rest0)
                           _prev0
                           (if (> (get-priority (car _rest0)) _priority0)
                              (find-iter (cdr _rest0) _rest0)
                              _prev0))))
                  (find-iter (content) _front0)))
            (define empty? (lambda ()
                  (null? (content))))
            (define enqueue (lambda (_priority1 _element0)
                  (insert-after! (find-prev-cell _priority1) (make-item _priority1 _element0))
                  true))
            (define dequeue (lambda ()
                  (if (null? (content))
                     false
                     (let ((_temp0 (car (content))))
                        (set-cdr! _front0 (cdr (content)))
                        (get-element _temp0)))))
            (define serve (lambda ()
                  (if (null? (content))
                     false
                     (get-element (car (content))))))
            (define dispatch (lambda (_m0)
                  (if (eq? _m0 'empty?)
                     empty?
                     (if (eq? _m0 'enqueue)
                        enqueue
                        (if (eq? _m0 'dequeue)
                           dequeue
                           (if (eq? _m0 'serve)
                              serve
                              (error "unknown request
                 -- create-priority-queue" _m0)))))))
            dispatch))))
 
(define pq (create-priority-queue))
 
((pq 'enqueue) 66 'Patrick)
 
((pq 'enqueue) -106 'Octo)
 
((pq 'enqueue) 0 'Sandy)
 
((pq 'enqueue) 89 'Spongebob)
 
((pq 'dequeue))
 
(equal? ((pq 'dequeue)) 'Patrick)
 
(define copy (lambda (from-vector to-vector from-index to-index)
      (vector-set! to-vector to-index (vector-ref from-vector from-index))))
 
(define move (lambda (from-vector to-vector from-low from-high to-index)
      (define move-iter (<change>
            (lambda (n)
               (if (<= (+ from-low n) from-high)
                  (begin
                     (copy from-vector to-vector (+ from-low n) (+ to-index n))
                     (move-iter (+ n 1)))
                  #f))
            (lambda (_n0)
               (if (<= (+ from-low _n0) from-high)
                  (begin
                     (copy from-vector to-vector (+ from-low _n0) (+ to-index _n0))
                     (move-iter (+ _n0 1)))
                  #f))))
      (move-iter 0)))
 
(define merge (lambda (vector1 vector2 vector low1 high1 low2 high2 to-index)
      (define merge-iter (lambda (index index1 index2)
            (if (> index1 high1)
               (move vector2 vector index2 high2 index)
               (if (> index2 high2)
                  (move vector1 vector index1 high1 index)
                  (if (< (vector-ref vector1 index1) (vector-ref vector2 index2))
                     (begin
                        (copy vector1 vector index1 index)
                        (merge-iter (+ index 1) (+ index1 1) index2))
                     (begin
                        (copy vector2 vector index2 index)
                        (merge-iter (+ index 1) index1 (+ index2 1))))))))
      (merge-iter to-index low1 low2)))
 
(define bottom-up-merge-sort (<change>
      (lambda (vector)
         (define merge-subs (lambda (len)
               (let ((aux-vector (make-vector (vector-length vector) 0)))
                  (define merge-subs-iter (lambda (index)
                        (if (< index (- (vector-length vector) (* 2 len)))
                           (begin
                              (merge vector vector aux-vector index (+ index len -1) (+ index len) (+ index len len -1) index)
                              (move aux-vector vector index (+ index len len -1) index)
                              (merge-subs-iter (+ index len len)))
                           (if (< index (- (vector-length vector) len))
                              (begin
                                 (merge
                                    vector
                                    vector
                                    aux-vector
                                    index
                                    (+ index len -1)
                                    (+ index len)
                                    (- (vector-length vector) 1)
                                    index)
                                 (move aux-vector vector index (- (vector-length vector) 1) index))
                              #f))))
                  (merge-subs-iter 0))))
         (define merge-sort-iter (lambda (len)
               (if (< len (vector-length vector))
                  (begin
                     (merge-subs len)
                     (merge-sort-iter (* 2 len)))
                  #f)))
         (merge-sort-iter 1))
      (lambda (_vector0)
         (define merge-subs (lambda (_len0)
               (let ((_aux-vector0 (make-vector (vector-length _vector0) 0)))
                  (define merge-subs-iter (lambda (_index0)
                        (if (< _index0 (- (vector-length _vector0) (* 2 _len0)))
                           (begin
                              (merge
                                 _vector0
                                 _vector0
                                 _aux-vector0
                                 _index0
                                 (+ _index0 _len0 -1)
                                 (+ _index0 _len0)
                                 (+ _index0 _len0 _len0 -1)
                                 _index0)
                              (move _aux-vector0 _vector0 _index0 (+ _index0 _len0 _len0 -1) _index0)
                              (merge-subs-iter (+ _index0 _len0 _len0)))
                           (if (< _index0 (- (vector-length _vector0) _len0))
                              (begin
                                 (merge
                                    _vector0
                                    _vector0
                                    _aux-vector0
                                    _index0
                                    (+ _index0 _len0 -1)
                                    (+ _index0 _len0)
                                    (- (vector-length _vector0) 1)
                                    _index0)
                                 (move _aux-vector0 _vector0 _index0 (- (vector-length _vector0) 1) _index0))
                              #f))))
                  (merge-subs-iter 0))))
         (define merge-sort-iter (lambda (_len1)
               (if (< _len1 (vector-length _vector0))
                  (begin
                     (merge-subs _len1)
                     (merge-sort-iter (* 2 _len1)))
                  #f)))
         (merge-sort-iter 1))))
 
(<change>
   (let ((aVector (vector 8 3 6 6 0 5 4 2 9 6)))
      (bottom-up-merge-sort aVector)
      (equal? aVector (vector 0 2 3 4 5 6 6 6 8 9)))
   (let ((_aVector0 (vector 8 3 6 6 0 5 4 2 9 6)))
      (bottom-up-merge-sort _aVector0)
      (equal? _aVector0 (vector 0 2 3 4 5 6 6 6 8 9))))
 
(define quick-sort2 (lambda (vector)
      (define swap (<change>
            (lambda (v index1 index2)
               (let ((temp (vector-ref v index1)))
                  (vector-set! v index1 (vector-ref v index2))
                  (vector-set! v index2 temp)))
            (lambda (_v0 _index10 _index20)
               (let ((_temp0 (vector-ref _v0 _index10)))
                  (vector-set! _v0 _index10 (vector-ref _v0 _index20))
                  (vector-set! _v0 _index20 _temp0)))))
      (define quick-sort-aux (lambda (low high)
            (define quick-sort-aux-iter (<change>
                  (lambda (mid-value from to)
                     (define quick-right (lambda (index1)
                           (if (if (< index1 high) (< (vector-ref vector index1) mid-value) #f)
                              (quick-right (+ index1 1))
                              index1)))
                     (define quick-left (lambda (index2)
                           (if (if (> index2 low) (> (vector-ref vector index2) mid-value) #f)
                              (quick-left (- index2 1))
                              index2)))
                     (let ((index1 (quick-right (+ from 1)))
                           (index2 (quick-left to)))
                        (if (< index1 index2)
                           (begin
                              (swap vector index1 index2)
                              (quick-sort-aux-iter mid-value index1 index2))
                           index2)))
                  (lambda (_mid-value0 _from0 _to0)
                     (define quick-right (lambda (_index10)
                           (if (if (< _index10 high) (< (vector-ref vector _index10) _mid-value0) #f)
                              (quick-right (+ _index10 1))
                              _index10)))
                     (define quick-left (lambda (_index20)
                           (if (if (> _index20 low) (> (vector-ref vector _index20) _mid-value0) #f)
                              (quick-left (- _index20 1))
                              _index20)))
                     (let ((_index11 (quick-right (+ _from0 1)))
                           (_index21 (quick-left _to0)))
                        (if (< _index11 _index21)
                           (begin
                              (swap vector _index11 _index21)
                              (quick-sort-aux-iter _mid-value0 _index11 _index21))
                           _index21)))))
            (if (< low high)
               (<change>
                  (let ((middle (quotient (+ low high) 2))
                        (pivot-index (+ low 1)))
                     (swap vector middle pivot-index)
                     (if (> (vector-ref vector pivot-index) (vector-ref vector high))
                        (swap vector pivot-index high)
                        #f)
                     (if (> (vector-ref vector low) (vector-ref vector high))
                        (swap vector low high)
                        #f)
                     (if (< (vector-ref vector pivot-index) (vector-ref vector low))
                        (swap vector pivot-index low)
                        #f)
                     (let ((mid-index (quick-sort-aux-iter (vector-ref vector pivot-index) (+ low 1) high)))
                        (swap vector mid-index pivot-index)
                        (quick-sort-aux low (- mid-index 1))
                        (quick-sort-aux (+ mid-index 1) high)))
                  (let ((_middle0 (quotient (+ low high) 2))
                        (_pivot-index0 (+ low 1)))
                     (swap vector _middle0 _pivot-index0)
                     (if (> (vector-ref vector _pivot-index0) (vector-ref vector high))
                        (swap vector _pivot-index0 high)
                        #f)
                     (if (> (vector-ref vector low) (vector-ref vector high))
                        (swap vector low high)
                        #f)
                     (if (< (vector-ref vector _pivot-index0) (vector-ref vector low))
                        (swap vector _pivot-index0 low)
                        #f)
                     (let ((_mid-index0 (quick-sort-aux-iter (vector-ref vector _pivot-index0) (+ low 1) high)))
                        (swap vector _mid-index0 _pivot-index0)
                        (quick-sort-aux low (- _mid-index0 1))
                        (quick-sort-aux (+ _mid-index0 1) high))))
               #f)))
      (quick-sort-aux 0 (- (vector-length vector) 1))))
 
(define test3 (vector 8 3 6 6 1 5 4 2 9 6))
 
(quick-sort2 test3)
 
(equal? test3 (vector 1 2 3 4 5 6 6 6 8 9))
 
(define create-stack (lambda (eq-fnct)
      (let ((content ()))
         (define empty? (<change>
               (lambda ()
                  (null? content))
               (lambda ()
                  (null? content))))
         (define push (<change>
               (lambda (element)
                  (set! content (cons element content))
                  #t)
               (lambda (_element0)
                  (set! content (cons _element0 content))
                  #t)))
         (define pop (<change>
               (lambda ()
                  (if (null? content)
                     #f
                     (let ((temp (car content)))
                        (set! content (cdr content))
                        temp)))
               (lambda ()
                  (if (null? content)
                     #f
                     (let ((_temp0 (car content)))
                        (set! content (cdr content))
                        _temp0)))))
         (define top (lambda ()
               (if (null? content) #f (car content))))
         (define is-in (lambda (element)
               (if (member element content) #t #f)))
         (define dispatch (lambda (m)
               (if (eq? m 'empty?)
                  empty?
                  (if (eq? m 'push)
                     push
                     (if (eq? m 'pop)
                        pop
                        (if (eq? m 'top)
                           top
                           (if (eq? m 'is-in)
                              is-in
                              (error "unknown request -- create-stack" m))))))))
         dispatch)))
 
(<change>
   (let ((stack (create-stack =)))
      (if ((stack 'empty?))
         (if (begin ((stack 'push) 13) (not ((stack 'empty?))))
            (if ((stack 'is-in) 13)
               (if (= ((stack 'top)) 13)
                  (begin
                     ((stack 'push) 14)
                     (= ((stack 'pop)) 14))
                  #f)
               #f)
            #f)
         #f))
   (let ((_stack0 (create-stack =)))
      (if ((_stack0 'empty?))
         (if (begin ((_stack0 'push) 13) (not ((_stack0 'empty?))))
            (if ((_stack0 'is-in) 13)
               (if (= ((_stack0 'top)) 13)
                  (begin
                     ((_stack0 'push) 14)
                     (= ((_stack0 'pop)) 14))
                  #f)
               #f)
            #f)
         #f)))
 
