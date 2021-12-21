;; renamed lambdas/lets: 97
 
(define atom? (<change>
      (lambda (x)
         (not (pair? x)))
      (lambda (_x0)
         (not (pair? _x0)))))
 
(define depth (<change>
      (lambda (tree)
         (if (null? tree)
            0
            (if (atom? tree)
               0
               (max (+ 1 (depth (car tree))) (depth (cdr tree))))))
      (lambda (_tree0)
         (if (null? _tree0)
            0
            (if (atom? _tree0)
               0
               (max (+ 1 (depth (car _tree0))) (depth (cdr _tree0))))))))
 
(define leaf-count (<change>
      (lambda (tree)
         (if (null? tree)
            0
            (if (atom? tree)
               1
               (+ (leaf-count (car tree)) (leaf-count (cdr tree))))))
      (lambda (_tree0)
         (if (null? _tree0)
            0
            (if (atom? _tree0)
               1
               (+ (leaf-count (car _tree0)) (leaf-count (cdr _tree0))))))))
 
(define depth-and-leaf-count (<change>
      (lambda (tree)
         (define make-res cons)
         (define depth car)
         (define leaf-count cdr)
         (if (null? tree)
            (make-res 0 0)
            (if (atom? tree)
               (make-res 0 1)
               (let ((res-car (depth-and-leaf-count (car tree)))
                     (res-cdr (depth-and-leaf-count (cdr tree))))
                  (make-res
                     (max (+ 1 (depth res-car)) (depth res-cdr))
                     (+ (leaf-count res-car) (leaf-count res-cdr)))))))
      (lambda (_tree0)
         (define make-res cons)
         (define depth car)
         (define leaf-count cdr)
         (if (null? _tree0)
            (make-res 0 0)
            (if (atom? _tree0)
               (make-res 0 1)
               (let ((_res-car0 (depth-and-leaf-count (car _tree0)))
                     (_res-cdr0 (depth-and-leaf-count (cdr _tree0))))
                  (make-res
                     (max (+ 1 (depth _res-car0)) (depth _res-cdr0))
                     (+ (leaf-count _res-car0) (leaf-count _res-cdr0)))))))))
 
(define l (__toplevel_cons
      (__toplevel_cons 1 (__toplevel_cons 2 ()))
      (__toplevel_cons
         (__toplevel_cons (__toplevel_cons 3 (__toplevel_cons 4 ())) (__toplevel_cons 5 ()))
         (__toplevel_cons (__toplevel_cons 6 (__toplevel_cons 7 ())) ()))))
 
(if (= (depth l) 3)
   (if (= (leaf-count l) 7)
      (equal? (depth-and-leaf-count l) (cons 3 7))
      #f)
   #f)
 
(define fringe (<change>
      (lambda (l)
         (if (null? l)
            ()
            (if (atom? l)
               (list l)
               (append (fringe (car l)) (fringe (cdr l))))))
      (lambda (_l0)
         (if (null? _l0)
            ()
            (if (atom? _l0)
               (list _l0)
               (append (fringe (car _l0)) (fringe (cdr _l0))))))))
 
(equal?
   (fringe
      (__toplevel_cons
         (__toplevel_cons 1 ())
         (__toplevel_cons
            (__toplevel_cons (__toplevel_cons (__toplevel_cons (__toplevel_cons 2 ()) ()) ()) ())
            (__toplevel_cons
               (__toplevel_cons
                  3
                  (__toplevel_cons (__toplevel_cons 4 (__toplevel_cons 5 ())) (__toplevel_cons 6 ())))
               (__toplevel_cons
                  (__toplevel_cons (__toplevel_cons 7 ()) (__toplevel_cons 8 (__toplevel_cons 9 ())))
                  ())))))
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
                  (__toplevel_cons 6 (__toplevel_cons 7 (__toplevel_cons 8 (__toplevel_cons 9 ()))))))))))
 
(define unfringe-1 (<change>
      (lambda (l)
         (if (null? l)
            ()
            (if (null? (cdr l))
               (list (car l))
               (list (car l) (unfringe-1 (cdr l))))))
      (lambda (_l0)
         (if (null? _l0)
            ()
            (if (null? (cdr _l0))
               (list (car _l0))
               (list (car _l0) (unfringe-1 (cdr _l0))))))))
 
(define unfringe-2 (lambda (l)
      (define pair (<change>
            (lambda (l)
               (if (null? l)
                  ()
                  (if (null? (cdr l))
                     (list l)
                     (cons (list (car l) (cadr l)) (pair (cddr l))))))
            (lambda (_l0)
               (if (null? _l0)
                  ()
                  (if (null? (cdr _l0))
                     (list _l0)
                     (cons (list (car _l0) (cadr _l0)) (pair (cddr _l0))))))))
      ((letrec ((loop (<change>
                       (lambda (l)
                          (if (let ((__or_res (null? l))) (if __or_res __or_res (null? (cdr l))))
                             l
                             (loop (pair l))))
                       (lambda (_l0)
                          (if (let ((___or_res0 (null? _l0))) (if ___or_res0 ___or_res0 (null? (cdr _l0))))
                             _l0
                             (loop (pair _l0)))))))
         loop)
         l)))
 
(if (equal? (unfringe-1 (__toplevel_cons 1 (__toplevel_cons 2 (__toplevel_cons 3 (__toplevel_cons 4 (__toplevel_cons 5 (__toplevel_cons 6 (__toplevel_cons 7 (__toplevel_cons 8 (__toplevel_cons 9 ())))))))))) (__toplevel_cons 1 (__toplevel_cons (__toplevel_cons 2 (__toplevel_cons (__toplevel_cons 3 (__toplevel_cons (__toplevel_cons 4 (__toplevel_cons (__toplevel_cons 5 (__toplevel_cons (__toplevel_cons 6 (__toplevel_cons (__toplevel_cons 7 (__toplevel_cons (__toplevel_cons 8 (__toplevel_cons (__toplevel_cons 9 ()) ())) ())) ())) ())) ())) ())) ())) ())))
   (equal?
      (unfringe-2
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
                        (__toplevel_cons 6 (__toplevel_cons 7 (__toplevel_cons 8 (__toplevel_cons 9 ()))))))))))
      (__toplevel_cons
         (__toplevel_cons
            (__toplevel_cons
               (__toplevel_cons
                  (__toplevel_cons 1 (__toplevel_cons 2 ()))
                  (__toplevel_cons (__toplevel_cons 3 (__toplevel_cons 4 ())) ()))
               (__toplevel_cons
                  (__toplevel_cons
                     (__toplevel_cons 5 (__toplevel_cons 6 ()))
                     (__toplevel_cons (__toplevel_cons 7 (__toplevel_cons 8 ())) ()))
                  ()))
            (__toplevel_cons (__toplevel_cons (__toplevel_cons (__toplevel_cons 9 ()) ()) ()) ()))
         ()))
   #f)
 
(define same-structure? (<change>
      (lambda (l1 l2)
         (if (if (atom? l1) (atom? l2) #f)
            #t
            (if (let ((__or_res (atom? l1))) (if __or_res __or_res (atom? l2)))
               #f
               (if (same-structure? (car l1) (car l2))
                  (same-structure? (cdr l1) (cdr l2))
                  #f))))
      (lambda (_l10 _l20)
         (if (if (atom? _l10) (atom? _l20) #f)
            #t
            (if (let ((___or_res0 (atom? _l10))) (if ___or_res0 ___or_res0 (atom? _l20)))
               #f
               (if (same-structure? (car _l10) (car _l20))
                  (same-structure? (cdr _l10) (cdr _l20))
                  #f))))))
 
(define same-structure?-or (<change>
      (lambda (l1 l2)
         (let ((__or_res (if (atom? l1) (atom? l2) #f)))
            (if __or_res
               __or_res
               (if (pair? l1)
                  (if (pair? l2)
                     (if (same-structure?-or (car l1) (car l2))
                        (same-structure?-or (cdr l1) (cdr l2))
                        #f)
                     #f)
                  #f))))
      (lambda (_l10 _l20)
         (let ((___or_res0 (if (atom? _l10) (atom? _l20) #f)))
            (if ___or_res0
               ___or_res0
               (if (pair? _l10)
                  (if (pair? _l20)
                     (if (same-structure?-or (car _l10) (car _l20))
                        (same-structure?-or (cdr _l10) (cdr _l20))
                        #f)
                     #f)
                  #f))))))
 
(if (same-structure? (__toplevel_cons (__toplevel_cons 1 (__toplevel_cons 2 ())) (__toplevel_cons (__toplevel_cons (__toplevel_cons 3 4) (__toplevel_cons (__toplevel_cons (__toplevel_cons 5 (__toplevel_cons 6 ())) (__toplevel_cons (__toplevel_cons (__toplevel_cons 7 (__toplevel_cons 8 ())) (__toplevel_cons (__toplevel_cons 9 ()) ())) ())) ())) ())) (__toplevel_cons (__toplevel_cons 'a (__toplevel_cons 'b ())) (__toplevel_cons (__toplevel_cons (__toplevel_cons 'c 'd) (__toplevel_cons (__toplevel_cons (__toplevel_cons 'e (__toplevel_cons 'f ())) (__toplevel_cons (__toplevel_cons (__toplevel_cons 'g (__toplevel_cons 'h ())) (__toplevel_cons (__toplevel_cons 'i ()) ())) ())) ())) ())))
   (not
      (same-structure?
         (__toplevel_cons
            (__toplevel_cons 1 (__toplevel_cons 2 ()))
            (__toplevel_cons
               (__toplevel_cons
                  (__toplevel_cons 3 (__toplevel_cons 4 ()))
                  (__toplevel_cons
                     (__toplevel_cons
                        (__toplevel_cons 5 (__toplevel_cons 6 ()))
                        (__toplevel_cons
                           (__toplevel_cons
                              (__toplevel_cons 7 (__toplevel_cons 8 ()))
                              (__toplevel_cons (__toplevel_cons 9 ()) ()))
                           ()))
                     ()))
               ()))
         (__toplevel_cons
            (__toplevel_cons
               (__toplevel_cons
                  (__toplevel_cons 1 (__toplevel_cons 2 ()))
                  (__toplevel_cons (__toplevel_cons 3 (__toplevel_cons 4 ())) ()))
               (__toplevel_cons
                  (__toplevel_cons
                     (__toplevel_cons 5 (__toplevel_cons 6 ()))
                     (__toplevel_cons (__toplevel_cons 7 (__toplevel_cons 8 ())) ()))
                  ()))
            (__toplevel_cons 9 ()))))
   #f)
 
(define deep-combine (<change>
      (lambda (combiner null-value l)
         (if (null? l)
            null-value
            (if (atom? l)
               l
               (combiner (deep-combine combiner null-value (car l)) (deep-combine combiner null-value (cdr l))))))
      (lambda (_combiner0 _null-value0 _l0)
         (if (null? _l0)
            _null-value0
            (if (atom? _l0)
               _l0
               (_combiner0
                  (deep-combine _combiner0 _null-value0 (car _l0))
                  (deep-combine _combiner0 _null-value0 (cdr _l0))))))))
 
(define deep-map (<change>
      (lambda (f l)
         (if (null? l)
            ()
            (if (atom? l)
               (f l)
               (cons (deep-map f (car l)) (deep-map f (cdr l))))))
      (lambda (_f0 _l0)
         (if (null? _l0)
            ()
            (if (atom? _l0)
               (_f0 _l0)
               (cons (deep-map _f0 (car _l0)) (deep-map _f0 (cdr _l0))))))))
 
(if (= (deep-combine + 0 (__toplevel_cons (__toplevel_cons (__toplevel_cons (__toplevel_cons 1 (__toplevel_cons 2 ())) (__toplevel_cons (__toplevel_cons 3 (__toplevel_cons 4 ())) ())) (__toplevel_cons (__toplevel_cons (__toplevel_cons 5 (__toplevel_cons 6 ())) (__toplevel_cons (__toplevel_cons 7 (__toplevel_cons 8 ())) ())) ())) (__toplevel_cons 9 ()))) 45)
   (equal?
      (deep-map
         (<change>
            (lambda (x)
               (* x x))
            (lambda (_x0)
               (* _x0 _x0)))
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
   #f)
 
(define boom (__toplevel_cons
      (__toplevel_cons 'blad (__toplevel_cons (__toplevel_cons 'appel 'golden) ()))
      (__toplevel_cons
         (__toplevel_cons 'blad (__toplevel_cons (__toplevel_cons 'appel 'granny) ()))
         (__toplevel_cons
            (__toplevel_cons
               (__toplevel_cons (__toplevel_cons 'appel 'golden) (__toplevel_cons 'blad ()))
               (__toplevel_cons 'blad (__toplevel_cons (__toplevel_cons 'appel 'cox) ())))
            ()))))
 
(define blad? (lambda (boom)
      (eq? boom 'blad)))
 
(define appel? (<change>
      (lambda (boom)
         (if (pair? boom) (eq? (car boom) 'appel) #f))
      (lambda (_boom0)
         (if (pair? _boom0) (eq? (car _boom0) 'appel) #f))))
 
(define type (<change>
      (lambda (appel)
         (cdr appel))
      (lambda (_appel0)
         (cdr _appel0))))
 
(define leafs (<change>
      (lambda (boom)
         (if (null? boom)
            0
            (if (blad? boom)
               1
               (if (appel? boom)
                  0
                  (+ (leafs (car boom)) (leafs (cdr boom)))))))
      (lambda (_boom0)
         (if (null? _boom0)
            0
            (if (blad? _boom0)
               1
               (if (appel? _boom0)
                  0
                  (+ (leafs (car _boom0)) (leafs (cdr _boom0)))))))))
 
(define all-apples (<change>
      (lambda (boom)
         (if (null? boom)
            ()
            (if (blad? boom)
               ()
               (if (appel? boom)
                  (list (type boom))
                  (append (all-apples (car boom)) (all-apples (cdr boom)))))))
      (lambda (_boom0)
         (if (null? _boom0)
            ()
            (if (blad? _boom0)
               ()
               (if (appel? _boom0)
                  (list (type _boom0))
                  (append (all-apples (car _boom0)) (all-apples (cdr _boom0)))))))))
 
(define conditional-append (<change>
      (lambda (l1 l2)
         (if (null? l1)
            l2
            (if (member (car l1) l2)
               (conditional-append (cdr l1) l2)
               (cons (car l1) (conditional-append (cdr l1) l2)))))
      (lambda (_l10 _l20)
         (if (null? _l10)
            _l20
            (if (member (car _l10) _l20)
               (conditional-append (cdr _l10) _l20)
               (cons (car _l10) (conditional-append (cdr _l10) _l20)))))))
 
(define apple-types (<change>
      (lambda (boom)
         (if (null? boom)
            ()
            (if (blad? boom)
               ()
               (if (appel? boom)
                  (list (type boom))
                  (conditional-append (apple-types (car boom)) (apple-types (cdr boom)))))))
      (lambda (_boom0)
         (if (null? _boom0)
            ()
            (if (blad? _boom0)
               ()
               (if (appel? _boom0)
                  (list (type _boom0))
                  (conditional-append (apple-types (car _boom0)) (apple-types (cdr _boom0)))))))))
 
(define bewerk-boom (<change>
      (lambda (boom doe-blad doe-appel combiner init)
         (if (null? boom)
            init
            (if (blad? boom)
               (doe-blad boom)
               (if (appel? boom)
                  (doe-appel boom)
                  (combiner
                     (bewerk-boom (car boom) doe-blad doe-appel combiner init)
                     (bewerk-boom (cdr boom) doe-blad doe-appel combiner init))))))
      (lambda (_boom0 _doe-blad0 _doe-appel0 _combiner0 _init0)
         (if (null? _boom0)
            _init0
            (if (blad? _boom0)
               (_doe-blad0 _boom0)
               (if (appel? _boom0)
                  (_doe-appel0 _boom0)
                  (_combiner0
                     (bewerk-boom (car _boom0) _doe-blad0 _doe-appel0 _combiner0 _init0)
                     (bewerk-boom (cdr _boom0) _doe-blad0 _doe-appel0 _combiner0 _init0))))))))
 
(define leafs-dmv-bewerk (<change>
      (lambda (boom)
         (bewerk-boom boom (lambda (blad) 1) (lambda (appel) 0) + 0))
      (lambda (_boom0)
         (bewerk-boom _boom0 (lambda (_blad0) 1) (lambda (_appel0) 0) + 0))))
 
(define all-apples-dmv-bewerk (<change>
      (lambda (boom)
         (bewerk-boom boom (lambda (blad) ()) (lambda (appel) (list (type appel))) append ()))
      (lambda (_boom0)
         (bewerk-boom _boom0 (lambda (_blad0) ()) (lambda (_appel0) (list (type _appel0))) append ()))))
 
(define apple-types-dmv-bewerk (<change>
      (lambda (boom)
         (bewerk-boom boom (lambda (blad) ()) (lambda (appel) (list (type appel))) conditional-append ()))
      (lambda (_boom0)
         (bewerk-boom
            _boom0
            (lambda (_blad0)
               ())
            (lambda (_appel0)
               (list (type _appel0)))
            conditional-append
            ()))))
 
(if (= (leafs boom) 4)
   (if (equal? (all-apples boom) (__toplevel_cons 'golden (__toplevel_cons 'granny (__toplevel_cons 'golden (__toplevel_cons 'cox ())))))
      (if (equal? (apple-types boom) (__toplevel_cons 'granny (__toplevel_cons 'golden (__toplevel_cons 'cox ()))))
         (if (= (leafs-dmv-bewerk boom) 4)
            (if (equal? (all-apples-dmv-bewerk boom) (__toplevel_cons 'golden (__toplevel_cons 'granny (__toplevel_cons 'golden (__toplevel_cons 'cox ())))))
               (equal?
                  (apple-types-dmv-bewerk boom)
                  (__toplevel_cons 'granny (__toplevel_cons 'golden (__toplevel_cons 'cox ()))))
               #f)
            #f)
         #f)
      #f)
   #f)
 
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
 
(define hierarchisch? (<change>
      (lambda (p1 p2 organigram)
         (define hierarchisch?-in (lambda (path organigrammen)
               (if (null? organigrammen)
                  #f
                  (let ((__or_res (hierarchisch? path (car organigrammen))))
                     (if __or_res
                        __or_res
                        (hierarchisch?-in path (cdr organigrammen)))))))
         (define hierarchisch? (lambda (path organigram)
               (if (if (eq? p1 (baas organigram)) (member p2 path) #f)
                  #t
                  (if (if (eq? p2 (baas organigram)) (member p1 path) #f)
                     #t
                     (hierarchisch?-in (cons (baas organigram) path) (sub-organigrammen organigram))))))
         (hierarchisch? () organigram))
      (lambda (_p10 _p20 _organigram0)
         (define hierarchisch?-in (lambda (_path0 _organigrammen0)
               (if (null? _organigrammen0)
                  #f
                  (let ((___or_res0 (hierarchisch? _path0 (car _organigrammen0))))
                     (if ___or_res0
                        ___or_res0
                        (hierarchisch?-in _path0 (cdr _organigrammen0)))))))
         (define hierarchisch? (lambda (_path1 _organigram1)
               (if (if (eq? _p10 (baas _organigram1)) (member _p20 _path1) #f)
                  #t
                  (if (if (eq? _p20 (baas _organigram1)) (member _p10 _path1) #f)
                     #t
                     (hierarchisch?-in (cons (baas _organigram1) _path1) (sub-organigrammen _organigram1))))))
         (hierarchisch? () _organigram0))))
 
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
 
(define atom? (<change>
      (lambda (x)
         (not (pair? x)))
      (lambda (_x0)
         (not (pair? _x0)))))
 
(define mijn-vuurwerk (__toplevel_cons
      'groen
      (__toplevel_cons
         (__toplevel_cons
            (__toplevel_cons
               'blauw
               (__toplevel_cons
                  (__toplevel_cons
                     'X
                     (__toplevel_cons
                        (__toplevel_cons 'blauw (__toplevel_cons (__toplevel_cons 'X (__toplevel_cons 'X ())) ()))
                        (__toplevel_cons 'X (__toplevel_cons 'X ()))))
                  ()))
            (__toplevel_cons
               (__toplevel_cons
                  'rood
                  (__toplevel_cons
                     (__toplevel_cons
                        (__toplevel_cons 'groen (__toplevel_cons (__toplevel_cons 'X (__toplevel_cons 'X ())) ()))
                        (__toplevel_cons 'X ()))
                     ()))
               (__toplevel_cons
                  'X
                  (__toplevel_cons
                     (__toplevel_cons 'geel (__toplevel_cons (__toplevel_cons 'X (__toplevel_cons 'X ())) ()))
                     ()))))
         ())))
 
(define kleur (<change>
      (lambda (vuurwerk)
         (car vuurwerk))
      (lambda (_vuurwerk0)
         (car _vuurwerk0))))
 
(define takken (<change>
      (lambda (vuurwerk)
         (cadr vuurwerk))
      (lambda (_vuurwerk0)
         (cadr _vuurwerk0))))
 
(define low-energy? (<change>
      (lambda (vuurwerk)
         (eq? vuurwerk 'X))
      (lambda (_vuurwerk0)
         (eq? _vuurwerk0 'X))))
 
(define tel-knallen (<change>
      (lambda (vuurwerk)
         (if (null? vuurwerk)
            0
            (if (low-energy? vuurwerk)
               0
               (if (atom? vuurwerk)
                  1
                  (+ (tel-knallen (car vuurwerk)) (tel-knallen (cdr vuurwerk)))))))
      (lambda (_vuurwerk0)
         (if (null? _vuurwerk0)
            0
            (if (low-energy? _vuurwerk0)
               0
               (if (atom? _vuurwerk0)
                  1
                  (+ (tel-knallen (car _vuurwerk0)) (tel-knallen (cdr _vuurwerk0)))))))))
 
(define tel-low-energies (<change>
      (lambda (v)
         (if (null? v)
            0
            (if (low-energy? v)
               1
               (if (atom? v)
                  0
                  (+ (tel-low-energies (car v)) (tel-low-energies (cdr v)))))))
      (lambda (_v0)
         (if (null? _v0)
            0
            (if (low-energy? _v0)
               1
               (if (atom? _v0)
                  0
                  (+ (tel-low-energies (car _v0)) (tel-low-energies (cdr _v0)))))))))
 
(define tel-einde-in (<change>
      (lambda (takken een-kleur)
         (if (null? takken)
            0
            (if (low-energy? (car takken))
               0
               (+ (tel-einde (car takken) een-kleur) (tel-einde-in (cdr takken) een-kleur)))))
      (lambda (_takken0 _een-kleur0)
         (if (null? _takken0)
            0
            (if (low-energy? (car _takken0))
               0
               (+ (tel-einde (car _takken0) _een-kleur0) (tel-einde-in (cdr _takken0) _een-kleur0)))))))
 
(define tel-einde (<change>
      (lambda (vuurwerk een-kleur)
         (if (eq? (kleur vuurwerk) een-kleur)
            (tel-low-energies (takken vuurwerk))
            (tel-einde-in (takken vuurwerk) een-kleur)))
      (lambda (_vuurwerk0 _een-kleur0)
         (if (eq? (kleur _vuurwerk0) _een-kleur0)
            (tel-low-energies (takken _vuurwerk0))
            (tel-einde-in (takken _vuurwerk0) _een-kleur0)))))
 
(define ster? (<change>
      (lambda (vuurwerk)
         (not (member 'X (takken vuurwerk))))
      (lambda (_vuurwerk0)
         (not (member 'X (takken _vuurwerk0))))))
 
(if (eq? (kleur mijn-vuurwerk) 'groen)
   (if (equal? (takken mijn-vuurwerk) (__toplevel_cons (__toplevel_cons 'blauw (__toplevel_cons (__toplevel_cons 'X (__toplevel_cons (__toplevel_cons 'blauw (__toplevel_cons (__toplevel_cons 'X (__toplevel_cons 'X ())) ())) (__toplevel_cons 'X (__toplevel_cons 'X ())))) ())) (__toplevel_cons (__toplevel_cons 'rood (__toplevel_cons (__toplevel_cons (__toplevel_cons 'groen (__toplevel_cons (__toplevel_cons 'X (__toplevel_cons 'X ())) ())) (__toplevel_cons 'X ())) ())) (__toplevel_cons 'X (__toplevel_cons (__toplevel_cons 'geel (__toplevel_cons (__toplevel_cons 'X (__toplevel_cons 'X ())) ())) ())))))
      (if (not (low-energy? mijn-vuurwerk))
         (if (low-energy? 'X)
            (if (= (tel-knallen mijn-vuurwerk) 6)
               (if (= (tel-einde mijn-vuurwerk 'blauw) 5)
                  (not (ster? mijn-vuurwerk))
                  #f)
               #f)
            #f)
         #f)
      #f)
   #f)
 
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
 
(define print-lijn (lambda (aantalblanco tekst)
      (display-n aantalblanco " ")
      (display2 tekst)
      (newline2)))
 
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
 
(define organigram-member-in (<change>
      (lambda (een-label organigrammen)
         (if (null? organigrammen)
            #f
            (let ((__or_res (organigram-member een-label (car organigrammen))))
               (if __or_res
                  __or_res
                  (organigram-member-in een-label (cdr organigrammen))))))
      (lambda (_een-label0 _organigrammen0)
         (if (null? _organigrammen0)
            #f
            (let ((___or_res0 (organigram-member _een-label0 (car _organigrammen0))))
               (if ___or_res0
                  ___or_res0
                  (organigram-member-in _een-label0 (cdr _organigrammen0))))))))
 
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
 
(define print-tot (lambda (organigram niveau)
      (define print-tot (<change>
            (lambda (organigram niveau max-niveau)
               (if (<= niveau max-niveau)
                  (begin
                     (print-lijn niveau (label organigram))
                     (for-each (lambda (organigram) (print-tot organigram (+ niveau 1) max-niveau)) (takken organigram)))
                  #f))
            (lambda (_organigram0 _niveau0 _max-niveau0)
               (if (<= _niveau0 _max-niveau0)
                  (begin
                     (print-lijn _niveau0 (label _organigram0))
                     (for-each
                        (lambda (_organigram1)
                           (print-tot _organigram1 (+ _niveau0 1) _max-niveau0))
                        (takken _organigram0)))
                  #f))))
      (print-tot organigram 0 niveau)))
 
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
 
(define maak-dier (<change>
      (lambda (naam eigenschappen)
         (list naam eigenschappen))
      (lambda (_naam0 _eigenschappen0)
         (list _naam0 _eigenschappen0))))
 
(define naam (<change>
      (lambda (dier)
         (car dier))
      (lambda (_dier0)
         (car _dier0))))
 
(define eigenschappen (<change>
      (lambda (dier)
         (cadr dier))
      (lambda (_dier0)
         (cadr _dier0))))
 
(define dier? (<change>
      (lambda (dier)
         (if (pair? dier)
            (if (atom? (naam dier))
               (pair? (eigenschappen dier))
               #f)
            #f))
      (lambda (_dier0)
         (if (pair? _dier0)
            (if (atom? (naam _dier0))
               (pair? (eigenschappen _dier0))
               #f)
            #f))))
 
(define maak-boom (<change>
      (lambda (knoop deelbomen)
         (list knoop deelbomen))
      (lambda (_knoop0 _deelbomen0)
         (list _knoop0 _deelbomen0))))
 
(define knoop (<change>
      (lambda (boom)
         (car boom))
      (lambda (_boom0)
         (car _boom0))))
 
(define deelbomen (<change>
      (lambda (boom)
         (cadr boom))
      (lambda (_boom0)
         (cadr _boom0))))
 
(define leeg? (<change>
      (lambda (boom)
         (null? boom))
      (lambda (_boom0)
         (null? _boom0))))
 
(define classificatieboom (maak-boom
      (maak-dier 'dier (__toplevel_cons 'kan-ademen (__toplevel_cons 'kan-bewegen ())))
      (list
         (maak-boom
            (maak-dier
               'vis
               (__toplevel_cons 'kan-zwemmen (__toplevel_cons 'heeft-schubben (__toplevel_cons 'heeft-vinnen ()))))
            (list (maak-dier 'ballonvis (__toplevel_cons 'kan-zwellen (__toplevel_cons 'is-geel ())))))
         (maak-boom
            (maak-dier
               'landdier
               (__toplevel_cons 'heeft-huid (__toplevel_cons 'kan-lopen (__toplevel_cons 'heeft-poten ()))))
            (list (maak-dier 'olifant (__toplevel_cons 'is-groot ()))))
         (maak-boom
            (maak-dier
               'vogel
               (__toplevel_cons 'kan-vliegen (__toplevel_cons 'heeft-vleugels (__toplevel_cons 'heeft-veren ()))))
            (list
               (maak-dier 'kanarie (__toplevel_cons 'kan-zingen (__toplevel_cons 'is-geel ())))
               (maak-dier 'arend (__toplevel_cons 'is-groot ())))))))
 
(define all-kinds (<change>
      (lambda (boom)
         (if (leeg? boom)
            ()
            (if (dier? boom)
               (list (naam boom))
               (if (dier? (knoop boom))
                  (append (list (naam (knoop boom))) (all-kinds-in (deelbomen boom)))
                  (all-kinds-in (deelbomen boom))))))
      (lambda (_boom0)
         (if (leeg? _boom0)
            ()
            (if (dier? _boom0)
               (list (naam _boom0))
               (if (dier? (knoop _boom0))
                  (append (list (naam (knoop _boom0))) (all-kinds-in (deelbomen _boom0)))
                  (all-kinds-in (deelbomen _boom0))))))))
 
(define all-kinds-in (<change>
      (lambda (lst)
         (if (null? lst)
            ()
            (append (all-kinds (car lst)) (all-kinds-in (cdr lst)))))
      (lambda (_lst0)
         (if (null? _lst0)
            ()
            (append (all-kinds (car _lst0)) (all-kinds-in (cdr _lst0)))))))
 
(define geef-eigenschappen (lambda (boom soort)
      (define geef-eig (<change>
            (lambda (boom eig)
               (if (dier? boom)
                  (if (eq? (naam boom) soort)
                     (append eig (list (eigenschappen boom)))
                     #f)
                  (if (if (dier? (knoop boom)) (eq? (naam (knoop boom)) soort) #f)
                     (append eig (eigenschappen (knoop boom)))
                     (geef-eig-in (deelbomen boom) (append eig (eigenschappen (knoop boom)))))))
            (lambda (_boom0 _eig0)
               (if (dier? _boom0)
                  (if (eq? (naam _boom0) soort)
                     (append _eig0 (list (eigenschappen _boom0)))
                     #f)
                  (if (if (dier? (knoop _boom0)) (eq? (naam (knoop _boom0)) soort) #f)
                     (append _eig0 (eigenschappen (knoop _boom0)))
                     (geef-eig-in (deelbomen _boom0) (append _eig0 (eigenschappen (knoop _boom0)))))))))
      (define geef-eig-in (<change>
            (lambda (lst eig)
               (if (null? lst)
                  #f
                  (let ((__or_res (geef-eig (car lst) eig)))
                     (if __or_res
                        __or_res
                        (geef-eig-in (cdr lst) eig)))))
            (lambda (_lst0 _eig0)
               (if (null? _lst0)
                  #f
                  (let ((___or_res0 (geef-eig (car _lst0) _eig0)))
                     (if ___or_res0
                        ___or_res0
                        (geef-eig-in (cdr _lst0) _eig0)))))))
      (geef-eig boom ())))
 
(define ask? (<change>
      (lambda (boom soort eig)
         (let ((eigenschappen (geef-eigenschappen boom soort)))
            (pair? (memq eig eigenschappen))))
      (lambda (_boom0 _soort0 _eig0)
         (let ((_eigenschappen0 (geef-eigenschappen _boom0 _soort0)))
            (pair? (memq _eig0 _eigenschappen0))))))
 
(if (equal? (all-kinds classificatieboom) (__toplevel_cons 'dier (__toplevel_cons 'vis (__toplevel_cons 'ballonvis (__toplevel_cons 'landdier (__toplevel_cons 'olifant (__toplevel_cons 'vogel (__toplevel_cons 'kanarie (__toplevel_cons 'arend ())))))))))
   (if (ask? classificatieboom 'landdier 'kan-lopen)
      (if (ask? classificatieboom 'ballonvis 'heeft-vinnen)
         (not (ask? classificatieboom 'olifant 'kan-vliegen))
         #f)
      #f)
   #f)
 
(define maak-blad (<change>
      (lambda (type)
         type)
      (lambda (_type0)
         _type0)))
 
(define geef-type (<change>
      (lambda (blad)
         blad)
      (lambda (_blad0)
         _blad0)))
 
(define maak-knoop (lambda (deelbomen)
      deelbomen))
 
(define geef-deelbomen (<change>
      (lambda (boom)
         boom)
      (lambda (_boom0)
         _boom0)))
 
(define maak-hybride-tak (<change>
      (lambda (knopen)
         knopen)
      (lambda (_knopen0)
         _knopen0)))
 
(define geef-knopen (<change>
      (lambda (tak)
         tak)
      (lambda (_tak0)
         _tak0)))
 
(define knoop? (<change>
      (lambda (boom)
         (pair? boom))
      (lambda (_boom0)
         (pair? _boom0))))
 
(define blad2? (<change>
      (lambda (boom)
         (atom? boom))
      (lambda (_boom0)
         (atom? _boom0))))
 
(define hybride-tak (maak-hybride-tak
      (list
         (maak-knoop
            (list
               (maak-knoop (list (maak-blad 'appel) (maak-blad 'appel) (maak-blad 'blad)))
               (maak-blad 'peer)))
         (maak-knoop (list (maak-blad 'blad) (maak-blad 'peer)))
         (maak-knoop (list (maak-blad 'appel) (maak-knoop (list (maak-blad 'appel) (maak-blad 'blad))))))))
 
(define tak (maak-hybride-tak
      (list
         (maak-knoop
            (list
               (maak-knoop (list (maak-blad 'appel) (maak-blad 'appel) (maak-blad 'blad)))
               (maak-blad 'peer)))
         (maak-knoop (list (maak-blad 'blad) (maak-blad 'peer) (maak-blad 'appel)))
         (maak-knoop (list (maak-blad 'appel) (maak-knoop (list (maak-blad 'appel) (maak-blad 'blad))))))))
 
(define tel (<change>
      (lambda (boom)
         (define combine-results (lambda (l1 l2)
               (list (+ (car l1) (car l2)) (+ (cadr l1) (cadr l2)) (+ (caddr l1) (caddr l2)))))
         (define tel-hulp (lambda (boom)
               (if (leeg? boom)
                  (list 0 0 0)
                  (if (if (blad2? boom) (eq? boom 'appel) #f)
                     (list 1 0 0)
                     (if (if (blad2? boom) (eq? boom 'peer) #f)
                        (list 0 1 0)
                        (if (blad2? boom)
                           (list 0 0 1)
                           (tel-hulp-in (geef-knopen boom))))))))
         (define tel-hulp-in (lambda (lst)
               (if (null? lst)
                  (list 0 0 0)
                  (combine-results (tel-hulp (car lst)) (tel-hulp-in (cdr lst))))))
         (tel-hulp boom))
      (lambda (_boom0)
         (define combine-results (lambda (_l10 _l20)
               (list (+ (car _l10) (car _l20)) (+ (cadr _l10) (cadr _l20)) (+ (caddr _l10) (caddr _l20)))))
         (define tel-hulp (lambda (_boom1)
               (if (leeg? _boom1)
                  (list 0 0 0)
                  (if (if (blad2? _boom1) (eq? _boom1 'appel) #f)
                     (list 1 0 0)
                     (if (if (blad2? _boom1) (eq? _boom1 'peer) #f)
                        (list 0 1 0)
                        (if (blad2? _boom1)
                           (list 0 0 1)
                           (tel-hulp-in (geef-knopen _boom1))))))))
         (define tel-hulp-in (lambda (_lst0)
               (if (null? _lst0)
                  (list 0 0 0)
                  (combine-results (tel-hulp (car _lst0)) (tel-hulp-in (cdr _lst0))))))
         (tel-hulp _boom0))))
 
(define member? (<change>
      (lambda (x lst)
         (pair? (memq x lst)))
      (lambda (_x0 _lst0)
         (pair? (memq _x0 _lst0)))))
 
(define normaal? (lambda (knoop)
      (<change>
         (let ((types (map (lambda (x) (if (pair? x) 'tak x)) knoop)))
            (not (if (member? 'appel types) (member? 'peer types) #f)))
         (let ((_types0 (map (lambda (_x0) (if (pair? _x0) 'tak _x0)) knoop)))
            (not (if (member? 'appel _types0) (member? 'peer _types0) #f))))))
 
(define check-normaal (<change>
      (lambda (boom)
         (if (leeg? boom)
            #t
            (if (blad2? boom)
               #t
               (if (knoop? boom)
                  (if (normaal? boom)
                     (check-normaal-in (geef-knopen boom))
                     #f)
                  (check-normaal-in (geef-knopen boom))))))
      (lambda (_boom0)
         (if (leeg? _boom0)
            #t
            (if (blad2? _boom0)
               #t
               (if (knoop? _boom0)
                  (if (normaal? _boom0)
                     (check-normaal-in (geef-knopen _boom0))
                     #f)
                  (check-normaal-in (geef-knopen _boom0))))))))
 
(define check-normaal-in (<change>
      (lambda (lst)
         (if (null? lst)
            #t
            (if (check-normaal (car lst))
               (check-normaal-in (cdr lst))
               #f)))
      (lambda (_lst0)
         (if (null? _lst0)
            #t
            (if (check-normaal (car _lst0))
               (check-normaal-in (cdr _lst0))
               #f)))))
 
(if (equal? (tel hybride-tak) (__toplevel_cons 4 (__toplevel_cons 2 (__toplevel_cons 3 ()))))
   (check-normaal hybride-tak)
   #f)
 
(define foldr (<change>
      (lambda (f base lst)
         (define foldr-aux (lambda (lst)
               (if (null? lst)
                  base
                  (f (car lst) (foldr-aux (cdr lst))))))
         (foldr-aux lst))
      (lambda (_f0 _base0 _lst0)
         (define foldr-aux (lambda (_lst1)
               (if (null? _lst1)
                  _base0
                  (_f0 (car _lst1) (foldr-aux (cdr _lst1))))))
         (foldr-aux _lst0))))
 
(define Coca-Cola-NV (__toplevel_cons
      'Coca-Cola-NV
      (__toplevel_cons
         (__toplevel_cons
            'Frisdranken
            (__toplevel_cons
               (__toplevel_cons
                  'Coca-Cola
                  (__toplevel_cons
                     (__toplevel_cons
                        'Regular-Coca-Cola
                        (__toplevel_cons (__toplevel_cons 'Coke (__toplevel_cons (__toplevel_cons 10000000 ()) ())) ()))
                     (__toplevel_cons
                        (__toplevel_cons
                           'light-Coca-Cola
                           (__toplevel_cons
                              (__toplevel_cons 'Coke-Light (__toplevel_cons (__toplevel_cons 800000 ()) ()))
                              (__toplevel_cons (__toplevel_cons 'Coke-Zero (__toplevel_cons (__toplevel_cons 200000 ()) ())) ())))
                        ())))
               (__toplevel_cons
                  (__toplevel_cons
                     'Fanta
                     (__toplevel_cons
                        (__toplevel_cons 'Fanta-Orange (__toplevel_cons (__toplevel_cons 800000 ()) ()))
                        (__toplevel_cons
                           (__toplevel_cons 'Fanta-Lemon (__toplevel_cons (__toplevel_cons 200000 ()) ()))
                           ())))
                  (__toplevel_cons
                     (__toplevel_cons
                        'Sprite
                        (__toplevel_cons
                           (__toplevel_cons 'Sprite-Zero (__toplevel_cons (__toplevel_cons 1000000 ()) ()))
                           ()))
                     ()))))
         (__toplevel_cons
            (__toplevel_cons
               'Sappen
               (__toplevel_cons
                  (__toplevel_cons
                     'Minute-Maid
                     (__toplevel_cons
                        (__toplevel_cons 'Minute-Maid-Sinaas (__toplevel_cons (__toplevel_cons 2000000 ()) ()))
                        (__toplevel_cons
                           (__toplevel_cons 'Minute-Maid-Tomaat (__toplevel_cons (__toplevel_cons 1000000 ()) ()))
                           ())))
                  ()))
            ()))))
 
(define omzetcijfer (<change>
      (lambda (categorie)
         (caadr categorie))
      (lambda (_categorie0)
         (caadr _categorie0))))
 
(define heeft-omzetcijfer (<change>
      (lambda (categorie)
         (if (pair? categorie)
            (if (pair? (cadr categorie))
               (if (atom? (caadr categorie))
                  (number? (caadr categorie))
                  #f)
               #f)
            #f))
      (lambda (_categorie0)
         (if (pair? _categorie0)
            (if (pair? (cadr _categorie0))
               (if (atom? (caadr _categorie0))
                  (number? (caadr _categorie0))
                  #f)
               #f)
            #f))))
 
(define deel-categorien (<change>
      (lambda (categorie)
         (cdr categorie))
      (lambda (_categorie0)
         (cdr _categorie0))))
 
(define hoofdcategorie (<change>
      (lambda (categorie)
         (car categorie))
      (lambda (_categorie0)
         (car _categorie0))))
 
(define bereken (<change>
      (lambda (lst)
         (if (null? lst)
            0
            (if (atom? lst)
               0
               (if (number? (car lst))
                  (car lst)
                  (+ (bereken (car lst)) (bereken (cdr lst)))))))
      (lambda (_lst0)
         (if (null? _lst0)
            0
            (if (atom? _lst0)
               0
               (if (number? (car _lst0))
                  (car _lst0)
                  (+ (bereken (car _lst0)) (bereken (cdr _lst0)))))))))
 
(define omzet (<change>
      (lambda (bedrijf categorie)
         (if (eq? (hoofdcategorie bedrijf) categorie)
            (bereken bedrijf)
            (omzet-in (deel-categorien bedrijf) categorie)))
      (lambda (_bedrijf0 _categorie0)
         (if (eq? (hoofdcategorie _bedrijf0) _categorie0)
            (bereken _bedrijf0)
            (omzet-in (deel-categorien _bedrijf0) _categorie0)))))
 
(define omzet-in (<change>
      (lambda (lst categorie)
         (if (null? lst)
            #f
            (let ((__or_res (omzet (car lst) categorie)))
               (if __or_res
                  __or_res
                  (omzet-in (cdr lst) categorie)))))
      (lambda (_lst0 _categorie0)
         (if (null? _lst0)
            #f
            (let ((___or_res0 (omzet (car _lst0) _categorie0)))
               (if ___or_res0
                  ___or_res0
                  (omzet-in (cdr _lst0) _categorie0)))))))
 
(define collect-pairs (<change>
      (lambda (bedrijf)
         (if (heeft-omzetcijfer bedrijf)
            (list (list (hoofdcategorie bedrijf) (omzetcijfer bedrijf)))
            (collect-pairs-in (deel-categorien bedrijf))))
      (lambda (_bedrijf0)
         (if (heeft-omzetcijfer _bedrijf0)
            (list (list (hoofdcategorie _bedrijf0) (omzetcijfer _bedrijf0)))
            (collect-pairs-in (deel-categorien _bedrijf0))))))
 
(define collect-pairs-in (<change>
      (lambda (lst)
         (if (null? lst)
            ()
            (append (collect-pairs (car lst)) (collect-pairs-in (cdr lst)))))
      (lambda (_lst0)
         (if (null? _lst0)
            ()
            (append (collect-pairs (car _lst0)) (collect-pairs-in (cdr _lst0)))))))
 
(define verdeel-democratisch (<change>
      (lambda (bedrijf budget)
         (let* ((pairs (collect-pairs bedrijf))
                (total (foldr + 0 (map cadr pairs)))
                (factor (/ budget total)))
            (map (lambda (x) (list (car x) (* factor (cadr x)))) pairs)))
      (lambda (_bedrijf0 _budget0)
         (let* ((_pairs0 (collect-pairs _bedrijf0))
                (_total0 (foldr + 0 (map cadr _pairs0)))
                (_factor0 (/ _budget0 _total0)))
            (map (lambda (_x0) (list (car _x0) (* _factor0 (cadr _x0)))) _pairs0)))))
 
(define verdeel (<change>
      (lambda (bedrijf budget)
         (if (heeft-omzetcijfer bedrijf)
            (list (hoofdcategorie bedrijf) budget)
            (let* ((rest (deel-categorien bedrijf))
                   (new-budget (/ budget (length rest))))
               (cons (hoofdcategorie bedrijf) (verdeel-in rest new-budget)))))
      (lambda (_bedrijf0 _budget0)
         (if (heeft-omzetcijfer _bedrijf0)
            (list (hoofdcategorie _bedrijf0) _budget0)
            (let* ((_rest0 (deel-categorien _bedrijf0))
                   (_new-budget0 (/ _budget0 (length _rest0))))
               (cons (hoofdcategorie _bedrijf0) (verdeel-in _rest0 _new-budget0)))))))
 
(define verdeel-in (<change>
      (lambda (lst budget)
         (if (null? lst)
            ()
            (cons (verdeel (car lst) budget) (verdeel-in (cdr lst) budget))))
      (lambda (_lst0 _budget0)
         (if (null? _lst0)
            ()
            (cons (verdeel (car _lst0) _budget0) (verdeel-in (cdr _lst0) _budget0))))))
 
(if (= (omzet Coca-Cola-NV 'Coca-Cola) 11000000)
   (if (= (omzet Coca-Cola-NV 'Sprite) 1000000)
      (if (= (omzet Coca-Cola-NV 'Minute-Maid) 3000000)
         (if (equal? (verdeel-democratisch Coca-Cola-NV 128000000) (__toplevel_cons (__toplevel_cons 'Coke (__toplevel_cons 80000000 ())) (__toplevel_cons (__toplevel_cons 'Coke-Light (__toplevel_cons 6400000 ())) (__toplevel_cons (__toplevel_cons 'Coke-Zero (__toplevel_cons 1600000 ())) (__toplevel_cons (__toplevel_cons 'Fanta-Orange (__toplevel_cons 6400000 ())) (__toplevel_cons (__toplevel_cons 'Fanta-Lemon (__toplevel_cons 1600000 ())) (__toplevel_cons (__toplevel_cons 'Sprite-Zero (__toplevel_cons 8000000 ())) (__toplevel_cons (__toplevel_cons 'Minute-Maid-Sinaas (__toplevel_cons 16000000 ())) (__toplevel_cons (__toplevel_cons 'Minute-Maid-Tomaat (__toplevel_cons 8000000 ())) ())))))))))
            (equal?
               (verdeel Coca-Cola-NV 1200000)
               (__toplevel_cons
                  'Coca-Cola-NV
                  (__toplevel_cons
                     (__toplevel_cons
                        'Frisdranken
                        (__toplevel_cons
                           (__toplevel_cons
                              'Coca-Cola
                              (__toplevel_cons
                                 (__toplevel_cons
                                    'Regular-Coca-Cola
                                    (__toplevel_cons (__toplevel_cons 'Coke (__toplevel_cons 100000 ())) ()))
                                 (__toplevel_cons
                                    (__toplevel_cons
                                       'light-Coca-Cola
                                       (__toplevel_cons
                                          (__toplevel_cons 'Coke-Light (__toplevel_cons 50000 ()))
                                          (__toplevel_cons (__toplevel_cons 'Coke-Zero (__toplevel_cons 50000 ())) ())))
                                    ())))
                           (__toplevel_cons
                              (__toplevel_cons
                                 'Fanta
                                 (__toplevel_cons
                                    (__toplevel_cons 'Fanta-Orange (__toplevel_cons 100000 ()))
                                    (__toplevel_cons (__toplevel_cons 'Fanta-Lemon (__toplevel_cons 100000 ())) ())))
                              (__toplevel_cons
                                 (__toplevel_cons
                                    'Sprite
                                    (__toplevel_cons (__toplevel_cons 'Sprite-Zero (__toplevel_cons 200000 ())) ()))
                                 ()))))
                     (__toplevel_cons
                        (__toplevel_cons
                           'Sappen
                           (__toplevel_cons
                              (__toplevel_cons
                                 'Minute-Maid
                                 (__toplevel_cons
                                    (__toplevel_cons 'Minute-Maid-Sinaas (__toplevel_cons 300000 ()))
                                    (__toplevel_cons (__toplevel_cons 'Minute-Maid-Tomaat (__toplevel_cons 300000 ())) ())))
                              ()))
                        ()))))
            #f)
         #f)
      #f)
   #f)
 
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
 
(define familiehoofd (<change>
      (lambda (fam)
         (car fam))
      (lambda (_fam0)
         (car _fam0))))
 
(define kinderen (<change>
      (lambda (fam)
         (cdr fam))
      (lambda (_fam0)
         (cdr _fam0))))
 
(define laatste-nakomeling? (<change>
      (lambda (fam)
         (null? (kinderen fam)))
      (lambda (_fam0)
         (null? (kinderen _fam0)))))
 
(define verdeel-democratisch2 (<change>
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
 
(define budget (<change>
      (lambda (boom budget-list)
         (define budget-hulp (lambda (boom budget-list)
               (+ (car budget-list) (budget-hulp-in (kinderen boom) (cdr budget-list)))))
         (define budget-hulp-in (lambda (bomen budget-list)
               (if (let ((__or_res (null? bomen))) (if __or_res __or_res (null? budget-list)))
                  0
                  (+ (budget-hulp (car bomen) budget-list) (budget-hulp-in (cdr bomen) budget-list)))))
         (budget-hulp-in (kinderen boom) budget-list))
      (lambda (_boom0 _budget-list0)
         (define budget-hulp (lambda (_boom1 _budget-list1)
               (+ (car _budget-list1) (budget-hulp-in (kinderen _boom1) (cdr _budget-list1)))))
         (define budget-hulp-in (lambda (_bomen0 _budget-list2)
               (if (let ((___or_res0 (null? _bomen0))) (if ___or_res0 ___or_res0 (null? _budget-list2)))
                  0
                  (+ (budget-hulp (car _bomen0) _budget-list2) (budget-hulp-in (cdr _bomen0) _budget-list2)))))
         (budget-hulp-in (kinderen _boom0) _budget-list0))))
 
(define verdeel2 (<change>
      (lambda (boom budget)
         (if (laatste-nakomeling? boom)
            (list (list (familiehoofd boom) budget))
            (let* ((rest (kinderen boom))
                   (new-budget (/ budget (length rest))))
               (verdeel2-in rest new-budget))))
      (lambda (_boom0 _budget0)
         (if (laatste-nakomeling? _boom0)
            (list (list (familiehoofd _boom0) _budget0))
            (let* ((_rest0 (kinderen _boom0))
                   (_new-budget0 (/ _budget0 (length _rest0))))
               (verdeel2-in _rest0 _new-budget0))))))
 
(define verdeel2-in (<change>
      (lambda (bomen budget)
         (if (null? bomen)
            ()
            (append (verdeel2 (car bomen) budget) (verdeel2-in (cdr bomen) budget))))
      (lambda (_bomen0 _budget0)
         (if (null? _bomen0)
            ()
            (append (verdeel2 (car _bomen0) _budget0) (verdeel2-in (cdr _bomen0) _budget0))))))
 
(if (= (verdeel-democratisch2 familieboom 1500) 100)
   (if (= (budget familieboom (__toplevel_cons 100 (__toplevel_cons 50 (__toplevel_cons 20 ())))) 650)
      (equal?
         (verdeel2 familieboom 3000)
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
 
(define VUB-circus (__toplevel_cons
      'ann
      (__toplevel_cons
         (__toplevel_cons
            'mien
            (__toplevel_cons
               (__toplevel_cons
                  'eef
                  (__toplevel_cons (__toplevel_cons 'bas ()) (__toplevel_cons (__toplevel_cons 'bob ()) ())))
               (__toplevel_cons
                  (__toplevel_cons
                     'els
                     (__toplevel_cons (__toplevel_cons 'jan ()) (__toplevel_cons (__toplevel_cons 'jos ()) ())))
                  (__toplevel_cons
                     (__toplevel_cons
                        'eva
                        (__toplevel_cons (__toplevel_cons 'tom ()) (__toplevel_cons (__toplevel_cons 'tim ()) ())))
                     ()))))
         (__toplevel_cons
            (__toplevel_cons
               'mies
               (__toplevel_cons
                  (__toplevel_cons
                     'ine
                     (__toplevel_cons (__toplevel_cons 'cas ()) (__toplevel_cons (__toplevel_cons 'cor ()) ())))
                  (__toplevel_cons
                     (__toplevel_cons
                        'ils
                        (__toplevel_cons (__toplevel_cons 'rik ()) (__toplevel_cons (__toplevel_cons 'raf ()) ())))
                     (__toplevel_cons
                        (__toplevel_cons
                           'ines
                           (__toplevel_cons (__toplevel_cons 'stef ()) (__toplevel_cons (__toplevel_cons 'staf ()) ())))
                        ()))))
            ()))))
 
(define hoofdartiest (<change>
      (lambda (piramide)
         (car piramide))
      (lambda (_piramide0)
         (car _piramide0))))
 
(define artiesten (lambda (piramide)
      (cdr piramide)))
 
(define artiest? (<change>
      (lambda (piramide)
         (if (pair? piramide) (atom? (car piramide)) #f))
      (lambda (_piramide0)
         (if (pair? _piramide0)
            (atom? (car _piramide0))
            #f))))
 
(define onderaan? (<change>
      (lambda (piramide)
         (null? (cdr piramide)))
      (lambda (_piramide0)
         (null? (cdr _piramide0)))))
 
(define jump (<change>
      (lambda (piramide artiest)
         (define jump-hulp (lambda (piramide pad)
               (if (if (artiest? piramide) (eq? (hoofdartiest piramide) artiest) #f)
                  pad
                  (jump-in (artiesten piramide) (cons (hoofdartiest piramide) pad)))))
         (define jump-in (lambda (lst pad)
               (if (null? lst)
                  #f
                  (let ((__or_res (jump-hulp (car lst) pad)))
                     (if __or_res __or_res (jump-in (cdr lst) pad))))))
         (reverse (jump-hulp piramide ())))
      (lambda (_piramide0 _artiest0)
         (define jump-hulp (lambda (_piramide1 _pad0)
               (if (if (artiest? _piramide1) (eq? (hoofdartiest _piramide1) _artiest0) #f)
                  _pad0
                  (jump-in (artiesten _piramide1) (cons (hoofdartiest _piramide1) _pad0)))))
         (define jump-in (lambda (_lst0 _pad1)
               (if (null? _lst0)
                  #f
                  (let ((___or_res0 (jump-hulp (car _lst0) _pad1)))
                     (if ___or_res0
                        ___or_res0
                        (jump-in (cdr _lst0) _pad1))))))
         (reverse (jump-hulp _piramide0 ())))))
 
(define fall (<change>
      (lambda (piramide artiest)
         (define fall-hulp (lambda (piramide pad)
               (if (if (artiest? piramide) (eq? (hoofdartiest piramide) artiest) #f)
                  (append pad (append (list (hoofdartiest piramide)) (map hoofdartiest (artiesten piramide))))
                  #f)
               (fall-in (artiesten piramide) (append pad (list (hoofdartiest piramide))))))
         (define fall-in (lambda (lst pad)
               (if (null? lst)
                  #f
                  (let ((__or_res (fall-hulp (car lst) pad)))
                     (if __or_res __or_res (fall-in (cdr lst) pad))))))
         (fall-hulp piramide ()))
      (lambda (_piramide0 _artiest0)
         (define fall-hulp (lambda (_piramide1 _pad0)
               (if (if (artiest? _piramide1) (eq? (hoofdartiest _piramide1) _artiest0) #f)
                  (append _pad0 (append (list (hoofdartiest _piramide1)) (map hoofdartiest (artiesten _piramide1))))
                  #f)
               (fall-in (artiesten _piramide1) (append _pad0 (list (hoofdartiest _piramide1))))))
         (define fall-in (lambda (_lst0 _pad1)
               (if (null? _lst0)
                  #f
                  (let ((___or_res0 (fall-hulp (car _lst0) _pad1)))
                     (if ___or_res0
                        ___or_res0
                        (fall-in (cdr _lst0) _pad1))))))
         (fall-hulp _piramide0 ()))))
 
(if (equal? (jump VUB-circus 'eva) (__toplevel_cons 'ann (__toplevel_cons 'mien ())))
   (if (equal? (jump VUB-circus 'stef) (__toplevel_cons 'ann (__toplevel_cons 'mies (__toplevel_cons 'ines ()))))
      (not
         (<change>
            (let ((__or_res (fall VUB-circus 'eva)))
               (if __or_res
                  __or_res
                  (let ((__or_res (fall VUB-circus 'stef)))
                     (if __or_res __or_res (fall VUB-circus 'mies)))))
            (let ((___or_res0 (fall VUB-circus 'eva)))
               (if ___or_res0
                  ___or_res0
                  (let ((___or_res1 (fall VUB-circus 'stef)))
                     (if ___or_res1
                        ___or_res1
                        (fall VUB-circus 'mies)))))))
      #f)
   #f)
 
