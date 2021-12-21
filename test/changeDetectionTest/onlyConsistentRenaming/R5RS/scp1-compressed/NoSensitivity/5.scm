;; renamed lambdas/lets: 12
 
(define add-to-end (lambda (e l)
      (if (null? l)
         (cons e ())
         (cons (car l) (add-to-end e (cdr l))))))
 
(if (equal? (add-to-end 999 (__toplevel_cons 1 (__toplevel_cons 2 (__toplevel_cons 3 (__toplevel_cons 4 (__toplevel_cons 5 ())))))) (__toplevel_cons 1 (__toplevel_cons 2 (__toplevel_cons 3 (__toplevel_cons 4 (__toplevel_cons 5 (__toplevel_cons 999 ())))))))
   (if (equal? (add-to-end 999 ()) (__toplevel_cons 999 ()))
      (equal? (add-to-end 999 (__toplevel_cons 1 ())) (__toplevel_cons 1 (__toplevel_cons 999 ())))
      #f)
   #f)
 
(if (equal? (append (__toplevel_cons 1 (__toplevel_cons 2 (__toplevel_cons 3 ()))) (__toplevel_cons 4 (__toplevel_cons 5 ()))) (__toplevel_cons 1 (__toplevel_cons 2 (__toplevel_cons 3 (__toplevel_cons 4 (__toplevel_cons 5 ()))))))
   (if (equal? (append (__toplevel_cons 1 (__toplevel_cons 2 (__toplevel_cons 3 ()))) ()) (__toplevel_cons 1 (__toplevel_cons 2 (__toplevel_cons 3 ()))))
      (if (equal? (append () (__toplevel_cons 1 (__toplevel_cons 2 (__toplevel_cons 3 ())))) (__toplevel_cons 1 (__toplevel_cons 2 (__toplevel_cons 3 ()))))
         (null? (append () ()))
         #f)
      #f)
   #f)
 
(define super-merge-n (<change>
      (lambda (lsts n)
         (define geef-n+rest (lambda (lst n)
               (if (let ((__or_res (= 0 n))) (if __or_res __or_res (null? lst)))
                  (cons () lst)
                  (let* ((res (geef-n+rest (cdr lst) (- n 1)))
                         (first (car res))
                         (rest (cdr res)))
                     (cons (cons (car lst) first) rest)))))
         (if (null? lsts)
            ()
            (let* ((g-n+rest (geef-n+rest (car lsts) n))
                   (first (car g-n+rest))
                   (rest (cdr g-n+rest)))
               (append first (super-merge-n (append (cdr lsts) (if (null? rest) rest (list rest))) n)))))
      (lambda (_lsts0 _n0)
         (define geef-n+rest (lambda (_lst0 _n1)
               (if (let ((___or_res0 (= 0 _n1))) (if ___or_res0 ___or_res0 (null? _lst0)))
                  (cons () _lst0)
                  (let* ((_res0 (geef-n+rest (cdr _lst0) (- _n1 1)))
                         (_first0 (car _res0))
                         (_rest0 (cdr _res0)))
                     (cons (cons (car _lst0) _first0) _rest0)))))
         (if (null? _lsts0)
            ()
            (let* ((_g-n+rest0 (geef-n+rest (car _lsts0) _n0))
                   (_first1 (car _g-n+rest0))
                   (_rest1 (cdr _g-n+rest0)))
               (append _first1 (super-merge-n (append (cdr _lsts0) (if (null? _rest1) _rest1 (list _rest1))) _n0)))))))
 
(equal?
   (super-merge-n
      (__toplevel_cons
         (__toplevel_cons
            'a
            (__toplevel_cons
               'b
               (__toplevel_cons 'c (__toplevel_cons 'd (__toplevel_cons 'e (__toplevel_cons 'f ()))))))
         (__toplevel_cons
            (__toplevel_cons
               'g
               (__toplevel_cons 'h (__toplevel_cons 'i (__toplevel_cons 'j (__toplevel_cons 'k ())))))
            (__toplevel_cons
               (__toplevel_cons
                  'l
                  (__toplevel_cons
                     'm
                     (__toplevel_cons 'n (__toplevel_cons 'o (__toplevel_cons 'p (__toplevel_cons 'q ()))))))
               (__toplevel_cons
                  (__toplevel_cons
                     'r
                     (__toplevel_cons
                        's
                        (__toplevel_cons 't (__toplevel_cons 'u (__toplevel_cons 'v (__toplevel_cons 'w ()))))))
                  ()))))
      3)
   (__toplevel_cons
      'a
      (__toplevel_cons
         'b
         (__toplevel_cons
            'c
            (__toplevel_cons
               'g
               (__toplevel_cons
                  'h
                  (__toplevel_cons
                     'i
                     (__toplevel_cons
                        'l
                        (__toplevel_cons
                           'm
                           (__toplevel_cons
                              'n
                              (__toplevel_cons
                                 'r
                                 (__toplevel_cons
                                    's
                                    (__toplevel_cons
                                       't
                                       (__toplevel_cons
                                          'd
                                          (__toplevel_cons
                                             'e
                                             (__toplevel_cons
                                                'f
                                                (__toplevel_cons
                                                   'j
                                                   (__toplevel_cons
                                                      'k
                                                      (__toplevel_cons
                                                         'o
                                                         (__toplevel_cons
                                                            'p
                                                            (__toplevel_cons 'q (__toplevel_cons 'u (__toplevel_cons 'v (__toplevel_cons 'w ()))))))))))))))))))))))))
 
(define compare (lambda (lijst1 lijst2)
      (if (<change> (let ((__or_res (null? lijst1))) (if __or_res __or_res (null? lijst2))) (let ((___or_res0 (null? lijst1))) (if ___or_res0 ___or_res0 (null? lijst2))))
         0
         (if (eq? (car lijst1) (car lijst2))
            (+ 1 (compare (cdr lijst1) (cdr lijst2)))
            0))))
 
(define compare-iter (<change>
      (lambda (lijst1 lijst2)
         (define loop (lambda (l1 l2 res)
               (if (let ((__or_res (null? l1))) (if __or_res __or_res (null? l2)))
                  res
                  (if (eq? (car l1) (car l2))
                     (loop (cdr l1) (cdr l2) (+ res 1))
                     res))))
         (loop lijst1 lijst2 0))
      (lambda (_lijst10 _lijst20)
         (define loop (lambda (_l10 _l20 _res0)
               (if (let ((___or_res0 (null? _l10))) (if ___or_res0 ___or_res0 (null? _l20)))
                  _res0
                  (if (eq? (car _l10) (car _l20))
                     (loop (cdr _l10) (cdr _l20) (+ _res0 1))
                     _res0))))
         (loop _lijst10 _lijst20 0))))
 
(define algemene-compare (<change>
      (lambda (lijst1 lijst2 test)
         (if (let ((__or_res (null? lijst1))) (if __or_res __or_res (null? lijst2)))
            0
            (if (test (car lijst1) (car lijst2))
               (+ 1 (algemene-compare (cdr lijst1) (cdr lijst2) test))
               0)))
      (lambda (_lijst10 _lijst20 _test0)
         (if (let ((___or_res0 (null? _lijst10))) (if ___or_res0 ___or_res0 (null? _lijst20)))
            0
            (if (_test0 (car _lijst10) (car _lijst20))
               (+ 1 (algemene-compare (cdr _lijst10) (cdr _lijst20) _test0))
               0)))))
 
(define compare-greater (<change>
      (lambda (lijst1 lijst2)
         (algemene-compare lijst1 lijst2 >))
      (lambda (_lijst10 _lijst20)
         (algemene-compare _lijst10 _lijst20 >))))
 
(if (= (compare (__toplevel_cons 'a (__toplevel_cons 'b (__toplevel_cons 'c (__toplevel_cons 'd (__toplevel_cons 'e (__toplevel_cons 'f (__toplevel_cons 'g ()))))))) (__toplevel_cons 'a (__toplevel_cons 'b (__toplevel_cons 'c (__toplevel_cons 'x (__toplevel_cons 'y ())))))) 3)
   (if (= (compare (__toplevel_cons 'x (__toplevel_cons 'a (__toplevel_cons 'b ()))) (__toplevel_cons 'a (__toplevel_cons 'b (__toplevel_cons 'c (__toplevel_cons 'd (__toplevel_cons 'e (__toplevel_cons 'f (__toplevel_cons 'g ())))))))) 0)
      (if (= (compare (__toplevel_cons 'a (__toplevel_cons 'b (__toplevel_cons 'c (__toplevel_cons 'e (__toplevel_cons 'f (__toplevel_cons 'g ())))))) (__toplevel_cons 'a (__toplevel_cons 'b ()))) 2)
         (if (= (compare-iter (__toplevel_cons 'a (__toplevel_cons 'b (__toplevel_cons 'c (__toplevel_cons 'd (__toplevel_cons 'e (__toplevel_cons 'f (__toplevel_cons 'g ()))))))) (__toplevel_cons 'a (__toplevel_cons 'b (__toplevel_cons 'c (__toplevel_cons 'x (__toplevel_cons 'y ())))))) 3)
            (if (= (compare-iter (__toplevel_cons 'x (__toplevel_cons 'a (__toplevel_cons 'b ()))) (__toplevel_cons 'a (__toplevel_cons 'b (__toplevel_cons 'c (__toplevel_cons 'd (__toplevel_cons 'e (__toplevel_cons 'f (__toplevel_cons 'g ())))))))) 0)
               (if (= (compare-iter (__toplevel_cons 'a (__toplevel_cons 'b (__toplevel_cons 'c (__toplevel_cons 'e (__toplevel_cons 'f (__toplevel_cons 'g ())))))) (__toplevel_cons 'a (__toplevel_cons 'b ()))) 2)
                  (=
                     (compare-greater
                        (__toplevel_cons
                           3
                           (__toplevel_cons
                              5
                              (__toplevel_cons 6 (__toplevel_cons 1 (__toplevel_cons 2 (__toplevel_cons 5 ()))))))
                        (__toplevel_cons
                           2
                           (__toplevel_cons
                              1
                              (__toplevel_cons 0 (__toplevel_cons 8 (__toplevel_cons 5 (__toplevel_cons 5 ())))))))
                     3)
                  #f)
               #f)
            #f)
         #f)
      #f)
   #f)
 
(define show (<change>
      (lambda (namen punten test?)
         (if (null? namen)
            ()
            (let ((res (show (cdr namen) (cdr punten) test?)))
               (if (test? (car punten))
                  (cons (car namen) res)
                  res))))
      (lambda (_namen0 _punten0 _test?0)
         (if (null? _namen0)
            ()
            (let ((_res0 (show (cdr _namen0) (cdr _punten0) _test?0)))
               (if (_test?0 (car _punten0))
                  (cons (car _namen0) _res0)
                  _res0))))))
 
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
 
(define comprimeer (<change>
      (lambda (metingen)
         (define hulp (lambda (lst prev count)
               (if (null? lst)
                  (list (list prev count))
                  (if (= (car lst) prev)
                     (hulp (cdr lst) prev (+ count 1))
                     (cons (list prev count) (hulp (cdr lst) (car lst) 1))))))
         (if (null? metingen)
            ()
            (hulp (cdr metingen) (car metingen) 1)))
      (lambda (_metingen0)
         (define hulp (lambda (_lst0 _prev0 _count0)
               (if (null? _lst0)
                  (list (list _prev0 _count0))
                  (if (= (car _lst0) _prev0)
                     (hulp (cdr _lst0) _prev0 (+ _count0 1))
                     (cons (list _prev0 _count0) (hulp (cdr _lst0) (car _lst0) 1))))))
         (if (null? _metingen0)
            ()
            (hulp (cdr _metingen0) (car _metingen0) 1)))))
 
(define comprimeer-iter (<change>
      (lambda (metingen)
         (define hulp (lambda (lst prev count res)
               (if (null? lst)
                  (reverse (cons (list prev count) res))
                  (if (= (car lst) prev)
                     (hulp (cdr lst) prev (+ count 1) res)
                     (hulp (cdr lst) (car lst) 1 (cons (list prev count) res))))))
         (if (null? metingen)
            ()
            (hulp (cdr metingen) (car metingen) 1 ())))
      (lambda (_metingen0)
         (define hulp (lambda (_lst0 _prev0 _count0 _res0)
               (if (null? _lst0)
                  (reverse (cons (list _prev0 _count0) _res0))
                  (if (= (car _lst0) _prev0)
                     (hulp (cdr _lst0) _prev0 (+ _count0 1) _res0)
                     (hulp (cdr _lst0) (car _lst0) 1 (cons (list _prev0 _count0) _res0))))))
         (if (null? _metingen0)
            ()
            (hulp (cdr _metingen0) (car _metingen0) 1 ())))))
 
(if (equal? (comprimeer (__toplevel_cons 3.750000e+01 (__toplevel_cons 3.750000e+01 (__toplevel_cons 3.720000e+01 (__toplevel_cons 3.800000e+01 (__toplevel_cons 3.800000e+01 (__toplevel_cons 3.800000e+01 (__toplevel_cons 3.830000e+01 ())))))))) (__toplevel_cons (__toplevel_cons 3.750000e+01 (__toplevel_cons 2 ())) (__toplevel_cons (__toplevel_cons 3.720000e+01 (__toplevel_cons 1 ())) (__toplevel_cons (__toplevel_cons 3.800000e+01 (__toplevel_cons 3 ())) (__toplevel_cons (__toplevel_cons 3.830000e+01 (__toplevel_cons 1 ())) ())))))
   (equal?
      (comprimeer-iter
         (__toplevel_cons
            3.750000e+01
            (__toplevel_cons
               3.750000e+01
               (__toplevel_cons
                  3.720000e+01
                  (__toplevel_cons
                     3.800000e+01
                     (__toplevel_cons 3.800000e+01 (__toplevel_cons 3.800000e+01 (__toplevel_cons 3.830000e+01 ()))))))))
      (__toplevel_cons
         (__toplevel_cons 3.750000e+01 (__toplevel_cons 2 ()))
         (__toplevel_cons
            (__toplevel_cons 3.720000e+01 (__toplevel_cons 1 ()))
            (__toplevel_cons
               (__toplevel_cons 3.800000e+01 (__toplevel_cons 3 ()))
               (__toplevel_cons (__toplevel_cons 3.830000e+01 (__toplevel_cons 1 ())) ())))))
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
 
(define totaal (<change>
      (lambda (aankopen kortingen)
         (define zoek-korting (lambda (kortingen artikel)
               (foldr + 0 (map (lambda (x) (if (eq? (car x) artikel) (cadr x) 0)) kortingen))))
         (if (null? aankopen)
            0
            (let* ((aankoop (car aankopen))
                   (korting (zoek-korting kortingen (car aankoop)))
                   (prijs (cadr aankoop)))
               (+ (- prijs (/ (* prijs korting) 100)) (totaal (cdr aankopen) (cdr kortingen))))))
      (lambda (_aankopen0 _kortingen0)
         (define zoek-korting (lambda (_kortingen1 _artikel0)
               (foldr + 0 (map (lambda (_x0) (if (eq? (car _x0) _artikel0) (cadr _x0) 0)) _kortingen1))))
         (if (null? _aankopen0)
            0
            (let* ((_aankoop0 (car _aankopen0))
                   (_korting0 (zoek-korting _kortingen0 (car _aankoop0)))
                   (_prijs0 (cadr _aankoop0)))
               (+ (- _prijs0 (/ (* _prijs0 _korting0) 100)) (totaal (cdr _aankopen0) (cdr _kortingen0))))))))
 
(define totaal-iter (<change>
      (lambda (aankopen kortingen)
         (define zoek-korting (lambda (kortingen artikel)
               (foldr + 0 (map (lambda (x) (if (eq? (car x) artikel) (cadr x) 0)) kortingen))))
         (define loop (lambda (lst res)
               (if (null? lst)
                  res
                  (let* ((aankoop (car lst))
                         (korting (zoek-korting kortingen (car aankoop)))
                         (prijs (cadr aankoop)))
                     (loop (cdr lst) (+ res (- prijs (/ (* prijs korting) 100))))))))
         (loop aankopen 0))
      (lambda (_aankopen0 _kortingen0)
         (define zoek-korting (lambda (_kortingen1 _artikel0)
               (foldr + 0 (map (lambda (_x0) (if (eq? (car _x0) _artikel0) (cadr _x0) 0)) _kortingen1))))
         (define loop (lambda (_lst0 _res0)
               (if (null? _lst0)
                  _res0
                  (let* ((_aankoop0 (car _lst0))
                         (_korting0 (zoek-korting _kortingen0 (car _aankoop0)))
                         (_prijs0 (cadr _aankoop0)))
                     (loop (cdr _lst0) (+ _res0 (- _prijs0 (/ (* _prijs0 _korting0) 100))))))))
         (loop _aankopen0 0))))
 
(define Z&Mkortingen (__toplevel_cons
      (__toplevel_cons 'jas (__toplevel_cons 50 ()))
      (__toplevel_cons
         (__toplevel_cons 'kleed (__toplevel_cons 50 ()))
         (__toplevel_cons
            (__toplevel_cons 'rok (__toplevel_cons 30 ()))
            (__toplevel_cons (__toplevel_cons 'trui (__toplevel_cons 20 ())) ())))))
 
(if (= (totaal (__toplevel_cons (__toplevel_cons 'jas (__toplevel_cons 100 ())) (__toplevel_cons (__toplevel_cons 'trui (__toplevel_cons 25 ())) (__toplevel_cons (__toplevel_cons 'rok (__toplevel_cons 70 ())) (__toplevel_cons (__toplevel_cons 't-shirt (__toplevel_cons 20 ())) ())))) (__toplevel_cons (__toplevel_cons 'jas (__toplevel_cons 50 ())) (__toplevel_cons (__toplevel_cons 'kleed (__toplevel_cons 50 ())) (__toplevel_cons (__toplevel_cons 'rok (__toplevel_cons 30 ())) (__toplevel_cons (__toplevel_cons 'trui (__toplevel_cons 20 ())) ()))))) 139)
   (=
      (totaal-iter
         (__toplevel_cons
            (__toplevel_cons 'jas (__toplevel_cons 100 ()))
            (__toplevel_cons
               (__toplevel_cons 'trui (__toplevel_cons 25 ()))
               (__toplevel_cons
                  (__toplevel_cons 'rok (__toplevel_cons 70 ()))
                  (__toplevel_cons (__toplevel_cons 't-shirt (__toplevel_cons 20 ())) ()))))
         (__toplevel_cons
            (__toplevel_cons 'jas (__toplevel_cons 50 ()))
            (__toplevel_cons
               (__toplevel_cons 'kleed (__toplevel_cons 50 ()))
               (__toplevel_cons
                  (__toplevel_cons 'rok (__toplevel_cons 30 ()))
                  (__toplevel_cons (__toplevel_cons 'trui (__toplevel_cons 20 ())) ())))))
      139)
   #f)
 
