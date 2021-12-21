;; renamed lambdas/lets: 1
 
(define foldr (lambda (f base lst)
      (define foldr-aux (lambda (lst)
            (if (null? lst)
               base
               (f (car lst) (foldr-aux (cdr lst))))))
      (foldr-aux lst)))
 
(define totaal (lambda (aankopen kortingen)
      (define zoek-korting (lambda (kortingen artikel)
            (foldr + 0 (map (lambda (x) (if (eq? (car x) artikel) (cadr x) 0)) kortingen))))
      (if (null? aankopen)
         0
         (let* ((aankoop (car aankopen))
                (korting (zoek-korting kortingen (car aankoop)))
                (prijs (cadr aankoop)))
            (+ (- prijs (/ (* prijs korting) 100)) (totaal (cdr aankopen) (cdr kortingen)))))))
 
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
 
