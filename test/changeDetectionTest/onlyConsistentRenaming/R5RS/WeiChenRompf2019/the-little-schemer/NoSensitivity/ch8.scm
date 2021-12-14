;; renamed lambdas/lets: 26
 
(define atom? (lambda (x)
      (if (not (pair? x)) (not (null? x)) #f)))
 
(define rember-f (<change>
      (lambda (test?)
         (lambda (a l)
            (if (null? l)
               ()
               (if (test? a (car l))
                  (cdr l)
                  (cons (car l) (rember-f test? a (cdr l)))))))
      (lambda (_test?0)
         (lambda (_a0 _l0)
            (if (null? _l0)
               ()
               (if (_test?0 _a0 (car _l0))
                  (cdr _l0)
                  (cons (car _l0) (rember-f _test?0 _a0 (cdr _l0)))))))))
 
(define eq?-c (<change>
      (lambda (a)
         (lambda (x)
            (eq? x a)))
      (lambda (_a0)
         (lambda (_x0)
            (eq? _x0 _a0)))))
 
(define eq?-salad (eq?-c 'salad))
 
(define rember-eq? (rember-f eq?))
 
(define rember-equal? (rember-f equal?))
 
(define insertL-f (lambda (test?)
      (<change>
         (lambda (new old l)
            (if (null? l)
               ()
               (if (test? (car l) old)
                  (cons new (cons old (cdr l)))
                  (cons (car l) ((insertL-f test?) new old (cdr l))))))
         (lambda (_new0 _old0 _l0)
            (if (null? _l0)
               ()
               (if (test? (car _l0) _old0)
                  (cons _new0 (cons _old0 (cdr _l0)))
                  (cons (car _l0) ((insertL-f test?) _new0 _old0 (cdr _l0)))))))))
 
(define insertR-f (<change>
      (lambda (test?)
         (lambda (new old l)
            (if (null? l)
               ()
               (if (test? (car l) old)
                  (cons old (cons new (cdr l)))
                  (cons (car l) ((insertR-f test?) new old (cdr l)))))))
      (lambda (_test?0)
         (lambda (_new0 _old0 _l0)
            (if (null? _l0)
               ()
               (if (_test?0 (car _l0) _old0)
                  (cons _old0 (cons _new0 (cdr _l0)))
                  (cons (car _l0) ((insertR-f _test?0) _new0 _old0 (cdr _l0)))))))))
 
(define insert-left (lambda (new old l)
      (cons new (cons old l))))
 
(define insert-right (<change>
      (lambda (new old l)
         (cons old (cons new l)))
      (lambda (_new0 _old0 _l0)
         (cons _old0 (cons _new0 _l0)))))
 
(define insert-g (<change>
      (lambda (test?)
         (lambda (insert)
            (lambda (new old l)
               (if (null? l)
                  ()
                  (if (test? (car l) old)
                     (insert new old (cdr l))
                     (cons (car l) (((insert-g test?) insert) new old (cdr l))))))))
      (lambda (_test?0)
         (lambda (_insert0)
            (lambda (_new0 _old0 _l0)
               (if (null? _l0)
                  ()
                  (if (_test?0 (car _l0) _old0)
                     (_insert0 _new0 _old0 (cdr _l0))
                     (cons (car _l0) (((insert-g _test?0) _insert0) _new0 _old0 (cdr _l0))))))))))
 
(((insert-g equal?) insert-left)
   'a
   'b
   (__toplevel_cons 'c (__toplevel_cons 'd (__toplevel_cons 'e (__toplevel_cons 'b ())))))
 
(((insert-g equal?) insert-right)
   'a
   'b
   (__toplevel_cons 'c (__toplevel_cons 'd (__toplevel_cons 'e (__toplevel_cons 'b ())))))
 
(define seqL (<change>
      (lambda (new old l)
         (cons new (cons old l)))
      (lambda (_new0 _old0 _l0)
         (cons _new0 (cons _old0 _l0)))))
 
(define seqR (<change>
      (lambda (new old l)
         (cons old (cons new l)))
      (lambda (_new0 _old0 _l0)
         (cons _old0 (cons _new0 _l0)))))
 
(define insertR (insert-g seqR))
 
(define insertL (insert-g (lambda (new old l) (cons new (cons old l)))))
 
(define seqS (<change>
      (lambda (new old l)
         (cons new l))
      (lambda (_new0 _old0 _l0)
         (cons _new0 _l0))))
 
(define subst (insert-g seqS))
 
(define seqrem (<change>
      (lambda (new old l)
         l)
      (lambda (_new0 _old0 _l0)
         _l0)))
 
(define yyy (lambda (a l)
      ((insert-g seqrem) #f a l)))
 
(define ^ (lambda (n m)
      (expt n m)))
 
(define operator (lambda (aexp)
      (car aexp)))
 
(define first-sub-exp (lambda (aexp)
      (car (cdr aexp))))
 
(define second-sub-exp (lambda (aexp)
      (car (cdr (cdr aexp)))))
 
(define atom-to-function (lambda (x)
      (if (eq? x '+) + (if (eq? x '*) * ^))))
 
(define value (lambda (nexp)
      (if (atom? nexp)
         nexp
         ((atom-to-function (operator nexp)) (value (first-sub-exp nexp)) (value (second-sub-exp nexp))))))
 
(define multirember-f (lambda (test?)
      (lambda (a lat)
         (if (null? lat)
            ()
            (if (test? (car lat) a)
               ((multirember-f test?) a (cdr lat))
               (cons (car lat) ((multirember-f test?) a (cdr lat))))))))
 
(define multirember-eq? (multirember-f eq?))
 
(define multiremberT (lambda (test? lat)
      (if (null? lat)
         ()
         (if (test? (car lat))
            (multiremberT test? (cdr lat))
            (cons (car lat) (multiremberT test? (cdr lat)))))))
 
(multiremberT
   (lambda (x)
      (eq? x 'a))
   (__toplevel_cons
      'b
      (__toplevel_cons
         'c
         (__toplevel_cons 'd (__toplevel_cons 'a (__toplevel_cons 'x (__toplevel_cons 'a ())))))))
 
(define multirember&co (lambda (a lat col)
      (if (null? lat)
         (col () ())
         (if (eq? (car lat) a)
            (multirember&co a (cdr lat) (lambda (newlat seen) (col newlat (cons (car lat) seen))))
            (multirember&co a (cdr lat) (lambda (newlat seen) (col (cons (car lat) newlat) seen)))))))
 
(define a-friend (lambda (x y)
      (null? y)))
 
(multirember&co 'a (__toplevel_cons 'a ()) a-friend)
 
(multirember&co
   'a
   ()
   (lambda (newlat seen)
      (a-friend newlat (cons (car (__toplevel_cons 'a ())) seen))))
 
((lambda (newlat seen) (a-friend newlat (cons (car (__toplevel_cons 'a ())) seen))) () ())
 
(multirember&co 'a (__toplevel_cons 'b (__toplevel_cons 'a ())) a-friend)
 
(multirember&co
   'a
   (__toplevel_cons 'a ())
   (lambda (newlat seen)
      (a-friend (cons (car (__toplevel_cons 'b (__toplevel_cons 'a ()))) newlat) seen)))
 
(multirember&co
   'a
   ()
   (lambda (newlat seen)
      ((lambda (newlat seen) (a-friend (cons (car (__toplevel_cons 'b (__toplevel_cons 'a ()))) newlat) seen))
         newlat
         (cons (car (__toplevel_cons 'a ())) seen))))
 
((lambda (newlat seen)
   ((lambda (newlat seen) (a-friend (cons (car (__toplevel_cons 'b (__toplevel_cons 'a ()))) newlat) seen))
      newlat
      (cons (car (__toplevel_cons 'a ())) seen)))
   ()
   ())
 
(multirember&co
   'a
   (__toplevel_cons
      'a
      (__toplevel_cons
         'b
         (__toplevel_cons
            'c
            (__toplevel_cons 'd (__toplevel_cons 'a (__toplevel_cons 'b (__toplevel_cons 'a ())))))))
   (lambda (x y)
      (length x)))
 
(multirember&co
   'a
   (__toplevel_cons
      'a
      (__toplevel_cons
         'b
         (__toplevel_cons
            'c
            (__toplevel_cons 'd (__toplevel_cons 'a (__toplevel_cons 'b (__toplevel_cons 'a ())))))))
   (lambda (x y)
      (display (cons "x:" x))))
 
(multirember&co
   'a
   (__toplevel_cons
      'a
      (__toplevel_cons
         'b
         (__toplevel_cons
            'c
            (__toplevel_cons 'd (__toplevel_cons 'a (__toplevel_cons 'b (__toplevel_cons 'a ())))))))
   (lambda (x y)
      (display (cons "y:" y))))
 
(define multiinsertL (lambda (new old lat)
      (if (null? lat)
         ()
         (if (eq? old (car lat))
            (cons new (cons old (multiinsertL new old (cdr lat))))
            (cons (car lat) (multiinsertL new old (car lat)))))))
 
(define multiinsertR (lambda (new old lat)
      (if (null? lat)
         ()
         (if (eq? old (car lat))
            (cons old (cons new (multiinsertR new old (cdr lat))))
            (cons (car lat) (multiinsertR new old (cdr lat)))))))
 
(define multiinsertLR (<change>
      (lambda (new oldL oldR lat)
         (if (null? lat)
            ()
            (if (eq? (car lat) oldL)
               (cons new (cons oldL (multiinsertLR new oldL oldR (cdr lat))))
               (if (eq? (car lat) oldR)
                  (cons oldR (cons new (multiinsertLR new oldL oldR (cdr lat))))
                  (cons (car lat) (multiinsertLR new oldL oldR (cdr lat)))))))
      (lambda (_new0 _oldL0 _oldR0 _lat0)
         (if (null? _lat0)
            ()
            (if (eq? (car _lat0) _oldL0)
               (cons _new0 (cons _oldL0 (multiinsertLR _new0 _oldL0 _oldR0 (cdr _lat0))))
               (if (eq? (car _lat0) _oldR0)
                  (cons _oldR0 (cons _new0 (multiinsertLR _new0 _oldL0 _oldR0 (cdr _lat0))))
                  (cons (car _lat0) (multiinsertLR _new0 _oldL0 _oldR0 (cdr _lat0)))))))))
 
(define add1 (<change>
      (lambda (x)
         (+ x 1))
      (lambda (_x0)
         (+ _x0 1))))
 
(define multiinsertLR&co (lambda (new oldL oldR lat col)
      (if (null? lat)
         (col () 0 0)
         (if (eq? (car lat) oldL)
            (multiinsertLR&co
               new
               oldL
               oldR
               (cdr lat)
               (lambda (newlat L R)
                  (col (cons new (cons oldL newlat)) (add1 L) R)))
            (if (eq? (car lat) oldR)
               (multiinsertLR&co
                  new
                  oldL
                  oldR
                  (cdr lat)
                  (lambda (newlat L R)
                     (col (cons oldR (cons new newlat)) L (add1 R))))
               (multiinsertLR&co new oldL oldR (cdr lat) (lambda (newlat L R) (col (cons (car lat) newlat) L R))))))))
 
(multiinsertLR&co
   'salty
   'fish
   'chips
   (__toplevel_cons
      'chips
      (__toplevel_cons
         'and
         (__toplevel_cons
            'fish
            (__toplevel_cons 'or (__toplevel_cons 'fish (__toplevel_cons 'and (__toplevel_cons 'chips ())))))))
   (lambda (newlat Lcount Rcount)
      newlat))
 
(define sub1 (lambda (n)
      (- n 1)))
 
(define o+ (lambda (n m)
      (if (zero? m) n (add1 (o+ n (sub1 m))))))
 
(define o- (lambda (n m)
      (if (zero? m) n (sub1 (o- n (sub1 m))))))
 
(define o* (<change>
      (lambda (n m)
         (if (zero? m) 0 (o+ n (o* n (sub1 m)))))
      (lambda (_n0 _m0)
         (if (zero? _m0) 0 (o+ _n0 (o* _n0 (sub1 _m0)))))))
 
(define o/ (lambda (n m)
      (if (< n m) 0 (add1 (o/ (o- n m) m)))))
 
(define my-even? (<change>
      (lambda (n)
         (= (o* (o/ n 2) 2) n))
      (lambda (_n0)
         (= (o* (o/ _n0 2) 2) _n0))))
 
(define evens-only* (<change>
      (lambda (l)
         (if (null? l)
            ()
            (if (atom? (car l))
               (if (even? (car l))
                  (cons (car l) (evens-only* (cdr l)))
                  (evens-only* (cdr l)))
               (cons (evens-only* (car l)) (evens-only* (cdr l))))))
      (lambda (_l0)
         (if (null? _l0)
            ()
            (if (atom? (car _l0))
               (if (even? (car _l0))
                  (cons (car _l0) (evens-only* (cdr _l0)))
                  (evens-only* (cdr _l0)))
               (cons (evens-only* (car _l0)) (evens-only* (cdr _l0))))))))
 
(define evens-only*&co (<change>
      (lambda (l col)
         (if (null? l)
            (col () 1 0)
            (if (atom? (car l))
               (if (even? (car l))
                  (evens-only*&co (cdr l) (lambda (newl p s) (col (cons (car l) newl) (* (car l) p) s)))
                  (evens-only*&co (cdr l) (lambda (newl p s) (col newl p (+ (car l) s)))))
               (evens-only*&co
                  (car l)
                  (lambda (al ap as)
                     (evens-only*&co (cdr l) (lambda (dl dp ds) (col (cons al dl) (* ap dp) (+ as ds)))))))))
      (lambda (_l0 _col0)
         (if (null? _l0)
            (_col0 () 1 0)
            (if (atom? (car _l0))
               (if (even? (car _l0))
                  (evens-only*&co
                     (cdr _l0)
                     (lambda (_newl0 _p0 _s0)
                        (_col0 (cons (car _l0) _newl0) (* (car _l0) _p0) _s0)))
                  (evens-only*&co (cdr _l0) (lambda (_newl1 _p1 _s1) (_col0 _newl1 _p1 (+ (car _l0) _s1)))))
               (evens-only*&co
                  (car _l0)
                  (lambda (_al0 _ap0 _as0)
                     (evens-only*&co
                        (cdr _l0)
                        (lambda (_dl0 _dp0 _ds0)
                           (_col0 (cons _al0 _dl0) (* _ap0 _dp0) (+ _as0 _ds0)))))))))))
 
(evens-only*&co
   (__toplevel_cons
      (__toplevel_cons 9 (__toplevel_cons 1 (__toplevel_cons 2 (__toplevel_cons 8 ()))))
      (__toplevel_cons
         3
         (__toplevel_cons
            10
            (__toplevel_cons
               (__toplevel_cons
                  (__toplevel_cons 9 (__toplevel_cons 9 ()))
                  (__toplevel_cons 7 (__toplevel_cons 6 ())))
               (__toplevel_cons 2 ())))))
   (lambda (newl product sum)
      (cons sum (cons product newl))))
 
