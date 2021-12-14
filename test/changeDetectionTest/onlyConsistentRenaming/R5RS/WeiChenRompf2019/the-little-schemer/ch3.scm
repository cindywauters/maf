;; renamed lambdas/lets: 5
 
(define rember (<change>
      (lambda (a lat)
         (if (null? lat)
            ()
            (if (eq? (car lat) a)
               (cdr lat)
               (cons (car lat) (rember a (cdr lat))))))
      (lambda (_a0 _lat0)
         (if (null? _lat0)
            ()
            (if (eq? (car _lat0) _a0)
               (cdr _lat0)
               (cons (car _lat0) (rember _a0 (cdr _lat0))))))))
 
(rember 'a (__toplevel_cons 'b (__toplevel_cons 'c (__toplevel_cons 'a (__toplevel_cons 'd ())))))
 
(define first (<change>
      (lambda (l)
         (if (null? l)
            ()
            (cons (car (car l)) (first (cdr l)))))
      (lambda (_l0)
         (if (null? _l0)
            ()
            (cons (car (car _l0)) (first (cdr _l0)))))))
 
(first
   (__toplevel_cons
      (__toplevel_cons 'a (__toplevel_cons 'b ()))
      (__toplevel_cons
         (__toplevel_cons 'c (__toplevel_cons 'd ()))
         (__toplevel_cons (__toplevel_cons 'e (__toplevel_cons 'f ())) ()))))
 
(define insertR (lambda (new old lat)
      (if (null? lat)
         ()
         (if (eq? (car lat) old)
            (cons old (cons new (cdr lat)))
            (cons (car lat) (insertR new old (cdr lat)))))))
 
(insertR
   'z
   'b
   (__toplevel_cons 'a (__toplevel_cons 'b (__toplevel_cons 'c (__toplevel_cons 'd ())))))
 
(define insertL (lambda (new old lat)
      (if (null? lat)
         ()
         (if (eq? (car lat) old)
            (cons new lat)
            (cons (car lat) (insertL new old (cdr lat)))))))
 
(insertL
   'z
   'b
   (__toplevel_cons 'a (__toplevel_cons 'b (__toplevel_cons 'c (__toplevel_cons 'd ())))))
 
(define subst (lambda (new old lat)
      (if (null? lat)
         ()
         (if (eq? (car lat) old)
            (cons new (cdr lat))
            (cons (car lat) (subst new old (cdr lat)))))))
 
(subst
   'z
   'b
   (__toplevel_cons 'a (__toplevel_cons 'b (__toplevel_cons 'c (__toplevel_cons 'd ())))))
 
(define subst2 (<change>
      (lambda (new o1 o2 lat)
         (if (null? lat)
            ()
            (if (let ((__or_res (eq? (car lat) o1))) (if __or_res __or_res (eq? (car lat) o2)))
               (cons new (cdr lat))
               (cons (car lat) (subst2 new o1 o2 (cdr lat))))))
      (lambda (_new0 _o10 _o20 _lat0)
         (if (null? _lat0)
            ()
            (if (let ((___or_res0 (eq? (car _lat0) _o10))) (if ___or_res0 ___or_res0 (eq? (car _lat0) _o20)))
               (cons _new0 (cdr _lat0))
               (cons (car _lat0) (subst2 _new0 _o10 _o20 (cdr _lat0))))))))
 
(subst2
   'z
   'c
   'b
   (__toplevel_cons 'a (__toplevel_cons 'b (__toplevel_cons 'c (__toplevel_cons 'd ())))))
 
(subst2
   'z
   'c
   'b
   (__toplevel_cons 'a (__toplevel_cons 'c (__toplevel_cons 'b (__toplevel_cons 'd ())))))
 
(define multirember (lambda (a lat)
      (if (null? lat)
         ()
         (if (eq? (car lat) a)
            (multirember a (cdr lat))
            (cons (car lat) (multirember a (cdr lat)))))))
 
(multirember
   'a
   (__toplevel_cons
      'x
      (__toplevel_cons 'y (__toplevel_cons 'a (__toplevel_cons 'z (__toplevel_cons 'a ()))))))
 
(define multiinsertR (<change>
      (lambda (new old lat)
         (if (null? lat)
            ()
            (if (eq? (car lat) old)
               (cons old (cons new (multiinsertR new old (cdr lat))))
               (cons (car lat) (multiinsertR new old (cdr lat))))))
      (lambda (_new0 _old0 _lat0)
         (if (null? _lat0)
            ()
            (if (eq? (car _lat0) _old0)
               (cons _old0 (cons _new0 (multiinsertR _new0 _old0 (cdr _lat0))))
               (cons (car _lat0) (multiinsertR _new0 _old0 (cdr _lat0))))))))
 
(multiinsertR
   'a
   'b
   (__toplevel_cons
      'x
      (__toplevel_cons 'b (__toplevel_cons 'y (__toplevel_cons 'b (__toplevel_cons 'z ()))))))
 
(define multiinsertL (<change>
      (lambda (new old lat)
         (if (null? lat)
            ()
            (if (eq? (car lat) old)
               (cons new (cons old (multiinsertL new old (cdr lat))))
               (cons (car lat) (multiinsertL new old (cdr lat))))))
      (lambda (_new0 _old0 _lat0)
         (if (null? _lat0)
            ()
            (if (eq? (car _lat0) _old0)
               (cons _new0 (cons _old0 (multiinsertL _new0 _old0 (cdr _lat0))))
               (cons (car _lat0) (multiinsertL _new0 _old0 (cdr _lat0))))))))
 
(multiinsertL
   'a
   'b
   (__toplevel_cons
      'x
      (__toplevel_cons 'b (__toplevel_cons 'y (__toplevel_cons 'b (__toplevel_cons 'z ()))))))
 
(define multisubst (lambda (new old lat)
      (if (null? lat)
         ()
         (if (eq? (car lat) old)
            (cons new (multisubst new old (cdr lat)))
            (cons (car lat) (multisubst new old (cdr lat)))))))
 
(multisubst
   'a
   'b
   (__toplevel_cons
      'x
      (__toplevel_cons 'b (__toplevel_cons 'y (__toplevel_cons 'b (__toplevel_cons 'z ()))))))
 
