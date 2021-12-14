;; renamed lambdas/lets: 6
 
(define atom? (lambda (x)
      (if (not (pair? x)) (not (null? x)) #f)))
 
(define add1 (<change>
      (lambda (x)
         (+ x 1))
      (lambda (_x0)
         (+ _x0 1))))
 
(define rember? (lambda (a l)
      (if (null? l)
         ()
         (if (atom? (car l))
            (if (eq? (car l) a)
               (rember* a (cdr l))
               (cons (car l) (rember* a (cdr l))))
            (cons (rember* a (car l)) (rember* a (cdr l)))))))
 
(define insertR* (<change>
      (lambda (new old l)
         (if (null? l)
            ()
            (if (atom? (car l))
               (if (eq? (car l) old)
                  (cons (car l) (cons new (insertR* new old (cdr l))))
                  (cons (car l) (insertR* new old (cdr l))))
               (cons (insertR* new old (car l)) (insertR* new old (cdr l))))))
      (lambda (_new0 _old0 _l0)
         (if (null? _l0)
            ()
            (if (atom? (car _l0))
               (if (eq? (car _l0) _old0)
                  (cons (car _l0) (cons _new0 (insertR* _new0 _old0 (cdr _l0))))
                  (cons (car _l0) (insertR* _new0 _old0 (cdr _l0))))
               (cons (insertR* _new0 _old0 (car _l0)) (insertR* _new0 _old0 (cdr _l0))))))))
 
(define occur* (lambda (a l)
      (if (null? l)
         0
         (if (atom? (car l))
            (if (eq? (car l) a)
               (add1 (occur* a (cdr l)))
               (occur* a (cdr l)))
            (+ (occur* a (car l)) (occur* a (cdr l)))))))
 
(define subst* (<change>
      (lambda (new old l)
         (if (null? l)
            ()
            (if (atom? (car l))
               (if (eq? (car l) old)
                  (cons new (subst* new old (cdr l)))
                  (subst* new old (cdr l)))
               (cons (subst* new old (car l)) (subst* new old (cdr l))))))
      (lambda (_new0 _old0 _l0)
         (if (null? _l0)
            ()
            (if (atom? (car _l0))
               (if (eq? (car _l0) _old0)
                  (cons _new0 (subst* _new0 _old0 (cdr _l0)))
                  (subst* _new0 _old0 (cdr _l0)))
               (cons (subst* _new0 _old0 (car _l0)) (subst* _new0 _old0 (cdr _l0))))))))
 
(define insertL* (lambda (new old l)
      (if (null? l)
         ()
         (if (atom? (car l))
            (if (eq? (car l) old)
               (cons new (cons old (insertL* new old (cdr l))))
               (cons (car l) (insertL* new old (cdr l))))
            (cons (insertL* new old (car l)) (insertL* new old (cdr l)))))))
 
(define member* (lambda (a l)
      (if (null? l)
         #f
         (if (atom? (car l))
            (if (eq? (car l) a) #t (member* a (cdr l)))
            (let ((__or_res (member* a (car l))))
               (if __or_res __or_res (member* a (cdr l))))))))
 
(define leftmost (lambda (l)
      (if (atom? (car l)) (car l) (leftmost (car l)))))
 
(define eqan? (<change>
      (lambda (a1 a2)
         (if (if (number? a1) (number? a2) #f)
            (= a1 a2)
            (if (let ((__or_res (number? a1))) (if __or_res __or_res (number? a2)))
               #f
               (eq? a1 a2))))
      (lambda (_a10 _a20)
         (if (if (number? _a10) (number? _a20) #f)
            (= _a10 _a20)
            (if (let ((___or_res0 (number? _a10))) (if ___or_res0 ___or_res0 (number? _a20)))
               #f
               (eq? _a10 _a20))))))
 
(define eqlist? (lambda (l1 l2)
      (if (if (null? l1) (null? l2) #f)
         #t
         (if (let ((__or_res (null? l1))) (if __or_res __or_res (null? l2)))
            #f
            (if (if (atom? (car l1)) (atom? (car l2)) #f)
               (if (eqan? (car l1) (car l2))
                  (eqlist? (cdr l1) (cdr l2))
                  #f)
               (if (let ((__or_res (atom? (car l1)))) (if __or_res __or_res (atom? (car l2))))
                  #f
                  (if (eqlist? (car l1) (car l2))
                     (eqlist? (cdr l2) (cdr l2))
                     #f)))))))
 
(define my-equal? (lambda (s1 s2)
      (if (if (atom? s1) (atom? s2) #f)
         (eqan? s1 s2)
         (if (let ((__or_res (atom? s1))) (if __or_res __or_res (atom? s2)))
            #f
            (eqlist? s1 s2)))))
 
(define rember (<change>
      (lambda (s l)
         (if (null? l)
            ()
            (if (my-equal? (car l) s)
               (cdr l)
               (cons (car l) (rember s (cdr l))))))
      (lambda (_s0 _l0)
         (if (null? _l0)
            ()
            (if (my-equal? (car _l0) _s0)
               (cdr _l0)
               (cons (car _l0) (rember _s0 (cdr _l0))))))))
 
