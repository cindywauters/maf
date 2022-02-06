;; renamed lambdas/lets: 4
 
(define boom (cons
      (cons 'blad (cons (cons 'appel 'golden) ()))
      (cons
         (cons 'blad (cons (cons 'appel 'granny) ()))
         (cons
            (cons
               (cons (cons 'appel 'golden) (cons 'blad ()))
               (cons 'blad (cons (cons 'appel 'cox) ())))
            ()))))

(define blad? (lambda (boom)
      (eq? boom 'blad)))

(define appel? (lambda (boom)
      (if (pair? boom) (eq? (car boom) 'appel) #f)))

(define type (<change>
      (lambda (appel)
         (cdr appel))
      (lambda (_appel0)
         (cdr _appel0))))

(define leafs (lambda (boom)
      (if (null? boom)
         0
         (if (blad? boom)
            1
            (if (appel? boom)
               0
               (+ (leafs (car boom)) (leafs (cdr boom))))))))

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

(define conditional-append (lambda (l1 l2)
      (if (null? l1)
         l2
         (if (member (car l1) l2)
            (conditional-append (cdr l1) l2)
            (cons (car l1) (conditional-append (cdr l1) l2))))))

(define apple-types (lambda (boom)
      (if (null? boom)
         ()
         (if (blad? boom)
            ()
            (if (appel? boom)
               (list (type boom))
               (conditional-append (apple-types (car boom)) (apple-types (cdr boom))))))))

(define bewerk-boom (lambda (boom doe-blad doe-appel combiner init)
      (if (null? boom)
         init
         (if (blad? boom)
            (doe-blad boom)
            (if (appel? boom)
               (doe-appel boom)
               (combiner
                  (bewerk-boom (car boom) doe-blad doe-appel combiner init)
                  (bewerk-boom (cdr boom) doe-blad doe-appel combiner init)))))))

(define leafs-dmv-bewerk (lambda (boom)
      (bewerk-boom boom (<change> (lambda (blad) 1) (lambda (_blad0) 1)) (lambda (appel) 0) + 0)))

(define all-apples-dmv-bewerk (lambda (boom)
      (bewerk-boom boom (lambda (blad) ()) (lambda (appel) (list (type appel))) append ())))

(define apple-types-dmv-bewerk (<change>
      (lambda (boom)
         (bewerk-boom boom (lambda (blad) ()) (lambda (appel) (list (type appel))) conditional-append ()))
      (lambda (_boom0)
         (bewerk-boom
            _boom0
            (lambda (_blad0)
               ())
            (lambda (_appel0)
            (lambda (_appel0)
               (list (type _appel0)))
            conditional-append
            ()))))

(if (= (leafs boom) 4)
   (if (equal? (all-apples boom) (cons 'golden (cons 'granny (cons 'golden (cons 'cox ())))))
      (if (equal? (apple-types boom) (cons 'granny (cons 'golden (cons 'cox ()))))
         (if (= (leafs-dmv-bewerk boom) 4)
            (if (equal? (all-apples-dmv-bewerk boom) (cons 'golden (cons 'granny (cons 'golden (cons 'cox ())))))
               (equal?
                  (apple-types-dmv-bewerk boom)
                  (cons 'granny (cons 'golden (cons 'cox ()))))
               #f)
            #f)
         #f)
      #f)
   #f)
 
