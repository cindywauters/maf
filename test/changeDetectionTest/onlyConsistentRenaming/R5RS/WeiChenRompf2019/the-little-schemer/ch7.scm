;; renamed lambdas/lets: 6
 
(define member? (lambda (a lat)
      (if (null? lat)
         #f
         (let ((__or_res (eq? (car lat) a)))
            (if __or_res __or_res (member? a (cdr lat)))))))
 
(define set? (lambda (lat)
      (if (null? lat)
         #t
         (if (member? (car lat) (cdr lat))
            #f
            (set? (cdr lat))))))
 
(define makeset (lambda (lat)
      (if (null? lat)
         ()
         (if (member? (car lat) (cdr lat))
            (makeset (cdr lat))
            (cons (car lat) (makeset (cdr lat)))))))
 
(define multirember (<change>
      (lambda (a lat)
         (if (null? lat)
            ()
            (if (eq? (car lat) a)
               (multirember a (cdr lat))
               (cons (car lat) (multirember a (cdr lat))))))
      (lambda (_a0 _lat0)
         (if (null? _lat0)
            ()
            (if (eq? (car _lat0) _a0)
               (multirember _a0 (cdr _lat0))
               (cons (car _lat0) (multirember _a0 (cdr _lat0))))))))
 
(define makeset (lambda (lat)
      (if (null? lat)
         ()
         (cons (car lat) (makeset (multirember (car lat) (cdr lat)))))))
 
(define subset? (<change>
      (lambda (set1 set2)
         (if (null? set1)
            #t
            (if (member? (car set1) set2)
               (subset? (cdr set1) set2)
               #f)))
      (lambda (_set10 _set20)
         (if (null? _set10)
            #t
            (if (member? (car _set10) _set20)
               (subset? (cdr _set10) _set20)
               #f)))))
 
(define subset? (lambda (set1 set2)
      (if (null? set1)
         #t
         (let ((__cond-empty-body (if (member? (car set1) set2)
                                    (subset? (cdr set1) set2)
                                    #f)))
            (if __cond-empty-body __cond-empty-body #f)))))
 
(define eqset? (lambda (set1 set2)
      (if (subset? set1 set2) (subset? set2 set1) #f)))
 
(define intersect? (<change>
      (lambda (set1 set2)
         (if (null? set1)
            #f
            (if (member? (car set1) set2)
               #t
               (intersect? (cdr set1) set2))))
      (lambda (_set10 _set20)
         (if (null? _set10)
            #f
            (if (member? (car _set10) _set20)
               #t
               (intersect? (cdr _set10) _set20))))))
 
(define intersect? (<change>
      (lambda (set1 set2)
         (if (null? set1)
            #f
            (let ((__or_res (member? (car set1) set2)))
               (if __or_res
                  __or_res
                  (intersect? (cdr set1) set2)))))
      (lambda (_set10 _set20)
         (if (null? _set10)
            #f
            (let ((___or_res0 (member? (car _set10) _set20)))
               (if ___or_res0
                  ___or_res0
                  (intersect? (cdr _set10) _set20)))))))
 
(define intersect (lambda (set1 set2)
      (if (null? set1)
         ()
         (if (member? (car set1) set2)
            (cons (car set1) (intersect (cdr set1) set2))
            (intersect (cdr set1) set2)))))
 
(define union (lambda (set1 set2)
      (if (null? set1)
         set2
         (if (member? (car set1) set2)
            (union (cdr set1) set2)
            (cons (car set1) (union (cdr set1) set2))))))
 
(define xxx (lambda (set1 set2)
      (if (null? set1)
         ()
         (if (member? (car set1) set2)
            (xxx (cdr set1) set2)
            (cons (car set1) (xxx (cdr set1) set2))))))
 
(define intersectall (lambda (l-set)
      (if (null? (cdr l-set))
         (car l-set)
         (intersect (car l-set) (intersectall (cdr l-set))))))
 
(define a-pair? (lambda (x)
      (if (atom? x)
         #f
         (if (null? x)
            #f
            (if (null? (cdr x))
               #f
               (if (null? (cdr (cdr x))) #t #f))))))
 
(define first (lambda (p)
      (car p)))
 
(define second (lambda (p)
      (car (cdr p))))
 
(define build (lambda (s1 s2)
      (cons s1 (cons s2 ()))))
 
(define third (<change>
      (lambda (l)
         (car (cdr (cdr l))))
      (lambda (_l0)
         (car (cdr (cdr _l0))))))
 
(define firsts (lambda (l)
      (if (null? l)
         ()
         (if cons
            (begin
               (first (car l))
               (firsts (cdr l)))
            #f))))
 
(define seconds (lambda (l)
      (if (null? l)
         ()
         (if cons
            (begin
               (second (car l))
               (seconds (cdr l)))
            #f))))
 
(define fun? (lambda (rel)
      (set? (firsts rel))))
 
(define revrel (lambda (rel)
      (if (null? rel)
         ()
         (cons (build (second (car rel)) (first (car rel))) (revrel (cdr rel))))))
 
(define revpair (lambda (pair)
      (build (second pair) (first pair))))
 
(define revrel (lambda (rel)
      (if (null? rel)
         ()
         (cons (revpair (car rel)) (revrel (cdr rel))))))
 
(define fullfun? (<change>
      (lambda (fun)
         (set? (seconds fun)))
      (lambda (_fun0)
         (set? (seconds _fun0)))))
 
(define one-to-one? (lambda (fun)
      (fun? (revrel fun))))
 
