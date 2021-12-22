;; renamed lambdas/lets: 3
;; Of which consistent renamings: 2

(define one-to (lambda (n)
      (<change>
         (letrec ((loop (lambda (i l)
                          (if (= i 0) l (loop (- i 1) (cons i l))))))
            (loop n ()))
         (letrec ((_loop0 (lambda (_i0 _l0)
                            (if (= _i0 0)
                               _l0
                               (_loop0 (- _i0 1) (cons _i0 _l0))))))
            (_loop0 n ())))))
 
(define ok? (<change>
      (lambda (row dist placed)
         (if (null? placed)
            #t
            (if (not (= (car placed) (+ row dist)))
               (if (not (= (car placed) (- row dist)))
                  (ok? row (+ dist 1) (cdr placed))
                  #f)
               #f)))
      (lambda (_row0 _dist0 _placed0)
         (if (null? _placed0)
            #f ;; NOT RENAMING, changed true to false
            (if (not (= (car _placed0) (+ _row0 _dist0)))
               (if (not (= (car _placed0) (- _row0 _dist0)))
                  (ok? _row0 (+ _dist0 1) (cdr _placed0))
                  #f)
               #f)))))
 
(define try-it (<change>
      (lambda (x y z)
         (if (null? x)
            (if (null? y) 1 0)
            (+
               (if (ok? (car x) 1 z)
                  (try-it (append (cdr x) y) () (cons (car x) z))
                  0)
               (try-it (cdr x) (cons (car x) y) z))))
      (lambda (_x0 _y0 _z0)
         (if (null? _x0)
            (if (null? _y0) 1 0)
            (+
               (if (ok? (car _x0) 1 _z0)
                  (try-it (append (cdr _x0) _y0) () (cons (car _x0) _z0))
                  0)
               (try-it (cdr _x0) (cons (car _x0) _y0) _z0))))))
 
(define nqueens (lambda (n)
      (try-it (one-to n) () ())))
 
(nqueens 8)
 
#t
 
