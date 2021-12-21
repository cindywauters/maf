;; renamed lambdas/lets: 4
 
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
 
(define depth-and-leaf-count (lambda (tree)
      (define make-res cons)
      (define depth car)
      (define leaf-count cdr)
      (if (null? tree)
         (make-res 0 0)
         (if (atom? tree)
            (make-res 0 1)
            (<change>
               (let ((res-car (depth-and-leaf-count (car tree)))
                     (res-cdr (depth-and-leaf-count (cdr tree))))
                  (make-res
                     (max (+ 1 (depth res-car)) (depth res-cdr))
                     (+ (leaf-count res-car) (leaf-count res-cdr))))
               (let ((_res-car0 (depth-and-leaf-count (car tree)))
                     (_res-cdr0 (depth-and-leaf-count (cdr tree))))
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
 
