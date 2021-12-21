;; renamed lambdas/lets: 6
 
(define make-point (lambda (x y)
      (define dispatch (<change>
            (lambda (msg)
               (if (eq? msg 'x-value)
                  x
                  (if (eq? msg 'y-value) y (error "wrong message"))))
            (lambda (_msg0)
               (if (eq? _msg0 'x-value)
                  x
                  (if (eq? _msg0 'y-value)
                     y
                     (error "wrong message"))))))
      dispatch))
 
(define make-segment (<change>
      (lambda (start end)
         (define midpoint (lambda ()
               (make-point (/ (+ (start 'x-value) (end 'x-value)) 2) (/ (+ (start 'y-value) (end 'y-value)) 2))))
         (define dispatch (lambda (msg)
               (if (eq? msg 'start-point)
                  start
                  (if (eq? msg 'end-point)
                     end
                     (if (eq? msg 'midpoint)
                        (midpoint)
                        (error "wrong message"))))))
         dispatch)
      (lambda (_start0 _end0)
         (define midpoint (lambda ()
               (make-point
                  (/ (+ (_start0 'x-value) (_end0 'x-value)) 2)
                  (/ (+ (_start0 'y-value) (_end0 'y-value)) 2))))
         (define dispatch (lambda (_msg0)
               (if (eq? _msg0 'start-point)
                  _start0
                  (if (eq? _msg0 'end-point)
                     _end0
                     (if (eq? _msg0 'midpoint)
                        (midpoint)
                        (error "wrong message"))))))
         dispatch)))
 
(define make-w-vector (lambda args
      (define dimension (lambda ()
            (length args)))
      (define coordinate (lambda (n)
            (if (<change> (let ((__or_res (< n 1))) (if __or_res __or_res (> n (dimension)))) (let ((___or_res0 (< n 1))) (if ___or_res0 ___or_res0 (> n (dimension)))))
               (error "coordinate is out of range")
               (list-ref args (- n 1)))))
      (define add (lambda (w-vector)
            (define loop (<change>
                  (lambda (ctr res)
                     (if (= ctr 0)
                        (apply make-w-vector res)
                        (loop (- ctr 1) (cons (+ (coordinate ctr) ((w-vector 'coordinate) ctr)) res))))
                  (lambda (_ctr0 _res0)
                     (if (= _ctr0 0)
                        (apply make-w-vector _res0)
                        (loop (- _ctr0 1) (cons (+ (coordinate _ctr0) ((w-vector 'coordinate) _ctr0)) _res0))))))
            (loop (dimension) ())))
      (define dispatch (lambda (msg)
            (if (eq? msg 'dimension)
               (dimension)
               (if (eq? msg 'coordinate)
                  coordinate
                  (if (eq? msg 'add) add (error "wrong message"))))))
      dispatch))
 
(define make-polynome (lambda coefficients
      (let ((polynome (apply make-w-vector coefficients)))
         (define coefficient (lambda (index)
               ((polynome 'coordinate) index)))
         (define order (<change>
               (lambda ()
                  (- (polynome 'dimension) 1))
               (lambda ()
                  (- (polynome 'dimension) 1))))
         (define dispatch (<change>
               (lambda (msg)
                  (if (eq? msg 'order)
                     (order)
                     (if (eq? msg 'coefficient)
                        coefficient
                        (error "wrong message"))))
               (lambda (_msg0)
                  (if (eq? _msg0 'order)
                     (order)
                     (if (eq? _msg0 'coefficient)
                        coefficient
                        (error "wrong message"))))))
         dispatch)))
 
(define point1 (make-point 6 10))
 
(define point2 (make-point 10 20))
 
(define segment (make-segment point1 point2))
 
(define midpoint (segment 'midpoint))
 
(define w-vector1 (make-w-vector 1 2 3))
 
(define w-vector2 (make-w-vector 4 5 6))
 
(define polynome (make-polynome 1 2 3))
 
(if (= (point1 'x-value) 6)
   (if (= ((segment 'start-point) 'y-value) 10)
      (if (= (midpoint 'x-value) 8)
         (if (= ((w-vector1 'coordinate) 2) 2)
            (if (= ((w-vector2 'coordinate) 1) 4)
               (if (= ((((w-vector1 'add) w-vector2) 'coordinate) 1) 5)
                  (if (= (polynome 'order) 2)
                     (= ((polynome 'coefficient) 2) 2)
                     #f)
                  #f)
               #f)
            #f)
         #f)
      #f)
   #f)
 
