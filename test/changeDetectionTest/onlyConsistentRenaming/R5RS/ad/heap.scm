;; renamed lambdas/lets: 2
 
(define my-++ (lambda (n)
      (+ n 1)))
 
(define my--- (lambda (n)
      (- n 1)))
 
(define false #f)
 
(define true #t)
 
(define nil ())
 
(define key (lambda (x)
      x))
 
(define make-heap (lambda (a-vector nr-of-elements)
      (define iter (<change>
            (lambda (index)
               (if (> index 0)
                  (begin
                     (sift-down a-vector index nr-of-elements)
                     (iter (my--- index)))
                  #f))
            (lambda (_index0)
               (if (> _index0 0)
                  (begin
                     (sift-down a-vector _index0 nr-of-elements)
                     (iter (my--- _index0)))
                  #f))))
      (iter (quotient nr-of-elements 2))))
 
(define sift-down (lambda (heap from to)
      (define smallest-child (lambda (parent)
            (let* ((child1 (* 2 parent))
                   (child2 (my-++ child1)))
               (if (> child1 to)
                  false
                  (if (> child2 to)
                     child1
                     (if (< (key (vector-ref heap child1)) (key (vector-ref heap child2)))
                        child1
                        child2))))))
      (define iter (lambda (parent)
            (let ((child (smallest-child parent)))
               (if child
                  (if (> (key (vector-ref heap parent)) (key (vector-ref heap child)))
                     (begin
                        (swap heap child parent)
                        (iter child))
                     #f)
                  #f))))
      (iter from)))
 
(define swap (lambda (a-vector i1 i2)
      (let ((temp (vector-ref a-vector i1)))
         (vector-set! a-vector i1 (vector-ref a-vector i2))
         (vector-set! a-vector i2 temp))))
 
(define sift-up (lambda (heap from)
      (define iter (lambda (child)
            (<change>
               (let ((parent (quotient child 2)))
                  (if (> parent 0)
                     (if (> (key (vector-ref heap parent)) (key (vector-ref heap child)))
                        (begin
                           (swap heap child parent)
                           (iter parent))
                        #f)
                     #f))
               (let ((_parent0 (quotient child 2)))
                  (if (> _parent0 0)
                     (if (> (key (vector-ref heap _parent0)) (key (vector-ref heap child)))
                        (begin
                           (swap heap child _parent0)
                           (iter _parent0))
                        #f)
                     #f)))))
      (iter from)))
 
(define create-heap (lambda (size)
      (cons 0 (make-vector (my-++ size)))))
 
(define is-empty? (lambda (heap)
      (eq? (car heap) 0)))
 
(define insert (lambda (heap item)
      (let* ((content (cdr heap))
             (new-nr-of-elements (my-++ (car heap)))
             (size (my--- (vector-length content))))
         (display "insert    ")
         (if (> new-nr-of-elements size)
            false
            (begin
               (vector-set! content new-nr-of-elements item)
               (sift-up content new-nr-of-elements)
               (set-car! heap new-nr-of-elements)))
         (display heap)
         (newline))))
 
(define v (vector 'lol 5 8 1 3 9 10 2 0))
 
(make-heap v 8)
 
(equal? v (vector 'lol 0 3 1 5 9 10 2 8))
 
