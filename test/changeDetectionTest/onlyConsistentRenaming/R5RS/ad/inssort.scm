;; renamed lambdas/lets: 1
 
(define insertion-sort (lambda (vector)
      (let ((high (- (vector-length vector) 1)))
         (define shift-left (lambda (vector index)
               (vector-set! vector (- index 1) (vector-ref vector index))))
         (define insert-sort-iter (lambda (index1)
               (define insert (<change>
                     (lambda (index1)
                        (let ((insert-value (vector-ref vector (- index1 1))))
                           (define insert-iter (lambda (index2)
                                 (if (if (<= index2 high) (< (vector-ref vector index2) insert-value) #f)
                                    (begin
                                       (shift-left vector index2)
                                       (insert-iter (+ index2 1)))
                                    (vector-set! vector (- index2 1) insert-value))))
                           (insert-iter index1)))
                     (lambda (_index10)
                        (let ((_insert-value0 (vector-ref vector (- _index10 1))))
                           (define insert-iter (lambda (_index20)
                                 (if (if (<= _index20 high) (< (vector-ref vector _index20) _insert-value0) #f)
                                    (begin
                                       (shift-left vector _index20)
                                       (insert-iter (+ _index20 1)))
                                    (vector-set! vector (- _index20 1) _insert-value0))))
                           (insert-iter _index10)))))
               (if (> index1 0)
                  (begin
                     (insert index1)
                     (insert-sort-iter (- index1 1)))
                  #f)))
         (insert-sort-iter high))))
 
(define vect (vector 5 2 7 1 0 9 8 6 3 4))
 
(insertion-sort vect)
 
(equal? vect (vector 0 1 2 3 4 5 6 7 8 9))
 
