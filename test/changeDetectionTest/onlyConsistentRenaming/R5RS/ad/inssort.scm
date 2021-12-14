;; renamed lambdas/lets: 1
 
(define insertion-sort (lambda (vector)
      (<change>
         (let ((high (- (vector-length vector) 1)))
            (define shift-left (lambda (vector index)
                  (vector-set! vector (- index 1) (vector-ref vector index))))
            (define insert-sort-iter (lambda (index1)
                  (define insert (lambda (index1)
                        (let ((insert-value (vector-ref vector (- index1 1))))
                           (define insert-iter (lambda (index2)
                                 (if (if (<= index2 high) (< (vector-ref vector index2) insert-value) #f)
                                    (begin
                                       (shift-left vector index2)
                                       (insert-iter (+ index2 1)))
                                    (vector-set! vector (- index2 1) insert-value))))
                           (insert-iter index1))))
                  (if (> index1 0)
                     (begin
                        (insert index1)
                        (insert-sort-iter (- index1 1)))
                     #f)))
            (insert-sort-iter high))
         (let ((_high0 (- (vector-length vector) 1)))
            (define shift-left (lambda (_vector0 _index0)
                  (vector-set! _vector0 (- _index0 1) (vector-ref _vector0 _index0))))
            (define insert-sort-iter (lambda (_index10)
                  (define insert (lambda (_index11)
                        (let ((_insert-value0 (vector-ref vector (- _index11 1))))
                           (define insert-iter (lambda (_index20)
                                 (if (if (<= _index20 _high0) (< (vector-ref vector _index20) _insert-value0) #f)
                                    (begin
                                       (shift-left vector _index20)
                                       (insert-iter (+ _index20 1)))
                                    (vector-set! vector (- _index20 1) _insert-value0))))
                           (insert-iter _index11))))
                  (if (> _index10 0)
                     (begin
                        (insert _index10)
                        (insert-sort-iter (- _index10 1)))
                     #f)))
            (insert-sort-iter _high0)))))
 
(define vect (vector 5 2 7 1 0 9 8 6 3 4))
 
(insertion-sort vect)
 
(equal? vect (vector 0 1 2 3 4 5 6 7 8 9))
 
