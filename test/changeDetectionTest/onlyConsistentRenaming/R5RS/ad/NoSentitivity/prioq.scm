;; renamed lambdas/lets: 1
 
(define true #t)
 
(define false #f)
 
(define make-item (lambda (priority element)
      (cons priority element)))
 
(define get-priority (lambda (item)
      (car item)))
 
(define get-element (lambda (item)
      (cdr item)))
 
(define create-priority-queue (<change>
      (lambda ()
         (let ((front (cons 'boe ())))
            (define content (lambda ()
                  (cdr front)))
            (define insert-after! (lambda (cell item)
                  (let ((new-cell (cons item ())))
                     (set-cdr! new-cell (cdr cell))
                     (set-cdr! cell new-cell))))
            (define find-prev-cell (lambda (priority)
                  (define find-iter (lambda (rest prev)
                        (if (null? rest)
                           prev
                           (if (> (get-priority (car rest)) priority)
                              (find-iter (cdr rest) rest)
                              prev))))
                  (find-iter (content) front)))
            (define empty? (lambda ()
                  (null? (content))))
            (define enqueue (lambda (priority element)
                  (insert-after! (find-prev-cell priority) (make-item priority element))
                  true))
            (define dequeue (lambda ()
                  (if (null? (content))
                     false
                     (let ((temp (car (content))))
                        (set-cdr! front (cdr (content)))
                        (get-element temp)))))
            (define serve (lambda ()
                  (if (null? (content))
                     false
                     (get-element (car (content))))))
            (define dispatch (lambda (m)
                  (if (eq? m 'empty?)
                     empty?
                     (if (eq? m 'enqueue)
                        enqueue
                        (if (eq? m 'dequeue)
                           dequeue
                           (if (eq? m 'serve)
                              serve
                              (error "unknown request
                 -- create-priority-queue" m)))))))
            dispatch))
      (lambda ()
         (let ((_front0 (cons 'boe ())))
            (define content (lambda ()
                  (cdr _front0)))
            (define insert-after! (lambda (_cell0 _item0)
                  (let ((_new-cell0 (cons _item0 ())))
                     (set-cdr! _new-cell0 (cdr _cell0))
                     (set-cdr! _cell0 _new-cell0))))
            (define find-prev-cell (lambda (_priority0)
                  (define find-iter (lambda (_rest0 _prev0)
                        (if (null? _rest0)
                           _prev0
                           (if (> (get-priority (car _rest0)) _priority0)
                              (find-iter (cdr _rest0) _rest0)
                              _prev0))))
                  (find-iter (content) _front0)))
            (define empty? (lambda ()
                  (null? (content))))
            (define enqueue (lambda (_priority1 _element0)
                  (insert-after! (find-prev-cell _priority1) (make-item _priority1 _element0))
                  true))
            (define dequeue (lambda ()
                  (if (null? (content))
                     false
                     (let ((_temp0 (car (content))))
                        (set-cdr! _front0 (cdr (content)))
                        (get-element _temp0)))))
            (define serve (lambda ()
                  (if (null? (content))
                     false
                     (get-element (car (content))))))
            (define dispatch (lambda (_m0)
                  (if (eq? _m0 'empty?)
                     empty?
                     (if (eq? _m0 'enqueue)
                        enqueue
                        (if (eq? _m0 'dequeue)
                           dequeue
                           (if (eq? _m0 'serve)
                              serve
                              (error "unknown request
                 -- create-priority-queue" _m0)))))))
            dispatch))))
 
(define pq (create-priority-queue))
 
((pq 'enqueue) 66 'Patrick)
 
((pq 'enqueue) -106 'Octo)
 
((pq 'enqueue) 0 'Sandy)
 
((pq 'enqueue) 89 'Spongebob)
 
((pq 'dequeue))
 
(equal? ((pq 'dequeue)) 'Patrick)
 
