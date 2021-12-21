;; renamed lambdas/lets: 7
 
(define true #t)
 
(define false #f)
 
(define make-item (lambda (priority element)
      (cons priority element)))
 
(define get-priority (lambda (item)
      (car item)))
 
(define get-element (lambda (item)
      (cdr item)))
 
(define create-priority-queue (lambda ()
      (let ((front (cons 'boe ())))
         (define content (<change>
               (lambda ()
                  (cdr front))
               (lambda ()
                  (cdr front))))
         (define insert-after! (lambda (cell item)
               (<change>
                  (let ((new-cell (cons item ())))
                     (set-cdr! new-cell (cdr cell))
                     (set-cdr! cell new-cell))
                  (let ((_new-cell0 (cons item ())))
                     (set-cdr! _new-cell0 (cdr cell))
                     (set-cdr! cell _new-cell0)))))
         (define find-prev-cell (lambda (priority)
               (define find-iter (<change>
                     (lambda (rest prev)
                        (if (null? rest)
                           prev
                           (if (> (get-priority (car rest)) priority)
                              (find-iter (cdr rest) rest)
                              prev)))
                     (lambda (_rest0 _prev0)
                        (if (null? _rest0)
                           _prev0
                           (if (> (get-priority (car _rest0)) priority)
                              (find-iter (cdr _rest0) _rest0)
                              _prev0)))))
               (find-iter (content) front)))
         (define empty? (<change>
               (lambda ()
                  (null? (content)))
               (lambda ()
                  (null? (content)))))
         (define enqueue (lambda (priority element)
               (insert-after! (find-prev-cell priority) (make-item priority element))
               true))
         (define dequeue (<change>
               (lambda ()
                  (if (null? (content))
                     false
                     (let ((temp (car (content))))
                        (set-cdr! front (cdr (content)))
                        (get-element temp))))
               (lambda ()
                  (if (null? (content))
                     false
                     (let ((_temp0 (car (content))))
                        (set-cdr! front (cdr (content)))
                        (get-element _temp0))))))
         (define serve (<change>
               (lambda ()
                  (if (null? (content))
                     false
                     (get-element (car (content)))))
               (lambda ()
                  (if (null? (content))
                     false
                     (get-element (car (content)))))))
         (define dispatch (<change>
               (lambda (m)
                  (if (eq? m 'empty?)
                     empty?
                     (if (eq? m 'enqueue)
                        enqueue
                        (if (eq? m 'dequeue)
                           dequeue
                           (if (eq? m 'serve)
                              serve
                              (error "unknown request
                 -- create-priority-queue" m))))))
               (lambda (_m0)
                  (if (eq? _m0 'empty?)
                     empty?
                     (if (eq? _m0 'enqueue)
                        enqueue
                        (if (eq? _m0 'dequeue)
                           dequeue
                           (if (eq? _m0 'serve)
                              serve
                              (error "unknown request
                 -- create-priority-queue" _m0))))))))
         dispatch)))
 
(define pq (create-priority-queue))
 
((pq 'enqueue) 66 'Patrick)
 
((pq 'enqueue) -106 'Octo)
 
((pq 'enqueue) 0 'Sandy)
 
((pq 'enqueue) 89 'Spongebob)
 
((pq 'dequeue))
 
(equal? ((pq 'dequeue)) 'Patrick)
 
