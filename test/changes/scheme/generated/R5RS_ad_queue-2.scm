; Changes:
; * removed: 0
; * added: 2
; * swaps: 1
; * negated predicates: 1
(letrec ((create-queue (lambda ()
                         (let ((front ())
                               (rear ()))
                            (letrec ((empty? (lambda ()
                                               (null? front)))
                                     (enqueue (lambda (element-list)
                                                (if (null? element-list)
                                                   #t
                                                   (begin
                                                      (if (null? front)
                                                         (begin
                                                            (set! front (list (car element-list)))
                                                            (set! rear front))
                                                         (begin
                                                            (set-cdr! rear (list (car element-list)))
                                                            (set! rear (cdr rear))))
                                                      (<change>
                                                         ()
                                                         (car element-list))
                                                      (enqueue (cdr element-list))))))
                                     (dequeue (lambda ()
                                                (if (null? front)
                                                   (error "Can't front. The queue is empty.")
                                                   (let ((temp (car front)))
                                                      (<change>
                                                         (set! front (cdr front))
                                                         temp)
                                                      (<change>
                                                         temp
                                                         (set! front (cdr front)))))))
                                     (serve (lambda ()
                                              (if (null? front)
                                                 (error "Can't serve. The queue is empty.")
                                                 (car front))))
                                     (dispatch (lambda (msg . args)
                                                 (<change>
                                                    ()
                                                    (eq? msg 'serve))
                                                 (if (eq? msg 'empty?)
                                                    (empty?)
                                                    (if (eq? msg 'enqueue)
                                                       (enqueue args)
                                                       (if (eq? msg 'dequeue)
                                                          (dequeue)
                                                          (if (eq? msg 'serve)
                                                             (serve)
                                                             (error "unknown request -- create-queue" msg))))))))
                               dispatch))))
         (queue (create-queue)))
   (queue 'enqueue 1 2 3)
   (if (not (queue 'empty?))
      (if (= 1 (queue 'dequeue))
         (if (= 2 (queue 'dequeue))
            (if (= 3 (queue 'serve))
               (if (<change> (= 3 (queue 'dequeue)) (not (= 3 (queue 'dequeue))))
                  (queue 'empty?)
                  #f)
               #f)
            #f)
         #f)
      #f))