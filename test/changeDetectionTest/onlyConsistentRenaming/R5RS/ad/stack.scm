;; renamed lambdas/lets: 2
 
(define false #f)
 
(define true #t)
 
(define create-stack (lambda (eq-fnct)
      (let ((content ()))
         (define empty? (lambda ()
               (null? content)))
         (define push (lambda (element)
               (set! content (cons element content))
               #t))
         (define pop
               (lambda ()
                  (if (null? content)
                     #f
                     (let ((temp (car content)))
                        (set! content (cdr content))
                        temp))))
         (define top (<change>
               (lambda ()
                  (if (null? content) #f (car content)))
               (lambda ()
                  (if (null? content) #f (car content)))))
         (define is-in (lambda (element)
               (if (member element content) #t #f)))
         (define dispatch (lambda (m)
               (if (eq? m 'empty?)
                  empty?
                  (if (eq? m 'push)
                     push
                     (if (eq? m 'pop)
                        pop
                        (if (eq? m 'top)
                           top
                           (if (eq? m 'is-in)
                              is-in
                              (error "unknown request -- create-stack" m))))))))
         dispatch)))
 
 (let ((stack (create-stack =)))
    (if ((stack 'empty?))
      (if (begin ((stack 'push) 13) (not ((stack 'empty?))))
          (if ((stack 'is-in) 13)
             (if (= ((stack 'top)) 13)
                (begin
                   ((stack 'push) 14)
                   (= ((stack 'pop)) 14))
                #f)
             #f)
           #f)
        #f))
