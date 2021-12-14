;; renamed lambdas/lets: 3
 
(define false #f)
 
(define true #t)
 
(define create-stack (lambda (eq-fnct)
      (let ((content ()))
         (define empty? (lambda ()
               (null? content)))
         (define push (lambda (element)
               (set! content (cons element content))
               #t))
         (define pop (lambda ()
               (if (null? content)
                  #f
                  (let ((temp (car content)))
                     (set! content (cdr content))
                     temp))))
         (define top (lambda ()
               (if (null? content) #f (car content))))
         (define is-in (lambda (element)
               (if (member element content) #t #f)))
         (define dispatch (<change>
               (lambda (m)
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
                                 (error "unknown request -- create-stack" m)))))))
               (lambda (_m0)
                  (if (eq? _m0 'empty?)
                     empty?
                     (if (eq? _m0 'push)
                        push
                        (if (eq? _m0 'pop)
                           pop
                           (if (eq? _m0 'top)
                              top
                              (if (eq? _m0 'is-in)
                                 is-in
                                 (error "unknown request -- create-stack" _m0)))))))))
         dispatch)))
 
(<change>
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
   (let ((_stack0 (create-stack =)))
      (if ((_stack0 'empty?))
         (if (begin ((_stack0 'push) 13) (not ((_stack0 'empty?))))
            (if ((_stack0 'is-in) 13)
               (if (= ((_stack0 'top)) 13)
                  (begin
                     ((_stack0 'push) 14)
                     (= ((_stack0 'pop)) 14))
                  #f)
               #f)
            #f)
         #f)))
 
