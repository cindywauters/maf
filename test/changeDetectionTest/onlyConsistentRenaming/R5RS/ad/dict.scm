;; renamed lambdas/lets: 1
 
(define create-dictionary (lambda ()
      (<change>
         (let ((content ()))
            (define empty? (lambda ()
                  (null? content)))
            (define insert (lambda (key info)
                  (let ((temp (assoc key content)))
                     (if temp
                        (set-cdr! temp info)
                        (set! content (cons (cons key info) content))))
                  #t))
            (define delete (lambda (key)
                  (define remove-iter (lambda (current prev)
                        (if (null? current)
                           #f
                           (if (eq? key (caar current))
                              (begin
                                 (if (null? prev)
                                    (set! content (cdr content))
                                    (set-cdr! prev (cdr current)))
                                 #t)
                              (remove-iter (cdr current) current)))))
                  (remove-iter content ())))
            (define lookup (lambda (key)
                  (let ((temp (assoc key content)))
                     (if temp (cdr temp) #f))))
            (define map (lambda (a-function)
                  (define map-iter (lambda (the-current result)
                        (if (null? the-current)
                           (reverse result)
                           (map-iter (cdr the-current) (cons (a-function (caar the-current) (cdar the-current)) result)))))
                  (map-iter content ())))
            (define foreach (lambda (a-action)
                  (define foreach-iter (lambda (the-current)
                        (if (null? the-current)
                           #t
                           (begin
                              (a-action (caar the-current) (cdar the-current))
                              (foreach-iter (cdr the-current))))))
                  (foreach-iter content)
                  #t))
            (define display-dict (lambda ()
                  (foreach (lambda (key info) (display key) (display " ") (display info) (newline)))))
            (define dispatch (lambda (msg args)
                  (if (eq? msg 'empty?)
                     (empty?)
                     (if (eq? msg 'insert)
                        (insert (car args) (cadr args))
                        (if (eq? msg 'delete)
                           (delete (car args))
                           (if (eq? msg 'lookup)
                              (lookup (car args))
                              (if (eq? msg 'map)
                                 (map (car args))
                                 (if (eq? msg 'foreach)
                                    (foreach (car args))
                                    (if (eq? msg 'display)
                                       (display-dict)
                                       (error "unknown request -- create-dictionary" msg))))))))))
            dispatch)
         (let ((_content0 ()))
            (define empty? (lambda ()
                  (null? _content0)))
            (define insert (lambda (_key0 _info0)
                  (let ((_temp0 (assoc _key0 _content0)))
                     (if _temp0
                        (set-cdr! _temp0 _info0)
                        (set! _content0 (cons (cons _key0 _info0) _content0))))
                  #t))
            (define delete (lambda (_key1)
                  (define remove-iter (lambda (_current0 _prev0)
                        (if (null? _current0)
                           #f
                           (if (eq? _key1 (caar _current0))
                              (begin
                                 (if (null? _prev0)
                                    (set! _content0 (cdr _content0))
                                    (set-cdr! _prev0 (cdr _current0)))
                                 #t)
                              (remove-iter (cdr _current0) _current0)))))
                  (remove-iter _content0 ())))
            (define lookup (lambda (_key2)
                  (let ((_temp1 (assoc _key2 _content0)))
                     (if _temp1 (cdr _temp1) #f))))
            (define map (lambda (_a-function0)
                  (define map-iter (lambda (_the-current0 _result0)
                        (if (null? _the-current0)
                           (reverse _result0)
                           (map-iter
                              (cdr _the-current0)
                              (cons (_a-function0 (caar _the-current0) (cdar _the-current0)) _result0)))))
                  (map-iter _content0 ())))
            (define foreach (lambda (_a-action0)
                  (define foreach-iter (lambda (_the-current1)
                        (if (null? _the-current1)
                           #t
                           (begin
                              (_a-action0 (caar _the-current1) (cdar _the-current1))
                              (foreach-iter (cdr _the-current1))))))
                  (foreach-iter _content0)
                  #t))
            (define display-dict (lambda ()
                  (foreach (lambda (_key3 _info1) (display _key3) (display " ") (display _info1) (newline)))))
            (define dispatch (lambda (_msg0 _args0)
                  (if (eq? _msg0 'empty?)
                     (empty?)
                     (if (eq? _msg0 'insert)
                        (insert (car _args0) (cadr _args0))
                        (if (eq? _msg0 'delete)
                           (delete (car _args0))
                           (if (eq? _msg0 'lookup)
                              (lookup (car _args0))
                              (if (eq? _msg0 'map)
                                 (map (car _args0))
                                 (if (eq? _msg0 'foreach)
                                    (foreach (car _args0))
                                    (if (eq? _msg0 'display)
                                       (display-dict)
                                       (error "unknown request -- create-dictionary" _msg0))))))))))
            dispatch))))
 
(define nl->fr (create-dictionary))
 
(nl->fr 'insert (__toplevel_cons 'fiets (__toplevel_cons (__toplevel_cons 'bicyclette ()) ())))
 
(nl->fr 'insert (__toplevel_cons 'auto (__toplevel_cons (__toplevel_cons 'voiture ()) ())))
 
(nl->fr 'insert (__toplevel_cons 'huis (__toplevel_cons (__toplevel_cons 'maison ()) ())))
 
(nl->fr 'insert (__toplevel_cons 'vrachtwagen (__toplevel_cons (__toplevel_cons 'camion ()) ())))
 
(nl->fr 'insert (__toplevel_cons 'tientonner (__toplevel_cons (__toplevel_cons 'camion ()) ())))
 
(nl->fr 'lookup (__toplevel_cons 'fiets ()))
 
(nl->fr 'display ())
 
(define fr->eng (create-dictionary))
 
(fr->eng 'insert (__toplevel_cons 'bicyclette (__toplevel_cons (__toplevel_cons 'bike ()) ())))
 
(fr->eng 'insert (__toplevel_cons 'voiture (__toplevel_cons (__toplevel_cons 'car ()) ())))
 
(fr->eng
   'insert
   (__toplevel_cons 'maison (__toplevel_cons (__toplevel_cons 'house (__toplevel_cons 'home ())) ())))
 
(fr->eng 'insert (__toplevel_cons 'camion (__toplevel_cons (__toplevel_cons 'truck ()) ())))
 
(fr->eng 'lookup (__toplevel_cons 'bicyclette ()))
 
#t
 
