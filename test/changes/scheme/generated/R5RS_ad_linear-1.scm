; Changes:
; * removed: 1
; * added: 1
; * swaps: 1
; * negated predicates: 0
; * swapped branches: 1
; * calls to id fun: 5
(letrec ((create-hash-table (lambda (size hash-fct)
                              (let ((content (make-vector size 0))
                                    (same? =))
                                 (letrec ((next-index (lambda (index)
                                                        (remainder (+ index 1) size)))
                                          (make-item (lambda (status key info)
                                                       (list status key info)))
                                          (get-status (lambda (item)
                                                        (car item)))
                                          (set-status! (lambda (item status)
                                                         (set-car! item status)))
                                          (get-key (lambda (item)
                                                     (cadr item)))
                                          (set-key! (lambda (item key)
                                                      (set-car! (cdr item) key)))
                                          (get-info (lambda (item)
                                                      (<change>
                                                         (caddr item)
                                                         ((lambda (x) x) (caddr item)))))
                                          (set-info! (lambda (item info)
                                                       (set-car! (cddr item) info)))
                                          (insert (lambda (key info)
                                                    (letrec ((rehash-iter (lambda (current)
                                                                            (let* ((item (vector-ref content current))
                                                                                   (status (get-status item)))
                                                                               (if (not (eq? status 'data))
                                                                                  (begin
                                                                                     (set-status! item 'data)
                                                                                     (set-key! item key)
                                                                                     (set-info! item info))
                                                                                  (if (same? key (get-key item))
                                                                                     (set-info! item info)
                                                                                     (rehash-iter (next-index current))))))))
                                                       (<change>
                                                          (rehash-iter (hash-fct key))
                                                          ((lambda (x) x) (rehash-iter (hash-fct key)))))))
                                          (find-item (lambda (key)
                                                       (<change>
                                                          ()
                                                          current)
                                                       (letrec ((rehash-iter (lambda (current)
                                                                               (let* ((item (vector-ref content current))
                                                                                      (status (get-status item)))
                                                                                  (if (eq? status 'data)
                                                                                     (<change>
                                                                                        (if (same? key (get-key item))
                                                                                           item
                                                                                           (rehash-iter (next-index current)))
                                                                                        (if (eq? status 'empty)
                                                                                           #f
                                                                                           (rehash-iter (next-index current))))
                                                                                     (<change>
                                                                                        (if (eq? status 'empty)
                                                                                           #f
                                                                                           (rehash-iter (next-index current)))
                                                                                        (if (same? key (get-key item))
                                                                                           item
                                                                                           (rehash-iter (next-index current)))))))))
                                                          (rehash-iter (hash-fct key)))))
                                          (retrieve (lambda (key)
                                                      (let ((temp (find-item key)))
                                                         (if temp (get-info temp) #f))))
                                          (delete (lambda (key)
                                                    (<change>
                                                       (let ((temp (find-item key)))
                                                          (if temp
                                                             (begin
                                                                (set-status! temp 'deleted)
                                                                #t)
                                                             #f))
                                                       ((lambda (x) x) (let ((temp (find-item key))) (if temp (begin (set-status! temp 'deleted) #t) #f))))))
                                          (display-table (lambda ()
                                                           (<change>
                                                              (let ((stop (vector-length content)))
                                                                 (letrec ((iter (lambda (current)
                                                                                  (if (< current stop)
                                                                                     (begin
                                                                                        (display current)
                                                                                        (display "  ")
                                                                                        (display (vector-ref content current))
                                                                                        (newline)
                                                                                        (iter (+ current 1)))
                                                                                     #f))))
                                                                    (iter 0)))
                                                              ((lambda (x) x)
                                                                 (let ((stop (vector-length content)))
                                                                    (letrec ((iter (lambda (current)
                                                                                     (if (< current stop)
                                                                                        (begin
                                                                                           (display current)
                                                                                           (display "  ")
                                                                                           (display (vector-ref content current))
                                                                                           (newline)
                                                                                           (iter (+ current 1)))
                                                                                        #f))))
                                                                       (iter 0)))))))
                                          (dispatch (lambda (msg . args)
                                                      (<change>
                                                         (if (eq? msg 'insert)
                                                            (insert (car args) (cadr args))
                                                            (if (eq? msg 'delete)
                                                               (delete (car args))
                                                               (if (eq? msg 'retrieve)
                                                                  (retrieve (car args))
                                                                  (if (eq? msg 'display)
                                                                     (display-table)
                                                                     (error "unknown request -- create-hash-table" msg)))))
                                                         ((lambda (x) x)
                                                            (if (eq? msg 'insert)
                                                               (insert (car args) (cadr args))
                                                               (if (eq? msg 'delete)
                                                                  (delete (car args))
                                                                  (if (eq? msg 'retrieve)
                                                                     (retrieve (car args))
                                                                     (if (eq? msg 'display)
                                                                        (display-table)
                                                                        (error "unknown request -- create-hash-table" msg))))))))))
                                    (letrec ((__do_loop (lambda (index)
                                                          (if (negative? index)
                                                             'done
                                                             (begin
                                                                (vector-set! content index (make-item 'empty () ()))
                                                                (__do_loop (- index 1)))))))
                                       (__do_loop (- (vector-length content) 1)))
                                    dispatch))))
         (table (create-hash-table 13 (lambda (key) (modulo key 13)))))
   (<change>
      (table 'insert 1 79)
      (table 'insert 4 69))
   (<change>
      (table 'insert 4 69)
      (table 'insert 1 79))
   (table 'insert 14 98)
   (table 'insert 7 72)
   (<change>
      (table 'insert 27 14)
      ())
   (table 'insert 11 50)
   (table 'display))