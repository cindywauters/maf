;; renamed lambdas/lets: 3

(define find-last (<change>
      (lambda (lijst)
         (if (null? lijst)
            (error "find-last -- lijst heeft geen laatste element")
            (let ((next (cdr lijst)))
               (if (null? next) lijst (find-last next)))))
      (lambda (_lijst0)
         (if (null? _lijst0)
            (error "find-last -- lijst heeft geen laatste element")
            (let ((_next0 (cdr _lijst0)))
               (if (null? _next0) _lijst0 (find-last _next0)))))))

(define flatten! ;(<change>
      (lambda (lijst)
         (if (null? lijst)
            ()
            (let* ((sublist (car lijst))
                   (restlist (flatten! (cdr lijst))))
               (if (null? sublist)
                  restlist
                  (let ((last (find-last sublist)))
                     (set-cdr! last restlist)
                     sublist))))))
   ;   (lambda (_lijst0)
    ;     (if (null? _lijst0)
     ;       ()
      ;      (let* ((_sublist0 (car _lijst0))
       ;            (_restlist0 (flatten! (cdr _lijst0))))
        ;       (if (null? _sublist0)
         ;         _restlist0
          ;        (let ((_last0 (find-last _sublist0)))
           ;          (set-cdr! _last0 _restlist0)
            ;         _sublist0)))))))

(define atom? (lambda (x)
      (not (pair? x))))

(define flatten2!;  (<change>
      (lambda (lijst)
         (let ((hulpcel (cons 'dummy lijst)))
            (define flatten-aux! (lambda (prev current)
                  (if (null? current)
                     (cdr hulpcel)
                     (if (null? (car current))
                        (begin
                           (set-cdr! prev (cdr current))
                           (flatten-aux! prev (cdr current)))
                        (if (pair? (car current))
                           (begin
                              (set-cdr! prev (flatten2! (car current)))
                              (flatten-aux! (find-last prev) (cdr current)))
                           (if (null? (cdr prev))
                              (begin
                                 (set-cdr! prev current)
                                 (flatten-aux! (cdr prev) (cdr current)))
                              (if (atom? (car current))
                                 (flatten-aux! (cdr prev) (cdr current))
                                 #f)))))))
            (flatten-aux! hulpcel lijst)
            (cdr hulpcel))))
 ;     (lambda (_lijst0)
  ;       (let ((_hulpcel0 (cons 'dummy _lijst0)))
   ;         (define flatten-aux! (lambda (_prev0 _current0)
    ;              (if (null? _current0)
     ;                (cdr _hulpcel0)
      ;               (if (null? (car _current0))
       ;                 (begin
        ;                   (set-cdr! _prev0 (cdr _current0))
         ;                  (flatten-aux! _prev0 (cdr _current0)))
          ;              (if (pair? (car _current0))
           ;                (begin
            ;                  (set-cdr! _prev0 (flatten2! (car _current0)))
             ;                 (flatten-aux! (find-last _prev0) (cdr _current0)))
              ;;             (if (null? (cdr _prev0))
                ;              (begin
                 ;                (set-cdr! _prev0 _current0)
                     ;            (flatten-aux! (cdr _prev0) (cdr _current0)))
                  ;            (if (atom? (car _current0))
                   ;              (flatten-aux! (cdr _prev0) (cdr _current0))
         ;           ;;             #f)))))))
          ;  (flatten-aux! _hulpcel0 _lijst0)
           ; (cdr _hulpcel0))))

(define res ;(if (equal? (flatten! (cons (cons 1 (cons 2 ())) (cons (cons 3 (cons 4 (cons 5 ()))) (cons (cons 6 ()) (cons (cons 7 (cons 8 ())) ()))))) (cons 1 (cons 2 (cons 3 (cons 4 (cons 5 (cons 6 (cons 7 (cons 8 ())))))))))
     ; (if (equal? (flatten! (cons () (cons (cons 1 (cons 2 ())) (cons (cons 3 (cons 4 (cons 5 ()))) (cons () (cons (cons 6 ()) (cons (cons 7 (cons 8 ())) ()))))))) (cons 1 (cons 2 (cons 3 (cons 4 (cons 5 (cons 6 (cons 7 (cons 8 ())))))))))
      ;   (if (equal? (flatten2! (cons (cons 1 (cons (cons 2 (cons 3 ())) (cons 4 ()))) (cons 5 (cons 6 (cons (cons 7 (cons 8 ())) ()))))) (cons 1 (cons 2 (cons 3 (cons 4 (cons 5 (cons 6 (cons 7 (cons 8 ())))))))))
       ;     (if (equal? (flatten2! (cons (cons 1 (cons 2 ())) (cons (cons 3 (cons 4 (cons 5 ()))) (cons (cons 6 ()) (cons (cons 7 (cons 8 ())) ()))))) (cons 1 (cons 2 (cons 3 (cons 4 (cons 5 (cons 6 (cons 7 (cons 8 ())))))))))
               (if (equal? (flatten2! (cons () (cons (cons 1 (cons 2 ())) (cons (cons 3 ()) ()))))(cons 4 (cons 5 ())))
                  (equal?
                     (flatten2!
                        (cons
                           6
                           (cons
                              7  ())))
                     (cons
                        8
                        (cons
                           9 ())))
                 #f))
      ;         #f)
       ;     #f)
        ; #f)
      ;#f))

res

