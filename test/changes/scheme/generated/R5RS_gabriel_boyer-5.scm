; Changes:
; * removed: 4
; * added: 5
; * swaps: 10
; * negated predicates: 3
(letrec ((*namelist* ())
         (*lastlook* (__toplevel_cons 'xxx (__toplevel_cons () ())))
         (nameprop (lambda (name)
                     (<change>
                        @sensitivity:FA
                        (if (eq? name (car *lastlook*))
                           *lastlook*
                           (let ((pair (assq name *namelist*)))
                              (if pair (set! *lastlook* pair) #f)
                              pair)))
                     (<change>
                        (if (eq? name (car *lastlook*))
                           *lastlook*
                           (let ((pair (assq name *namelist*)))
                              (if pair (set! *lastlook* pair) #f)
                              pair))
                        @sensitivity:FA)))
         (get (lambda (name prop)
                (<change>
                   @sensitivity:FA
                   ())
                (let ((r (nameprop name)))
                   (if (pair? r)
                      (let ((s (assq prop (cdr r))))
                         (if (pair? s) (cdr s) #f))
                      #f))))
         (put (lambda (name prop valu)
                @sensitivity:FA
                (let ((r (nameprop name)))
                   (if (pair? r)
                      (let ((s (assq prop (cdr r))))
                         (if (pair? s)
                            (set-cdr! s valu)
                            (let ((item (cons prop valu)))
                               (set-cdr! r (cons item (cdr r))))))
                      (let ((item (cons prop valu)))
                         (set! *namelist* (cons (cons name (cons item ())) *namelist*)))))
                valu))
         (reinit-prop! (lambda ()
                         @sensitivity:FA
                         (<change>
                            (set! *namelist* ())
                            (set! *lastlook* (__toplevel_cons 'xxx (__toplevel_cons () ()))))
                         (<change>
                            (set! *lastlook* (__toplevel_cons 'xxx (__toplevel_cons () ())))
                            (set! *namelist* ()))))
         (get-null (lambda (name prop)
                     @sensitivity:FA
                     (let ((__or_res (get name prop)))
                        (if __or_res __or_res ()))))
         (unify-subst 0)
         (temp-temp 0)
         (add-lemma (lambda (term)
                      (<change>
                         @sensitivity:No
                         (if (if (pair? term) (if (eq? (car term) 'equal) (pair? (cadr term)) #f) #f)
                            (put (car (cadr term)) 'lemmas (cons term (get-null (car (cadr term)) 'lemmas)))
                            (error 'add-lemma "ADD-LEMMA did not like term:  " term)))
                      (<change>
                         (if (if (pair? term) (if (eq? (car term) 'equal) (pair? (cadr term)) #f) #f)
                            (put (car (cadr term)) 'lemmas (cons term (get-null (car (cadr term)) 'lemmas)))
                            (error 'add-lemma "ADD-LEMMA did not like term:  " term))
                         @sensitivity:No)))
         (add-lemma-lst (lambda (lst)
                          (<change>
                             ()
                             (display lst))
                          (<change>
                             ()
                             #t)
                          (<change>
                             @sensitivity:FA
                             ())
                          (if (null? lst)
                             #t
                             (begin
                                (add-lemma (car lst))
                                (add-lemma-lst (cdr lst))))))
         (apply-subst (lambda (alist term)
                        (<change>
                           ()
                           alist)
                        (<change>
                           @sensitivity:No
                           (if (not (not (pair? term)))
                              (if (begin (set! temp-temp (assq term alist)) temp-temp)
                                 (cdr temp-temp)
                                 term)
                              (cons (car term) (apply-subst-lst alist (cdr term)))))
                        (<change>
                           (if (not (pair? term))
                              (if (begin (set! temp-temp (assq term alist)) temp-temp)
                                 (cdr temp-temp)
                                 term)
                              (cons (car term) (apply-subst-lst alist (cdr term))))
                           @sensitivity:No)))
         (apply-subst-lst (lambda (alist lst)
                            @sensitivity:FA
                            (if (null? lst)
                               ()
                               (cons (apply-subst alist (car lst)) (apply-subst-lst alist (cdr lst))))))
         (falsep (lambda (x lst)
                   @sensitivity:FA
                   (let ((__or_res (equal? x (__toplevel_cons 'f ()))))
                      (if __or_res __or_res (member x lst)))))
         (one-way-unify (lambda (term1 term2)
                          (<change>
                             @sensitivity:FA
                             (begin
                                (set! unify-subst ())
                                (one-way-unify1 term1 term2)))
                          (<change>
                             (begin
                                (set! unify-subst ())
                                (one-way-unify1 term1 term2))
                             @sensitivity:FA)))
         (one-way-unify1 (lambda (term1 term2)
                           (<change>
                              @sensitivity:No
                              (if (not (pair? term2))
                                 (if (begin (set! temp-temp (assq term2 unify-subst)) temp-temp)
                                    (equal? term1 (cdr temp-temp))
                                    (begin
                                       #t
                                       (set! unify-subst (cons (cons term2 term1) unify-subst))))
                                 (if (not (pair? term1))
                                    #f
                                    (if (eq? (car term1) (car term2))
                                       (one-way-unify1-lst (cdr term1) (cdr term2))
                                       #f))))
                           (<change>
                              (if (not (pair? term2))
                                 (if (begin (set! temp-temp (assq term2 unify-subst)) temp-temp)
                                    (equal? term1 (cdr temp-temp))
                                    (begin
                                       (set! unify-subst (cons (cons term2 term1) unify-subst))
                                       #t))
                                 (if (not (pair? term1))
                                    #f
                                    (if (eq? (car term1) (car term2))
                                       (one-way-unify1-lst (cdr term1) (cdr term2))
                                       #f)))
                              @sensitivity:No)))
         (one-way-unify1-lst (lambda (lst1 lst2)
                               @sensitivity:FA
                               (if (null? lst1)
                                  #t
                                  (if (one-way-unify1 (car lst1) (car lst2))
                                     (one-way-unify1-lst (cdr lst1) (cdr lst2))
                                     #f))))
         (rewrite (lambda (term)
                    @sensitivity:No
                    (if (not (pair? term))
                       term
                       (rewrite-with-lemmas (cons (car term) (rewrite-args (cdr term))) (get-null (car term) 'lemmas)))))
         (rewrite-args (lambda (lst)
                         (<change>
                            @sensitivity:No
                            (if (null? lst)
                               ()
                               (cons (rewrite (car lst)) (rewrite-args (cdr lst)))))
                         (<change>
                            (if (null? lst)
                               ()
                               (cons (rewrite (car lst)) (rewrite-args (cdr lst))))
                            @sensitivity:No)))
         (rewrite-with-lemmas (lambda (term lst)
                                (<change>
                                   ()
                                   (display (rewrite-with-lemmas term (cdr lst))))
                                @sensitivity:FA
                                (if (<change> (null? lst) (not (null? lst)))
                                   term
                                   (if (one-way-unify term (cadr (car lst)))
                                      (rewrite (apply-subst unify-subst (caddr (car lst))))
                                      (rewrite-with-lemmas term (cdr lst))))))
         (setup (lambda ()
                  @sensitivity:FA
                  (add-lemma-lst
                     (__toplevel_cons
                        (__toplevel_cons
                           'equal
                           (__toplevel_cons
                              (__toplevel_cons 'compile (__toplevel_cons 'form ()))
                              (__toplevel_cons
                                 (__toplevel_cons
                                    'reverse
                                    (__toplevel_cons
                                       (__toplevel_cons
                                          'codegen
                                          (__toplevel_cons
                                             (__toplevel_cons 'optimize (__toplevel_cons 'form ()))
                                             (__toplevel_cons (__toplevel_cons 'nil ()) ())))
                                       ()))
                                 ())))
                        (__toplevel_cons
                           (__toplevel_cons
                              'equal
                              (__toplevel_cons
                                 (__toplevel_cons 'eqp (__toplevel_cons 'x (__toplevel_cons 'y ())))
                                 (__toplevel_cons
                                    (__toplevel_cons
                                       'equal
                                       (__toplevel_cons
                                          (__toplevel_cons 'fix (__toplevel_cons 'x ()))
                                          (__toplevel_cons (__toplevel_cons 'fix (__toplevel_cons 'y ())) ())))
                                    ())))
                           (__toplevel_cons
                              (__toplevel_cons
                                 'equal
                                 (__toplevel_cons
                                    (__toplevel_cons 'greaterp (__toplevel_cons 'x (__toplevel_cons 'y ())))
                                    (__toplevel_cons (__toplevel_cons 'lessp (__toplevel_cons 'y (__toplevel_cons 'x ()))) ())))
                              (__toplevel_cons
                                 (__toplevel_cons
                                    'equal
                                    (__toplevel_cons
                                       (__toplevel_cons 'lesseqp (__toplevel_cons 'x (__toplevel_cons 'y ())))
                                       (__toplevel_cons
                                          (__toplevel_cons
                                             'not
                                             (__toplevel_cons (__toplevel_cons 'lessp (__toplevel_cons 'y (__toplevel_cons 'x ()))) ()))
                                          ())))
                                 (__toplevel_cons
                                    (__toplevel_cons
                                       'equal
                                       (__toplevel_cons
                                          (__toplevel_cons 'greatereqp (__toplevel_cons 'x (__toplevel_cons 'y ())))
                                          (__toplevel_cons
                                             (__toplevel_cons
                                                'not
                                                (__toplevel_cons (__toplevel_cons 'lessp (__toplevel_cons 'x (__toplevel_cons 'y ()))) ()))
                                             ())))
                                    (__toplevel_cons
                                       (__toplevel_cons
                                          'equal
                                          (__toplevel_cons
                                             (__toplevel_cons 'boolean (__toplevel_cons 'x ()))
                                             (__toplevel_cons
                                                (__toplevel_cons
                                                   'or
                                                   (__toplevel_cons
                                                      (__toplevel_cons 'equal (__toplevel_cons 'x (__toplevel_cons (__toplevel_cons 't ()) ())))
                                                      (__toplevel_cons
                                                         (__toplevel_cons 'equal (__toplevel_cons 'x (__toplevel_cons (__toplevel_cons 'f ()) ())))
                                                         ())))
                                                ())))
                                       (__toplevel_cons
                                          (__toplevel_cons
                                             'equal
                                             (__toplevel_cons
                                                (__toplevel_cons 'iff (__toplevel_cons 'x (__toplevel_cons 'y ())))
                                                (__toplevel_cons
                                                   (__toplevel_cons
                                                      'and
                                                      (__toplevel_cons
                                                         (__toplevel_cons 'implies (__toplevel_cons 'x (__toplevel_cons 'y ())))
                                                         (__toplevel_cons (__toplevel_cons 'implies (__toplevel_cons 'y (__toplevel_cons 'x ()))) ())))
                                                   ())))
                                          (__toplevel_cons
                                             (__toplevel_cons
                                                'equal
                                                (__toplevel_cons
                                                   (__toplevel_cons 'even1 (__toplevel_cons 'x ()))
                                                   (__toplevel_cons
                                                      (__toplevel_cons
                                                         'if
                                                         (__toplevel_cons
                                                            (__toplevel_cons 'zerop (__toplevel_cons 'x ()))
                                                            (__toplevel_cons
                                                               (__toplevel_cons 't ())
                                                               (__toplevel_cons
                                                                  (__toplevel_cons 'odd (__toplevel_cons (__toplevel_cons '1- (__toplevel_cons 'x ())) ()))
                                                                  ()))))
                                                      ())))
                                             (__toplevel_cons
                                                (__toplevel_cons
                                                   'equal
                                                   (__toplevel_cons
                                                      (__toplevel_cons 'countps- (__toplevel_cons 'l (__toplevel_cons 'pred ())))
                                                      (__toplevel_cons
                                                         (__toplevel_cons
                                                            'countps-loop
                                                            (__toplevel_cons 'l (__toplevel_cons 'pred (__toplevel_cons (__toplevel_cons 'zero ()) ()))))
                                                         ())))
                                                (__toplevel_cons
                                                   (__toplevel_cons
                                                      'equal
                                                      (__toplevel_cons
                                                         (__toplevel_cons 'fact- (__toplevel_cons 'i ()))
                                                         (__toplevel_cons (__toplevel_cons 'fact-loop (__toplevel_cons 'i (__toplevel_cons 1 ()))) ())))
                                                   (__toplevel_cons
                                                      (__toplevel_cons
                                                         'equal
                                                         (__toplevel_cons
                                                            (__toplevel_cons 'reverse- (__toplevel_cons 'x ()))
                                                            (__toplevel_cons
                                                               (__toplevel_cons 'reverse-loop (__toplevel_cons 'x (__toplevel_cons (__toplevel_cons 'nil ()) ())))
                                                               ())))
                                                      (__toplevel_cons
                                                         (__toplevel_cons
                                                            'equal
                                                            (__toplevel_cons
                                                               (__toplevel_cons 'divides (__toplevel_cons 'x (__toplevel_cons 'y ())))
                                                               (__toplevel_cons
                                                                  (__toplevel_cons
                                                                     'zerop
                                                                     (__toplevel_cons (__toplevel_cons 'remainder (__toplevel_cons 'y (__toplevel_cons 'x ()))) ()))
                                                                  ())))
                                                         (__toplevel_cons
                                                            (__toplevel_cons
                                                               'equal
                                                               (__toplevel_cons
                                                                  (__toplevel_cons 'assume-true (__toplevel_cons 'var (__toplevel_cons 'alist ())))
                                                                  (__toplevel_cons
                                                                     (__toplevel_cons
                                                                        'cons
                                                                        (__toplevel_cons
                                                                           (__toplevel_cons 'cons (__toplevel_cons 'var (__toplevel_cons (__toplevel_cons 't ()) ())))
                                                                           (__toplevel_cons 'alist ())))
                                                                     ())))
                                                            (__toplevel_cons
                                                               (__toplevel_cons
                                                                  'equal
                                                                  (__toplevel_cons
                                                                     (__toplevel_cons 'assume-false (__toplevel_cons 'var (__toplevel_cons 'alist ())))
                                                                     (__toplevel_cons
                                                                        (__toplevel_cons
                                                                           'cons
                                                                           (__toplevel_cons
                                                                              (__toplevel_cons 'cons (__toplevel_cons 'var (__toplevel_cons (__toplevel_cons 'f ()) ())))
                                                                              (__toplevel_cons 'alist ())))
                                                                        ())))
                                                               (__toplevel_cons
                                                                  (__toplevel_cons
                                                                     'equal
                                                                     (__toplevel_cons
                                                                        (__toplevel_cons 'tautology-checker (__toplevel_cons 'x ()))
                                                                        (__toplevel_cons
                                                                           (__toplevel_cons
                                                                              'tautologyp
                                                                              (__toplevel_cons
                                                                                 (__toplevel_cons 'normalize (__toplevel_cons 'x ()))
                                                                                 (__toplevel_cons (__toplevel_cons 'nil ()) ())))
                                                                           ())))
                                                                  (__toplevel_cons
                                                                     (__toplevel_cons
                                                                        'equal
                                                                        (__toplevel_cons
                                                                           (__toplevel_cons 'falsify (__toplevel_cons 'x ()))
                                                                           (__toplevel_cons
                                                                              (__toplevel_cons
                                                                                 'falsify1
                                                                                 (__toplevel_cons
                                                                                    (__toplevel_cons 'normalize (__toplevel_cons 'x ()))
                                                                                    (__toplevel_cons (__toplevel_cons 'nil ()) ())))
                                                                              ())))
                                                                     (__toplevel_cons
                                                                        (__toplevel_cons
                                                                           'equal
                                                                           (__toplevel_cons
                                                                              (__toplevel_cons 'prime (__toplevel_cons 'x ()))
                                                                              (__toplevel_cons
                                                                                 (__toplevel_cons
                                                                                    'and
                                                                                    (__toplevel_cons
                                                                                       (__toplevel_cons 'not (__toplevel_cons (__toplevel_cons 'zerop (__toplevel_cons 'x ())) ()))
                                                                                       (__toplevel_cons
                                                                                          (__toplevel_cons
                                                                                             'not
                                                                                             (__toplevel_cons
                                                                                                (__toplevel_cons
                                                                                                   'equal
                                                                                                   (__toplevel_cons
                                                                                                      'x
                                                                                                      (__toplevel_cons (__toplevel_cons 'add1 (__toplevel_cons (__toplevel_cons 'zero ()) ())) ())))
                                                                                                ()))
                                                                                          (__toplevel_cons
                                                                                             (__toplevel_cons
                                                                                                'prime1
                                                                                                (__toplevel_cons 'x (__toplevel_cons (__toplevel_cons '1- (__toplevel_cons 'x ())) ())))
                                                                                             ()))))
                                                                                 ())))
                                                                        (__toplevel_cons
                                                                           (__toplevel_cons
                                                                              'equal
                                                                              (__toplevel_cons
                                                                                 (__toplevel_cons 'and (__toplevel_cons 'p (__toplevel_cons 'q ())))
                                                                                 (__toplevel_cons
                                                                                    (__toplevel_cons
                                                                                       'if
                                                                                       (__toplevel_cons
                                                                                          'p
                                                                                          (__toplevel_cons
                                                                                             (__toplevel_cons
                                                                                                'if
                                                                                                (__toplevel_cons
                                                                                                   'q
                                                                                                   (__toplevel_cons (__toplevel_cons 't ()) (__toplevel_cons (__toplevel_cons 'f ()) ()))))
                                                                                             (__toplevel_cons (__toplevel_cons 'f ()) ()))))
                                                                                    ())))
                                                                           (__toplevel_cons
                                                                              (__toplevel_cons
                                                                                 'equal
                                                                                 (__toplevel_cons
                                                                                    (__toplevel_cons 'or (__toplevel_cons 'p (__toplevel_cons 'q ())))
                                                                                    (__toplevel_cons
                                                                                       (__toplevel_cons
                                                                                          'if
                                                                                          (__toplevel_cons
                                                                                             'p
                                                                                             (__toplevel_cons
                                                                                                (__toplevel_cons 't ())
                                                                                                (__toplevel_cons
                                                                                                   (__toplevel_cons
                                                                                                      'if
                                                                                                      (__toplevel_cons
                                                                                                         'q
                                                                                                         (__toplevel_cons (__toplevel_cons 't ()) (__toplevel_cons (__toplevel_cons 'f ()) ()))))
                                                                                                   (__toplevel_cons (__toplevel_cons 'f ()) ())))))
                                                                                       ())))
                                                                              (__toplevel_cons
                                                                                 (__toplevel_cons
                                                                                    'equal
                                                                                    (__toplevel_cons
                                                                                       (__toplevel_cons 'not (__toplevel_cons 'p ()))
                                                                                       (__toplevel_cons
                                                                                          (__toplevel_cons
                                                                                             'if
                                                                                             (__toplevel_cons
                                                                                                'p
                                                                                                (__toplevel_cons (__toplevel_cons 'f ()) (__toplevel_cons (__toplevel_cons 't ()) ()))))
                                                                                          ())))
                                                                                 (__toplevel_cons
                                                                                    (__toplevel_cons
                                                                                       'equal
                                                                                       (__toplevel_cons
                                                                                          (__toplevel_cons 'implies (__toplevel_cons 'p (__toplevel_cons 'q ())))
                                                                                          (__toplevel_cons
                                                                                             (__toplevel_cons
                                                                                                'if
                                                                                                (__toplevel_cons
                                                                                                   'p
                                                                                                   (__toplevel_cons
                                                                                                      (__toplevel_cons
                                                                                                         'if
                                                                                                         (__toplevel_cons
                                                                                                            'q
                                                                                                            (__toplevel_cons (__toplevel_cons 't ()) (__toplevel_cons (__toplevel_cons 'f ()) ()))))
                                                                                                      (__toplevel_cons (__toplevel_cons 't ()) ()))))
                                                                                             ())))
                                                                                    (__toplevel_cons
                                                                                       (__toplevel_cons
                                                                                          'equal
                                                                                          (__toplevel_cons
                                                                                             (__toplevel_cons 'fix (__toplevel_cons 'x ()))
                                                                                             (__toplevel_cons
                                                                                                (__toplevel_cons
                                                                                                   'if
                                                                                                   (__toplevel_cons
                                                                                                      (__toplevel_cons 'numberp (__toplevel_cons 'x ()))
                                                                                                      (__toplevel_cons 'x (__toplevel_cons (__toplevel_cons 'zero ()) ()))))
                                                                                                ())))
                                                                                       (__toplevel_cons
                                                                                          (__toplevel_cons
                                                                                             'equal
                                                                                             (__toplevel_cons
                                                                                                (__toplevel_cons
                                                                                                   'if
                                                                                                   (__toplevel_cons
                                                                                                      (__toplevel_cons 'if (__toplevel_cons 'a (__toplevel_cons 'b (__toplevel_cons 'c ()))))
                                                                                                      (__toplevel_cons 'd (__toplevel_cons 'e ()))))
                                                                                                (__toplevel_cons
                                                                                                   (__toplevel_cons
                                                                                                      'if
                                                                                                      (__toplevel_cons
                                                                                                         'a
                                                                                                         (__toplevel_cons
                                                                                                            (__toplevel_cons 'if (__toplevel_cons 'b (__toplevel_cons 'd (__toplevel_cons 'e ()))))
                                                                                                            (__toplevel_cons
                                                                                                               (__toplevel_cons 'if (__toplevel_cons 'c (__toplevel_cons 'd (__toplevel_cons 'e ()))))
                                                                                                               ()))))
                                                                                                   ())))
                                                                                          (__toplevel_cons
                                                                                             (__toplevel_cons
                                                                                                'equal
                                                                                                (__toplevel_cons
                                                                                                   (__toplevel_cons 'zerop (__toplevel_cons 'x ()))
                                                                                                   (__toplevel_cons
                                                                                                      (__toplevel_cons
                                                                                                         'or
                                                                                                         (__toplevel_cons
                                                                                                            (__toplevel_cons 'equal (__toplevel_cons 'x (__toplevel_cons (__toplevel_cons 'zero ()) ())))
                                                                                                            (__toplevel_cons
                                                                                                               (__toplevel_cons 'not (__toplevel_cons (__toplevel_cons 'numberp (__toplevel_cons 'x ())) ()))
                                                                                                               ())))
                                                                                                      ())))
                                                                                             (__toplevel_cons
                                                                                                (__toplevel_cons
                                                                                                   'equal
                                                                                                   (__toplevel_cons
                                                                                                      (__toplevel_cons
                                                                                                         'plus
                                                                                                         (__toplevel_cons
                                                                                                            (__toplevel_cons 'plus (__toplevel_cons 'x (__toplevel_cons 'y ())))
                                                                                                            (__toplevel_cons 'z ())))
                                                                                                      (__toplevel_cons
                                                                                                         (__toplevel_cons
                                                                                                            'plus
                                                                                                            (__toplevel_cons
                                                                                                               'x
                                                                                                               (__toplevel_cons (__toplevel_cons 'plus (__toplevel_cons 'y (__toplevel_cons 'z ()))) ())))
                                                                                                         ())))
                                                                                                (__toplevel_cons
                                                                                                   (__toplevel_cons
                                                                                                      'equal
                                                                                                      (__toplevel_cons
                                                                                                         (__toplevel_cons
                                                                                                            'equal
                                                                                                            (__toplevel_cons
                                                                                                               (__toplevel_cons 'plus (__toplevel_cons 'a (__toplevel_cons 'b ())))
                                                                                                               (__toplevel_cons (__toplevel_cons 'zero ()) ())))
                                                                                                         (__toplevel_cons
                                                                                                            (__toplevel_cons
                                                                                                               'and
                                                                                                               (__toplevel_cons
                                                                                                                  (__toplevel_cons 'zerop (__toplevel_cons 'a ()))
                                                                                                                  (__toplevel_cons (__toplevel_cons 'zerop (__toplevel_cons 'b ())) ())))
                                                                                                            ())))
                                                                                                   (__toplevel_cons
                                                                                                      (__toplevel_cons
                                                                                                         'equal
                                                                                                         (__toplevel_cons
                                                                                                            (__toplevel_cons 'difference (__toplevel_cons 'x (__toplevel_cons 'x ())))
                                                                                                            (__toplevel_cons (__toplevel_cons 'zero ()) ())))
                                                                                                      (__toplevel_cons
                                                                                                         (__toplevel_cons
                                                                                                            'equal
                                                                                                            (__toplevel_cons
                                                                                                               (__toplevel_cons
                                                                                                                  'equal
                                                                                                                  (__toplevel_cons
                                                                                                                     (__toplevel_cons 'plus (__toplevel_cons 'a (__toplevel_cons 'b ())))
                                                                                                                     (__toplevel_cons (__toplevel_cons 'plus (__toplevel_cons 'a (__toplevel_cons 'c ()))) ())))
                                                                                                               (__toplevel_cons
                                                                                                                  (__toplevel_cons
                                                                                                                     'equal
                                                                                                                     (__toplevel_cons
                                                                                                                        (__toplevel_cons 'fix (__toplevel_cons 'b ()))
                                                                                                                        (__toplevel_cons (__toplevel_cons 'fix (__toplevel_cons 'c ())) ())))
                                                                                                                  ())))
                                                                                                         (__toplevel_cons
                                                                                                            (__toplevel_cons
                                                                                                               'equal
                                                                                                               (__toplevel_cons
                                                                                                                  (__toplevel_cons
                                                                                                                     'equal
                                                                                                                     (__toplevel_cons
                                                                                                                        (__toplevel_cons 'zero ())
                                                                                                                        (__toplevel_cons (__toplevel_cons 'difference (__toplevel_cons 'x (__toplevel_cons 'y ()))) ())))
                                                                                                                  (__toplevel_cons
                                                                                                                     (__toplevel_cons
                                                                                                                        'not
                                                                                                                        (__toplevel_cons (__toplevel_cons 'lessp (__toplevel_cons 'y (__toplevel_cons 'x ()))) ()))
                                                                                                                     ())))
                                                                                                            (__toplevel_cons
                                                                                                               (__toplevel_cons
                                                                                                                  'equal
                                                                                                                  (__toplevel_cons
                                                                                                                     (__toplevel_cons
                                                                                                                        'equal
                                                                                                                        (__toplevel_cons
                                                                                                                           'x
                                                                                                                           (__toplevel_cons (__toplevel_cons 'difference (__toplevel_cons 'x (__toplevel_cons 'y ()))) ())))
                                                                                                                     (__toplevel_cons
                                                                                                                        (__toplevel_cons
                                                                                                                           'and
                                                                                                                           (__toplevel_cons
                                                                                                                              (__toplevel_cons 'numberp (__toplevel_cons 'x ()))
                                                                                                                              (__toplevel_cons
                                                                                                                                 (__toplevel_cons
                                                                                                                                    'or
                                                                                                                                    (__toplevel_cons
                                                                                                                                       (__toplevel_cons 'equal (__toplevel_cons 'x (__toplevel_cons (__toplevel_cons 'zero ()) ())))
                                                                                                                                       (__toplevel_cons (__toplevel_cons 'zerop (__toplevel_cons 'y ())) ())))
                                                                                                                                 ())))
                                                                                                                        ())))
                                                                                                               (__toplevel_cons
                                                                                                                  (__toplevel_cons
                                                                                                                     'equal
                                                                                                                     (__toplevel_cons
                                                                                                                        (__toplevel_cons
                                                                                                                           'meaning
                                                                                                                           (__toplevel_cons
                                                                                                                              (__toplevel_cons
                                                                                                                                 'plus-tree
                                                                                                                                 (__toplevel_cons (__toplevel_cons 'append (__toplevel_cons 'x (__toplevel_cons 'y ()))) ()))
                                                                                                                              (__toplevel_cons 'a ())))
                                                                                                                        (__toplevel_cons
                                                                                                                           (__toplevel_cons
                                                                                                                              'plus
                                                                                                                              (__toplevel_cons
                                                                                                                                 (__toplevel_cons
                                                                                                                                    'meaning
                                                                                                                                    (__toplevel_cons (__toplevel_cons 'plus-tree (__toplevel_cons 'x ())) (__toplevel_cons 'a ())))
                                                                                                                                 (__toplevel_cons
                                                                                                                                    (__toplevel_cons
                                                                                                                                       'meaning
                                                                                                                                       (__toplevel_cons (__toplevel_cons 'plus-tree (__toplevel_cons 'y ())) (__toplevel_cons 'a ())))
                                                                                                                                    ())))
                                                                                                                           ())))
                                                                                                                  (__toplevel_cons
                                                                                                                     (__toplevel_cons
                                                                                                                        'equal
                                                                                                                        (__toplevel_cons
                                                                                                                           (__toplevel_cons
                                                                                                                              'meaning
                                                                                                                              (__toplevel_cons
                                                                                                                                 (__toplevel_cons
                                                                                                                                    'plus-tree
                                                                                                                                    (__toplevel_cons (__toplevel_cons 'plus-fringe (__toplevel_cons 'x ())) ()))
                                                                                                                                 (__toplevel_cons 'a ())))
                                                                                                                           (__toplevel_cons
                                                                                                                              (__toplevel_cons
                                                                                                                                 'fix
                                                                                                                                 (__toplevel_cons (__toplevel_cons 'meaning (__toplevel_cons 'x (__toplevel_cons 'a ()))) ()))
                                                                                                                              ())))
                                                                                                                     (__toplevel_cons
                                                                                                                        (__toplevel_cons
                                                                                                                           'equal
                                                                                                                           (__toplevel_cons
                                                                                                                              (__toplevel_cons
                                                                                                                                 'append
                                                                                                                                 (__toplevel_cons
                                                                                                                                    (__toplevel_cons 'append (__toplevel_cons 'x (__toplevel_cons 'y ())))
                                                                                                                                    (__toplevel_cons 'z ())))
                                                                                                                              (__toplevel_cons
                                                                                                                                 (__toplevel_cons
                                                                                                                                    'append
                                                                                                                                    (__toplevel_cons
                                                                                                                                       'x
                                                                                                                                       (__toplevel_cons (__toplevel_cons 'append (__toplevel_cons 'y (__toplevel_cons 'z ()))) ())))
                                                                                                                                 ())))
                                                                                                                        (__toplevel_cons
                                                                                                                           (__toplevel_cons
                                                                                                                              'equal
                                                                                                                              (__toplevel_cons
                                                                                                                                 (__toplevel_cons
                                                                                                                                    'reverse
                                                                                                                                    (__toplevel_cons (__toplevel_cons 'append (__toplevel_cons 'a (__toplevel_cons 'b ()))) ()))
                                                                                                                                 (__toplevel_cons
                                                                                                                                    (__toplevel_cons
                                                                                                                                       'append
                                                                                                                                       (__toplevel_cons
                                                                                                                                          (__toplevel_cons 'reverse (__toplevel_cons 'b ()))
                                                                                                                                          (__toplevel_cons (__toplevel_cons 'reverse (__toplevel_cons 'a ())) ())))
                                                                                                                                    ())))
                                                                                                                           (__toplevel_cons
                                                                                                                              (__toplevel_cons
                                                                                                                                 'equal
                                                                                                                                 (__toplevel_cons
                                                                                                                                    (__toplevel_cons
                                                                                                                                       'times
                                                                                                                                       (__toplevel_cons
                                                                                                                                          'x
                                                                                                                                          (__toplevel_cons (__toplevel_cons 'plus (__toplevel_cons 'y (__toplevel_cons 'z ()))) ())))
                                                                                                                                    (__toplevel_cons
                                                                                                                                       (__toplevel_cons
                                                                                                                                          'plus
                                                                                                                                          (__toplevel_cons
                                                                                                                                             (__toplevel_cons 'times (__toplevel_cons 'x (__toplevel_cons 'y ())))
                                                                                                                                             (__toplevel_cons (__toplevel_cons 'times (__toplevel_cons 'x (__toplevel_cons 'z ()))) ())))
                                                                                                                                       ())))
                                                                                                                              (__toplevel_cons
                                                                                                                                 (__toplevel_cons
                                                                                                                                    'equal
                                                                                                                                    (__toplevel_cons
                                                                                                                                       (__toplevel_cons
                                                                                                                                          'times
                                                                                                                                          (__toplevel_cons
                                                                                                                                             (__toplevel_cons 'times (__toplevel_cons 'x (__toplevel_cons 'y ())))
                                                                                                                                             (__toplevel_cons 'z ())))
                                                                                                                                       (__toplevel_cons
                                                                                                                                          (__toplevel_cons
                                                                                                                                             'times
                                                                                                                                             (__toplevel_cons
                                                                                                                                                'x
                                                                                                                                                (__toplevel_cons (__toplevel_cons 'times (__toplevel_cons 'y (__toplevel_cons 'z ()))) ())))
                                                                                                                                          ())))
                                                                                                                                 (__toplevel_cons
                                                                                                                                    (__toplevel_cons
                                                                                                                                       'equal
                                                                                                                                       (__toplevel_cons
                                                                                                                                          (__toplevel_cons
                                                                                                                                             'equal
                                                                                                                                             (__toplevel_cons
                                                                                                                                                (__toplevel_cons 'times (__toplevel_cons 'x (__toplevel_cons 'y ())))
                                                                                                                                                (__toplevel_cons (__toplevel_cons 'zero ()) ())))
                                                                                                                                          (__toplevel_cons
                                                                                                                                             (__toplevel_cons
                                                                                                                                                'or
                                                                                                                                                (__toplevel_cons
                                                                                                                                                   (__toplevel_cons 'zerop (__toplevel_cons 'x ()))
                                                                                                                                                   (__toplevel_cons (__toplevel_cons 'zerop (__toplevel_cons 'y ())) ())))
                                                                                                                                             ())))
                                                                                                                                    (__toplevel_cons
                                                                                                                                       (__toplevel_cons
                                                                                                                                          'equal
                                                                                                                                          (__toplevel_cons
                                                                                                                                             (__toplevel_cons
                                                                                                                                                'exec
                                                                                                                                                (__toplevel_cons
                                                                                                                                                   (__toplevel_cons 'append (__toplevel_cons 'x (__toplevel_cons 'y ())))
                                                                                                                                                   (__toplevel_cons 'pds (__toplevel_cons 'envrn ()))))
                                                                                                                                             (__toplevel_cons
                                                                                                                                                (__toplevel_cons
                                                                                                                                                   'exec
                                                                                                                                                   (__toplevel_cons
                                                                                                                                                      'y
                                                                                                                                                      (__toplevel_cons
                                                                                                                                                         (__toplevel_cons 'exec (__toplevel_cons 'x (__toplevel_cons 'pds (__toplevel_cons 'envrn ()))))
                                                                                                                                                         (__toplevel_cons 'envrn ()))))
                                                                                                                                                ())))
                                                                                                                                       (__toplevel_cons
                                                                                                                                          (__toplevel_cons
                                                                                                                                             'equal
                                                                                                                                             (__toplevel_cons
                                                                                                                                                (__toplevel_cons 'mc-flatten (__toplevel_cons 'x (__toplevel_cons 'y ())))
                                                                                                                                                (__toplevel_cons
                                                                                                                                                   (__toplevel_cons
                                                                                                                                                      'append
                                                                                                                                                      (__toplevel_cons (__toplevel_cons 'flatten (__toplevel_cons 'x ())) (__toplevel_cons 'y ())))
                                                                                                                                                   ())))
                                                                                                                                          (__toplevel_cons
                                                                                                                                             (__toplevel_cons
                                                                                                                                                'equal
                                                                                                                                                (__toplevel_cons
                                                                                                                                                   (__toplevel_cons
                                                                                                                                                      'member
                                                                                                                                                      (__toplevel_cons
                                                                                                                                                         'x
                                                                                                                                                         (__toplevel_cons (__toplevel_cons 'append (__toplevel_cons 'a (__toplevel_cons 'b ()))) ())))
                                                                                                                                                   (__toplevel_cons
                                                                                                                                                      (__toplevel_cons
                                                                                                                                                         'or
                                                                                                                                                         (__toplevel_cons
                                                                                                                                                            (__toplevel_cons 'member (__toplevel_cons 'x (__toplevel_cons 'a ())))
                                                                                                                                                            (__toplevel_cons (__toplevel_cons 'member (__toplevel_cons 'x (__toplevel_cons 'b ()))) ())))
                                                                                                                                                      ())))
                                                                                                                                             (__toplevel_cons
                                                                                                                                                (__toplevel_cons
                                                                                                                                                   'equal
                                                                                                                                                   (__toplevel_cons
                                                                                                                                                      (__toplevel_cons
                                                                                                                                                         'member
                                                                                                                                                         (__toplevel_cons 'x (__toplevel_cons (__toplevel_cons 'reverse (__toplevel_cons 'y ())) ())))
                                                                                                                                                      (__toplevel_cons (__toplevel_cons 'member (__toplevel_cons 'x (__toplevel_cons 'y ()))) ())))
                                                                                                                                                (__toplevel_cons
                                                                                                                                                   (__toplevel_cons
                                                                                                                                                      'equal
                                                                                                                                                      (__toplevel_cons
                                                                                                                                                         (__toplevel_cons 'length (__toplevel_cons (__toplevel_cons 'reverse (__toplevel_cons 'x ())) ()))
                                                                                                                                                         (__toplevel_cons (__toplevel_cons 'length (__toplevel_cons 'x ())) ())))
                                                                                                                                                   (__toplevel_cons
                                                                                                                                                      (__toplevel_cons
                                                                                                                                                         'equal
                                                                                                                                                         (__toplevel_cons
                                                                                                                                                            (__toplevel_cons
                                                                                                                                                               'member
                                                                                                                                                               (__toplevel_cons
                                                                                                                                                                  'a
                                                                                                                                                                  (__toplevel_cons (__toplevel_cons 'intersect (__toplevel_cons 'b (__toplevel_cons 'c ()))) ())))
                                                                                                                                                            (__toplevel_cons
                                                                                                                                                               (__toplevel_cons
                                                                                                                                                                  'and
                                                                                                                                                                  (__toplevel_cons
                                                                                                                                                                     (__toplevel_cons 'member (__toplevel_cons 'a (__toplevel_cons 'b ())))
                                                                                                                                                                     (__toplevel_cons (__toplevel_cons 'member (__toplevel_cons 'a (__toplevel_cons 'c ()))) ())))
                                                                                                                                                               ())))
                                                                                                                                                      (__toplevel_cons
                                                                                                                                                         (__toplevel_cons
                                                                                                                                                            'equal
                                                                                                                                                            (__toplevel_cons
                                                                                                                                                               (__toplevel_cons 'nth (__toplevel_cons (__toplevel_cons 'zero ()) (__toplevel_cons 'i ())))
                                                                                                                                                               (__toplevel_cons (__toplevel_cons 'zero ()) ())))
                                                                                                                                                         (__toplevel_cons
                                                                                                                                                            (__toplevel_cons
                                                                                                                                                               'equal
                                                                                                                                                               (__toplevel_cons
                                                                                                                                                                  (__toplevel_cons
                                                                                                                                                                     'exp
                                                                                                                                                                     (__toplevel_cons
                                                                                                                                                                        'i
                                                                                                                                                                        (__toplevel_cons (__toplevel_cons 'plus (__toplevel_cons 'j (__toplevel_cons 'k ()))) ())))
                                                                                                                                                                  (__toplevel_cons
                                                                                                                                                                     (__toplevel_cons
                                                                                                                                                                        'times
                                                                                                                                                                        (__toplevel_cons
                                                                                                                                                                           (__toplevel_cons 'exp (__toplevel_cons 'i (__toplevel_cons 'j ())))
                                                                                                                                                                           (__toplevel_cons (__toplevel_cons 'exp (__toplevel_cons 'i (__toplevel_cons 'k ()))) ())))
                                                                                                                                                                     ())))
                                                                                                                                                            (__toplevel_cons
                                                                                                                                                               (__toplevel_cons
                                                                                                                                                                  'equal
                                                                                                                                                                  (__toplevel_cons
                                                                                                                                                                     (__toplevel_cons
                                                                                                                                                                        'exp
                                                                                                                                                                        (__toplevel_cons
                                                                                                                                                                           'i
                                                                                                                                                                           (__toplevel_cons (__toplevel_cons 'times (__toplevel_cons 'j (__toplevel_cons 'k ()))) ())))
                                                                                                                                                                     (__toplevel_cons
                                                                                                                                                                        (__toplevel_cons
                                                                                                                                                                           'exp
                                                                                                                                                                           (__toplevel_cons
                                                                                                                                                                              (__toplevel_cons 'exp (__toplevel_cons 'i (__toplevel_cons 'j ())))
                                                                                                                                                                              (__toplevel_cons 'k ())))
                                                                                                                                                                        ())))
                                                                                                                                                               (__toplevel_cons
                                                                                                                                                                  (__toplevel_cons
                                                                                                                                                                     'equal
                                                                                                                                                                     (__toplevel_cons
                                                                                                                                                                        (__toplevel_cons 'reverse-loop (__toplevel_cons 'x (__toplevel_cons 'y ())))
                                                                                                                                                                        (__toplevel_cons
                                                                                                                                                                           (__toplevel_cons
                                                                                                                                                                              'append
                                                                                                                                                                              (__toplevel_cons (__toplevel_cons 'reverse (__toplevel_cons 'x ())) (__toplevel_cons 'y ())))
                                                                                                                                                                           ())))
                                                                                                                                                                  (__toplevel_cons
                                                                                                                                                                     (__toplevel_cons
                                                                                                                                                                        'equal
                                                                                                                                                                        (__toplevel_cons
                                                                                                                                                                           (__toplevel_cons 'reverse-loop (__toplevel_cons 'x (__toplevel_cons (__toplevel_cons 'nil ()) ())))
                                                                                                                                                                           (__toplevel_cons (__toplevel_cons 'reverse (__toplevel_cons 'x ())) ())))
                                                                                                                                                                     (__toplevel_cons
                                                                                                                                                                        (__toplevel_cons
                                                                                                                                                                           'equal
                                                                                                                                                                           (__toplevel_cons
                                                                                                                                                                              (__toplevel_cons
                                                                                                                                                                                 'count-list
                                                                                                                                                                                 (__toplevel_cons
                                                                                                                                                                                    'z
                                                                                                                                                                                    (__toplevel_cons (__toplevel_cons 'sort-lp (__toplevel_cons 'x (__toplevel_cons 'y ()))) ())))
                                                                                                                                                                              (__toplevel_cons
                                                                                                                                                                                 (__toplevel_cons
                                                                                                                                                                                    'plus
                                                                                                                                                                                    (__toplevel_cons
                                                                                                                                                                                       (__toplevel_cons 'count-list (__toplevel_cons 'z (__toplevel_cons 'x ())))
                                                                                                                                                                                       (__toplevel_cons (__toplevel_cons 'count-list (__toplevel_cons 'z (__toplevel_cons 'y ()))) ())))
                                                                                                                                                                                 ())))
                                                                                                                                                                        (__toplevel_cons
                                                                                                                                                                           (__toplevel_cons
                                                                                                                                                                              'equal
                                                                                                                                                                              (__toplevel_cons
                                                                                                                                                                                 (__toplevel_cons
                                                                                                                                                                                    'equal
                                                                                                                                                                                    (__toplevel_cons
                                                                                                                                                                                       (__toplevel_cons 'append (__toplevel_cons 'a (__toplevel_cons 'b ())))
                                                                                                                                                                                       (__toplevel_cons (__toplevel_cons 'append (__toplevel_cons 'a (__toplevel_cons 'c ()))) ())))
                                                                                                                                                                                 (__toplevel_cons (__toplevel_cons 'equal (__toplevel_cons 'b (__toplevel_cons 'c ()))) ())))
                                                                                                                                                                           (__toplevel_cons
                                                                                                                                                                              (__toplevel_cons
                                                                                                                                                                                 'equal
                                                                                                                                                                                 (__toplevel_cons
                                                                                                                                                                                    (__toplevel_cons
                                                                                                                                                                                       'plus
                                                                                                                                                                                       (__toplevel_cons
                                                                                                                                                                                          (__toplevel_cons 'remainder (__toplevel_cons 'x (__toplevel_cons 'y ())))
                                                                                                                                                                                          (__toplevel_cons
                                                                                                                                                                                             (__toplevel_cons
                                                                                                                                                                                                'times
                                                                                                                                                                                                (__toplevel_cons
                                                                                                                                                                                                   'y
                                                                                                                                                                                                   (__toplevel_cons (__toplevel_cons 'quotient (__toplevel_cons 'x (__toplevel_cons 'y ()))) ())))
                                                                                                                                                                                             ())))
                                                                                                                                                                                    (__toplevel_cons (__toplevel_cons 'fix (__toplevel_cons 'x ())) ())))
                                                                                                                                                                              (__toplevel_cons
                                                                                                                                                                                 (__toplevel_cons
                                                                                                                                                                                    'equal
                                                                                                                                                                                    (__toplevel_cons
                                                                                                                                                                                       (__toplevel_cons
                                                                                                                                                                                          'power-eval
                                                                                                                                                                                          (__toplevel_cons
                                                                                                                                                                                             (__toplevel_cons 'big-plus1 (__toplevel_cons 'l (__toplevel_cons 'i (__toplevel_cons 'base ()))))
                                                                                                                                                                                             (__toplevel_cons 'base ())))
                                                                                                                                                                                       (__toplevel_cons
                                                                                                                                                                                          (__toplevel_cons
                                                                                                                                                                                             'plus
                                                                                                                                                                                             (__toplevel_cons
                                                                                                                                                                                                (__toplevel_cons 'power-eval (__toplevel_cons 'l (__toplevel_cons 'base ())))
                                                                                                                                                                                                (__toplevel_cons 'i ())))
                                                                                                                                                                                          ())))
                                                                                                                                                                                 (__toplevel_cons
                                                                                                                                                                                    (__toplevel_cons
                                                                                                                                                                                       'equal
                                                                                                                                                                                       (__toplevel_cons
                                                                                                                                                                                          (__toplevel_cons
                                                                                                                                                                                             'power-eval
                                                                                                                                                                                             (__toplevel_cons
                                                                                                                                                                                                (__toplevel_cons
                                                                                                                                                                                                   'big-plus
                                                                                                                                                                                                   (__toplevel_cons 'x (__toplevel_cons 'y (__toplevel_cons 'i (__toplevel_cons 'base ())))))
                                                                                                                                                                                                (__toplevel_cons 'base ())))
                                                                                                                                                                                          (__toplevel_cons
                                                                                                                                                                                             (__toplevel_cons
                                                                                                                                                                                                'plus
                                                                                                                                                                                                (__toplevel_cons
                                                                                                                                                                                                   'i
                                                                                                                                                                                                   (__toplevel_cons
                                                                                                                                                                                                      (__toplevel_cons
                                                                                                                                                                                                         'plus
                                                                                                                                                                                                         (__toplevel_cons
                                                                                                                                                                                                            (__toplevel_cons 'power-eval (__toplevel_cons 'x (__toplevel_cons 'base ())))
                                                                                                                                                                                                            (__toplevel_cons (__toplevel_cons 'power-eval (__toplevel_cons 'y (__toplevel_cons 'base ()))) ())))
                                                                                                                                                                                                      ())))
                                                                                                                                                                                             ())))
                                                                                                                                                                                    (__toplevel_cons
                                                                                                                                                                                       (__toplevel_cons
                                                                                                                                                                                          'equal
                                                                                                                                                                                          (__toplevel_cons
                                                                                                                                                                                             (__toplevel_cons 'remainder (__toplevel_cons 'y (__toplevel_cons 1 ())))
                                                                                                                                                                                             (__toplevel_cons (__toplevel_cons 'zero ()) ())))
                                                                                                                                                                                       (__toplevel_cons
                                                                                                                                                                                          (__toplevel_cons
                                                                                                                                                                                             'equal
                                                                                                                                                                                             (__toplevel_cons
                                                                                                                                                                                                (__toplevel_cons
                                                                                                                                                                                                   'lessp
                                                                                                                                                                                                   (__toplevel_cons
                                                                                                                                                                                                      (__toplevel_cons 'remainder (__toplevel_cons 'x (__toplevel_cons 'y ())))
                                                                                                                                                                                                      (__toplevel_cons 'y ())))
                                                                                                                                                                                                (__toplevel_cons
                                                                                                                                                                                                   (__toplevel_cons 'not (__toplevel_cons (__toplevel_cons 'zerop (__toplevel_cons 'y ())) ()))
                                                                                                                                                                                                   ())))
                                                                                                                                                                                          (__toplevel_cons
                                                                                                                                                                                             (__toplevel_cons
                                                                                                                                                                                                'equal
                                                                                                                                                                                                (__toplevel_cons
                                                                                                                                                                                                   (__toplevel_cons 'remainder (__toplevel_cons 'x (__toplevel_cons 'x ())))
                                                                                                                                                                                                   (__toplevel_cons (__toplevel_cons 'zero ()) ())))
                                                                                                                                                                                             (__toplevel_cons
                                                                                                                                                                                                (__toplevel_cons
                                                                                                                                                                                                   'equal
                                                                                                                                                                                                   (__toplevel_cons
                                                                                                                                                                                                      (__toplevel_cons
                                                                                                                                                                                                         'lessp
                                                                                                                                                                                                         (__toplevel_cons
                                                                                                                                                                                                            (__toplevel_cons 'quotient (__toplevel_cons 'i (__toplevel_cons 'j ())))
                                                                                                                                                                                                            (__toplevel_cons 'i ())))
                                                                                                                                                                                                      (__toplevel_cons
                                                                                                                                                                                                         (__toplevel_cons
                                                                                                                                                                                                            'and
                                                                                                                                                                                                            (__toplevel_cons
                                                                                                                                                                                                               (__toplevel_cons 'not (__toplevel_cons (__toplevel_cons 'zerop (__toplevel_cons 'i ())) ()))
                                                                                                                                                                                                               (__toplevel_cons
                                                                                                                                                                                                                  (__toplevel_cons
                                                                                                                                                                                                                     'or
                                                                                                                                                                                                                     (__toplevel_cons
                                                                                                                                                                                                                        (__toplevel_cons 'zerop (__toplevel_cons 'j ()))
                                                                                                                                                                                                                        (__toplevel_cons
                                                                                                                                                                                                                           (__toplevel_cons
                                                                                                                                                                                                                              'not
                                                                                                                                                                                                                              (__toplevel_cons (__toplevel_cons 'equal (__toplevel_cons 'j (__toplevel_cons 1 ()))) ()))
                                                                                                                                                                                                                           ())))
                                                                                                                                                                                                                  ())))
                                                                                                                                                                                                         ())))
                                                                                                                                                                                                (__toplevel_cons
                                                                                                                                                                                                   (__toplevel_cons
                                                                                                                                                                                                      'equal
                                                                                                                                                                                                      (__toplevel_cons
                                                                                                                                                                                                         (__toplevel_cons
                                                                                                                                                                                                            'lessp
                                                                                                                                                                                                            (__toplevel_cons
                                                                                                                                                                                                               (__toplevel_cons 'remainder (__toplevel_cons 'x (__toplevel_cons 'y ())))
                                                                                                                                                                                                               (__toplevel_cons 'x ())))
                                                                                                                                                                                                         (__toplevel_cons
                                                                                                                                                                                                            (__toplevel_cons
                                                                                                                                                                                                               'and
                                                                                                                                                                                                               (__toplevel_cons
                                                                                                                                                                                                                  (__toplevel_cons 'not (__toplevel_cons (__toplevel_cons 'zerop (__toplevel_cons 'y ())) ()))
                                                                                                                                                                                                                  (__toplevel_cons
                                                                                                                                                                                                                     (__toplevel_cons 'not (__toplevel_cons (__toplevel_cons 'zerop (__toplevel_cons 'x ())) ()))
                                                                                                                                                                                                                     (__toplevel_cons
                                                                                                                                                                                                                        (__toplevel_cons
                                                                                                                                                                                                                           'not
                                                                                                                                                                                                                           (__toplevel_cons (__toplevel_cons 'lessp (__toplevel_cons 'x (__toplevel_cons 'y ()))) ()))
                                                                                                                                                                                                                        ()))))
                                                                                                                                                                                                            ())))
                                                                                                                                                                                                   (__toplevel_cons
                                                                                                                                                                                                      (__toplevel_cons
                                                                                                                                                                                                         'equal
                                                                                                                                                                                                         (__toplevel_cons
                                                                                                                                                                                                            (__toplevel_cons
                                                                                                                                                                                                               'power-eval
                                                                                                                                                                                                               (__toplevel_cons
                                                                                                                                                                                                                  (__toplevel_cons 'power-rep (__toplevel_cons 'i (__toplevel_cons 'base ())))
                                                                                                                                                                                                                  (__toplevel_cons 'base ())))
                                                                                                                                                                                                            (__toplevel_cons (__toplevel_cons 'fix (__toplevel_cons 'i ())) ())))
                                                                                                                                                                                                      (__toplevel_cons
                                                                                                                                                                                                         (__toplevel_cons
                                                                                                                                                                                                            'equal
                                                                                                                                                                                                            (__toplevel_cons
                                                                                                                                                                                                               (__toplevel_cons
                                                                                                                                                                                                                  'power-eval
                                                                                                                                                                                                                  (__toplevel_cons
                                                                                                                                                                                                                     (__toplevel_cons
                                                                                                                                                                                                                        'big-plus
                                                                                                                                                                                                                        (__toplevel_cons
                                                                                                                                                                                                                           (__toplevel_cons 'power-rep (__toplevel_cons 'i (__toplevel_cons 'base ())))
                                                                                                                                                                                                                           (__toplevel_cons
                                                                                                                                                                                                                              (__toplevel_cons 'power-rep (__toplevel_cons 'j (__toplevel_cons 'base ())))
                                                                                                                                                                                                                              (__toplevel_cons (__toplevel_cons 'zero ()) (__toplevel_cons 'base ())))))
                                                                                                                                                                                                                     (__toplevel_cons 'base ())))
                                                                                                                                                                                                               (__toplevel_cons (__toplevel_cons 'plus (__toplevel_cons 'i (__toplevel_cons 'j ()))) ())))
                                                                                                                                                                                                         (__toplevel_cons
                                                                                                                                                                                                            (__toplevel_cons
                                                                                                                                                                                                               'equal
                                                                                                                                                                                                               (__toplevel_cons
                                                                                                                                                                                                                  (__toplevel_cons 'gcd (__toplevel_cons 'x (__toplevel_cons 'y ())))
                                                                                                                                                                                                                  (__toplevel_cons (__toplevel_cons 'gcd (__toplevel_cons 'y (__toplevel_cons 'x ()))) ())))
                                                                                                                                                                                                            (__toplevel_cons
                                                                                                                                                                                                               (__toplevel_cons
                                                                                                                                                                                                                  'equal
                                                                                                                                                                                                                  (__toplevel_cons
                                                                                                                                                                                                                     (__toplevel_cons
                                                                                                                                                                                                                        'nth
                                                                                                                                                                                                                        (__toplevel_cons
                                                                                                                                                                                                                           (__toplevel_cons 'append (__toplevel_cons 'a (__toplevel_cons 'b ())))
                                                                                                                                                                                                                           (__toplevel_cons 'i ())))
                                                                                                                                                                                                                     (__toplevel_cons
                                                                                                                                                                                                                        (__toplevel_cons
                                                                                                                                                                                                                           'append
                                                                                                                                                                                                                           (__toplevel_cons
                                                                                                                                                                                                                              (__toplevel_cons 'nth (__toplevel_cons 'a (__toplevel_cons 'i ())))
                                                                                                                                                                                                                              (__toplevel_cons
                                                                                                                                                                                                                                 (__toplevel_cons
                                                                                                                                                                                                                                    'nth
                                                                                                                                                                                                                                    (__toplevel_cons
                                                                                                                                                                                                                                       'b
                                                                                                                                                                                                                                       (__toplevel_cons
                                                                                                                                                                                                                                          (__toplevel_cons
                                                                                                                                                                                                                                             'difference
                                                                                                                                                                                                                                             (__toplevel_cons 'i (__toplevel_cons (__toplevel_cons 'length (__toplevel_cons 'a ())) ())))
                                                                                                                                                                                                                                          ())))
                                                                                                                                                                                                                                 ())))
                                                                                                                                                                                                                        ())))
                                                                                                                                                                                                               (__toplevel_cons
                                                                                                                                                                                                                  (__toplevel_cons
                                                                                                                                                                                                                     'equal
                                                                                                                                                                                                                     (__toplevel_cons
                                                                                                                                                                                                                        (__toplevel_cons
                                                                                                                                                                                                                           'difference
                                                                                                                                                                                                                           (__toplevel_cons
                                                                                                                                                                                                                              (__toplevel_cons 'plus (__toplevel_cons 'x (__toplevel_cons 'y ())))
                                                                                                                                                                                                                              (__toplevel_cons 'x ())))
                                                                                                                                                                                                                        (__toplevel_cons (__toplevel_cons 'fix (__toplevel_cons 'y ())) ())))
                                                                                                                                                                                                                  (__toplevel_cons
                                                                                                                                                                                                                     (__toplevel_cons
                                                                                                                                                                                                                        'equal
                                                                                                                                                                                                                        (__toplevel_cons
                                                                                                                                                                                                                           (__toplevel_cons
                                                                                                                                                                                                                              'difference
                                                                                                                                                                                                                              (__toplevel_cons
                                                                                                                                                                                                                                 (__toplevel_cons 'plus (__toplevel_cons 'y (__toplevel_cons 'x ())))
                                                                                                                                                                                                                                 (__toplevel_cons 'x ())))
                                                                                                                                                                                                                           (__toplevel_cons (__toplevel_cons 'fix (__toplevel_cons 'y ())) ())))
                                                                                                                                                                                                                     (__toplevel_cons
                                                                                                                                                                                                                        (__toplevel_cons
                                                                                                                                                                                                                           'equal
                                                                                                                                                                                                                           (__toplevel_cons
                                                                                                                                                                                                                              (__toplevel_cons
                                                                                                                                                                                                                                 'difference
                                                                                                                                                                                                                                 (__toplevel_cons
                                                                                                                                                                                                                                    (__toplevel_cons 'plus (__toplevel_cons 'x (__toplevel_cons 'y ())))
                                                                                                                                                                                                                                    (__toplevel_cons (__toplevel_cons 'plus (__toplevel_cons 'x (__toplevel_cons 'z ()))) ())))
                                                                                                                                                                                                                              (__toplevel_cons (__toplevel_cons 'difference (__toplevel_cons 'y (__toplevel_cons 'z ()))) ())))
                                                                                                                                                                                                                        (__toplevel_cons
                                                                                                                                                                                                                           (__toplevel_cons
                                                                                                                                                                                                                              'equal
                                                                                                                                                                                                                              (__toplevel_cons
                                                                                                                                                                                                                                 (__toplevel_cons
                                                                                                                                                                                                                                    'times
                                                                                                                                                                                                                                    (__toplevel_cons
                                                                                                                                                                                                                                       'x
                                                                                                                                                                                                                                       (__toplevel_cons (__toplevel_cons 'difference (__toplevel_cons 'c (__toplevel_cons 'w ()))) ())))
                                                                                                                                                                                                                                 (__toplevel_cons
                                                                                                                                                                                                                                    (__toplevel_cons
                                                                                                                                                                                                                                       'difference
                                                                                                                                                                                                                                       (__toplevel_cons
                                                                                                                                                                                                                                          (__toplevel_cons 'times (__toplevel_cons 'c (__toplevel_cons 'x ())))
                                                                                                                                                                                                                                          (__toplevel_cons (__toplevel_cons 'times (__toplevel_cons 'w (__toplevel_cons 'x ()))) ())))
                                                                                                                                                                                                                                    ())))
                                                                                                                                                                                                                           (__toplevel_cons
                                                                                                                                                                                                                              (__toplevel_cons
                                                                                                                                                                                                                                 'equal
                                                                                                                                                                                                                                 (__toplevel_cons
                                                                                                                                                                                                                                    (__toplevel_cons
                                                                                                                                                                                                                                       'remainder
                                                                                                                                                                                                                                       (__toplevel_cons
                                                                                                                                                                                                                                          (__toplevel_cons 'times (__toplevel_cons 'x (__toplevel_cons 'z ())))
                                                                                                                                                                                                                                          (__toplevel_cons 'z ())))
                                                                                                                                                                                                                                    (__toplevel_cons (__toplevel_cons 'zero ()) ())))
                                                                                                                                                                                                                              (__toplevel_cons
                                                                                                                                                                                                                                 (__toplevel_cons
                                                                                                                                                                                                                                    'equal
                                                                                                                                                                                                                                    (__toplevel_cons
                                                                                                                                                                                                                                       (__toplevel_cons
                                                                                                                                                                                                                                          'difference
                                                                                                                                                                                                                                          (__toplevel_cons
                                                                                                                                                                                                                                             (__toplevel_cons
                                                                                                                                                                                                                                                'plus
                                                                                                                                                                                                                                                (__toplevel_cons
                                                                                                                                                                                                                                                   'b
                                                                                                                                                                                                                                                   (__toplevel_cons (__toplevel_cons 'plus (__toplevel_cons 'a (__toplevel_cons 'c ()))) ())))
                                                                                                                                                                                                                                             (__toplevel_cons 'a ())))
                                                                                                                                                                                                                                       (__toplevel_cons (__toplevel_cons 'plus (__toplevel_cons 'b (__toplevel_cons 'c ()))) ())))
                                                                                                                                                                                                                                 (__toplevel_cons
                                                                                                                                                                                                                                    (__toplevel_cons
                                                                                                                                                                                                                                       'equal
                                                                                                                                                                                                                                       (__toplevel_cons
                                                                                                                                                                                                                                          (__toplevel_cons
                                                                                                                                                                                                                                             'difference
                                                                                                                                                                                                                                             (__toplevel_cons
                                                                                                                                                                                                                                                (__toplevel_cons
                                                                                                                                                                                                                                                   'add1
                                                                                                                                                                                                                                                   (__toplevel_cons (__toplevel_cons 'plus (__toplevel_cons 'y (__toplevel_cons 'z ()))) ()))
                                                                                                                                                                                                                                                (__toplevel_cons 'z ())))
                                                                                                                                                                                                                                          (__toplevel_cons (__toplevel_cons 'add1 (__toplevel_cons 'y ())) ())))
                                                                                                                                                                                                                                    (__toplevel_cons
                                                                                                                                                                                                                                       (__toplevel_cons
                                                                                                                                                                                                                                          'equal
                                                                                                                                                                                                                                          (__toplevel_cons
                                                                                                                                                                                                                                             (__toplevel_cons
                                                                                                                                                                                                                                                'lessp
                                                                                                                                                                                                                                                (__toplevel_cons
                                                                                                                                                                                                                                                   (__toplevel_cons 'plus (__toplevel_cons 'x (__toplevel_cons 'y ())))
                                                                                                                                                                                                                                                   (__toplevel_cons (__toplevel_cons 'plus (__toplevel_cons 'x (__toplevel_cons 'z ()))) ())))
                                                                                                                                                                                                                                             (__toplevel_cons (__toplevel_cons 'lessp (__toplevel_cons 'y (__toplevel_cons 'z ()))) ())))
                                                                                                                                                                                                                                       (__toplevel_cons
                                                                                                                                                                                                                                          (__toplevel_cons
                                                                                                                                                                                                                                             'equal
                                                                                                                                                                                                                                             (__toplevel_cons
                                                                                                                                                                                                                                                (__toplevel_cons
                                                                                                                                                                                                                                                   'lessp
                                                                                                                                                                                                                                                   (__toplevel_cons
                                                                                                                                                                                                                                                      (__toplevel_cons 'times (__toplevel_cons 'x (__toplevel_cons 'z ())))
                                                                                                                                                                                                                                                      (__toplevel_cons (__toplevel_cons 'times (__toplevel_cons 'y (__toplevel_cons 'z ()))) ())))
                                                                                                                                                                                                                                                (__toplevel_cons
                                                                                                                                                                                                                                                   (__toplevel_cons
                                                                                                                                                                                                                                                      'and
                                                                                                                                                                                                                                                      (__toplevel_cons
                                                                                                                                                                                                                                                         (__toplevel_cons 'not (__toplevel_cons (__toplevel_cons 'zerop (__toplevel_cons 'z ())) ()))
                                                                                                                                                                                                                                                         (__toplevel_cons (__toplevel_cons 'lessp (__toplevel_cons 'x (__toplevel_cons 'y ()))) ())))
                                                                                                                                                                                                                                                   ())))
                                                                                                                                                                                                                                          (__toplevel_cons
                                                                                                                                                                                                                                             (__toplevel_cons
                                                                                                                                                                                                                                                'equal
                                                                                                                                                                                                                                                (__toplevel_cons
                                                                                                                                                                                                                                                   (__toplevel_cons
                                                                                                                                                                                                                                                      'lessp
                                                                                                                                                                                                                                                      (__toplevel_cons
                                                                                                                                                                                                                                                         'y
                                                                                                                                                                                                                                                         (__toplevel_cons (__toplevel_cons 'plus (__toplevel_cons 'x (__toplevel_cons 'y ()))) ())))
                                                                                                                                                                                                                                                   (__toplevel_cons
                                                                                                                                                                                                                                                      (__toplevel_cons 'not (__toplevel_cons (__toplevel_cons 'zerop (__toplevel_cons 'x ())) ()))
                                                                                                                                                                                                                                                      ())))
                                                                                                                                                                                                                                             (__toplevel_cons
                                                                                                                                                                                                                                                (__toplevel_cons
                                                                                                                                                                                                                                                   'equal
                                                                                                                                                                                                                                                   (__toplevel_cons
                                                                                                                                                                                                                                                      (__toplevel_cons
                                                                                                                                                                                                                                                         'gcd
                                                                                                                                                                                                                                                         (__toplevel_cons
                                                                                                                                                                                                                                                            (__toplevel_cons 'times (__toplevel_cons 'x (__toplevel_cons 'z ())))
                                                                                                                                                                                                                                                            (__toplevel_cons (__toplevel_cons 'times (__toplevel_cons 'y (__toplevel_cons 'z ()))) ())))
                                                                                                                                                                                                                                                      (__toplevel_cons
                                                                                                                                                                                                                                                         (__toplevel_cons
                                                                                                                                                                                                                                                            'times
                                                                                                                                                                                                                                                            (__toplevel_cons
                                                                                                                                                                                                                                                               'z
                                                                                                                                                                                                                                                               (__toplevel_cons (__toplevel_cons 'gcd (__toplevel_cons 'x (__toplevel_cons 'y ()))) ())))
                                                                                                                                                                                                                                                         ())))
                                                                                                                                                                                                                                                (__toplevel_cons
                                                                                                                                                                                                                                                   (__toplevel_cons
                                                                                                                                                                                                                                                      'equal
                                                                                                                                                                                                                                                      (__toplevel_cons
                                                                                                                                                                                                                                                         (__toplevel_cons
                                                                                                                                                                                                                                                            'value
                                                                                                                                                                                                                                                            (__toplevel_cons (__toplevel_cons 'normalize (__toplevel_cons 'x ())) (__toplevel_cons 'a ())))
                                                                                                                                                                                                                                                         (__toplevel_cons (__toplevel_cons 'value (__toplevel_cons 'x (__toplevel_cons 'a ()))) ())))
                                                                                                                                                                                                                                                   (__toplevel_cons
                                                                                                                                                                                                                                                      (__toplevel_cons
                                                                                                                                                                                                                                                         'equal
                                                                                                                                                                                                                                                         (__toplevel_cons
                                                                                                                                                                                                                                                            (__toplevel_cons
                                                                                                                                                                                                                                                               'equal
                                                                                                                                                                                                                                                               (__toplevel_cons
                                                                                                                                                                                                                                                                  (__toplevel_cons 'flatten (__toplevel_cons 'x ()))
                                                                                                                                                                                                                                                                  (__toplevel_cons
                                                                                                                                                                                                                                                                     (__toplevel_cons 'cons (__toplevel_cons 'y (__toplevel_cons (__toplevel_cons 'nil ()) ())))
                                                                                                                                                                                                                                                                     ())))
                                                                                                                                                                                                                                                            (__toplevel_cons
                                                                                                                                                                                                                                                               (__toplevel_cons
                                                                                                                                                                                                                                                                  'and
                                                                                                                                                                                                                                                                  (__toplevel_cons
                                                                                                                                                                                                                                                                     (__toplevel_cons 'nlistp (__toplevel_cons 'x ()))
                                                                                                                                                                                                                                                                     (__toplevel_cons (__toplevel_cons 'equal (__toplevel_cons 'x (__toplevel_cons 'y ()))) ())))
                                                                                                                                                                                                                                                               ())))
                                                                                                                                                                                                                                                      (__toplevel_cons
                                                                                                                                                                                                                                                         (__toplevel_cons
                                                                                                                                                                                                                                                            'equal
                                                                                                                                                                                                                                                            (__toplevel_cons
                                                                                                                                                                                                                                                               (__toplevel_cons 'listp (__toplevel_cons (__toplevel_cons 'gopher (__toplevel_cons 'x ())) ()))
                                                                                                                                                                                                                                                               (__toplevel_cons (__toplevel_cons 'listp (__toplevel_cons 'x ())) ())))
                                                                                                                                                                                                                                                         (__toplevel_cons
                                                                                                                                                                                                                                                            (__toplevel_cons
                                                                                                                                                                                                                                                               'equal
                                                                                                                                                                                                                                                               (__toplevel_cons
                                                                                                                                                                                                                                                                  (__toplevel_cons 'samefringe (__toplevel_cons 'x (__toplevel_cons 'y ())))
                                                                                                                                                                                                                                                                  (__toplevel_cons
                                                                                                                                                                                                                                                                     (__toplevel_cons
                                                                                                                                                                                                                                                                        'equal
                                                                                                                                                                                                                                                                        (__toplevel_cons
                                                                                                                                                                                                                                                                           (__toplevel_cons 'flatten (__toplevel_cons 'x ()))
                                                                                                                                                                                                                                                                           (__toplevel_cons (__toplevel_cons 'flatten (__toplevel_cons 'y ())) ())))
                                                                                                                                                                                                                                                                     ())))
                                                                                                                                                                                                                                                            (__toplevel_cons
                                                                                                                                                                                                                                                               (__toplevel_cons
                                                                                                                                                                                                                                                                  'equal
                                                                                                                                                                                                                                                                  (__toplevel_cons
                                                                                                                                                                                                                                                                     (__toplevel_cons
                                                                                                                                                                                                                                                                        'equal
                                                                                                                                                                                                                                                                        (__toplevel_cons
                                                                                                                                                                                                                                                                           (__toplevel_cons 'greatest-factor (__toplevel_cons 'x (__toplevel_cons 'y ())))
                                                                                                                                                                                                                                                                           (__toplevel_cons (__toplevel_cons 'zero ()) ())))
                                                                                                                                                                                                                                                                     (__toplevel_cons
                                                                                                                                                                                                                                                                        (__toplevel_cons
                                                                                                                                                                                                                                                                           'and
                                                                                                                                                                                                                                                                           (__toplevel_cons
                                                                                                                                                                                                                                                                              (__toplevel_cons
                                                                                                                                                                                                                                                                                 'or
                                                                                                                                                                                                                                                                                 (__toplevel_cons
                                                                                                                                                                                                                                                                                    (__toplevel_cons 'zerop (__toplevel_cons 'y ()))
                                                                                                                                                                                                                                                                                    (__toplevel_cons (__toplevel_cons 'equal (__toplevel_cons 'y (__toplevel_cons 1 ()))) ())))
                                                                                                                                                                                                                                                                              (__toplevel_cons
                                                                                                                                                                                                                                                                                 (__toplevel_cons 'equal (__toplevel_cons 'x (__toplevel_cons (__toplevel_cons 'zero ()) ())))
                                                                                                                                                                                                                                                                                 ())))
                                                                                                                                                                                                                                                                        ())))
                                                                                                                                                                                                                                                               (__toplevel_cons
                                                                                                                                                                                                                                                                  (__toplevel_cons
                                                                                                                                                                                                                                                                     'equal
                                                                                                                                                                                                                                                                     (__toplevel_cons
                                                                                                                                                                                                                                                                        (__toplevel_cons
                                                                                                                                                                                                                                                                           'equal
                                                                                                                                                                                                                                                                           (__toplevel_cons
                                                                                                                                                                                                                                                                              (__toplevel_cons 'greatest-factor (__toplevel_cons 'x (__toplevel_cons 'y ())))
                                                                                                                                                                                                                                                                              (__toplevel_cons 1 ())))
                                                                                                                                                                                                                                                                        (__toplevel_cons (__toplevel_cons 'equal (__toplevel_cons 'x (__toplevel_cons 1 ()))) ())))
                                                                                                                                                                                                                                                                  (__toplevel_cons
                                                                                                                                                                                                                                                                     (__toplevel_cons
                                                                                                                                                                                                                                                                        'equal
                                                                                                                                                                                                                                                                        (__toplevel_cons
                                                                                                                                                                                                                                                                           (__toplevel_cons
                                                                                                                                                                                                                                                                              'numberp
                                                                                                                                                                                                                                                                              (__toplevel_cons
                                                                                                                                                                                                                                                                                 (__toplevel_cons 'greatest-factor (__toplevel_cons 'x (__toplevel_cons 'y ())))
                                                                                                                                                                                                                                                                                 ()))
                                                                                                                                                                                                                                                                           (__toplevel_cons
                                                                                                                                                                                                                                                                              (__toplevel_cons
                                                                                                                                                                                                                                                                                 'not
                                                                                                                                                                                                                                                                                 (__toplevel_cons
                                                                                                                                                                                                                                                                                    (__toplevel_cons
                                                                                                                                                                                                                                                                                       'and
                                                                                                                                                                                                                                                                                       (__toplevel_cons
                                                                                                                                                                                                                                                                                          (__toplevel_cons
                                                                                                                                                                                                                                                                                             'or
                                                                                                                                                                                                                                                                                             (__toplevel_cons
                                                                                                                                                                                                                                                                                                (__toplevel_cons 'zerop (__toplevel_cons 'y ()))
                                                                                                                                                                                                                                                                                                (__toplevel_cons (__toplevel_cons 'equal (__toplevel_cons 'y (__toplevel_cons 1 ()))) ())))
                                                                                                                                                                                                                                                                                          (__toplevel_cons
                                                                                                                                                                                                                                                                                             (__toplevel_cons 'not (__toplevel_cons (__toplevel_cons 'numberp (__toplevel_cons 'x ())) ()))
                                                                                                                                                                                                                                                                                             ())))
                                                                                                                                                                                                                                                                                    ()))
                                                                                                                                                                                                                                                                              ())))
                                                                                                                                                                                                                                                                     (__toplevel_cons
                                                                                                                                                                                                                                                                        (__toplevel_cons
                                                                                                                                                                                                                                                                           'equal
                                                                                                                                                                                                                                                                           (__toplevel_cons
                                                                                                                                                                                                                                                                              (__toplevel_cons
                                                                                                                                                                                                                                                                                 'times-list
                                                                                                                                                                                                                                                                                 (__toplevel_cons (__toplevel_cons 'append (__toplevel_cons 'x (__toplevel_cons 'y ()))) ()))
                                                                                                                                                                                                                                                                              (__toplevel_cons
                                                                                                                                                                                                                                                                                 (__toplevel_cons
                                                                                                                                                                                                                                                                                    'times
                                                                                                                                                                                                                                                                                    (__toplevel_cons
                                                                                                                                                                                                                                                                                       (__toplevel_cons 'times-list (__toplevel_cons 'x ()))
                                                                                                                                                                                                                                                                                       (__toplevel_cons (__toplevel_cons 'times-list (__toplevel_cons 'y ())) ())))
                                                                                                                                                                                                                                                                                 ())))
                                                                                                                                                                                                                                                                        (__toplevel_cons
                                                                                                                                                                                                                                                                           (__toplevel_cons
                                                                                                                                                                                                                                                                              'equal
                                                                                                                                                                                                                                                                              (__toplevel_cons
                                                                                                                                                                                                                                                                                 (__toplevel_cons
                                                                                                                                                                                                                                                                                    'prime-list
                                                                                                                                                                                                                                                                                    (__toplevel_cons (__toplevel_cons 'append (__toplevel_cons 'x (__toplevel_cons 'y ()))) ()))
                                                                                                                                                                                                                                                                                 (__toplevel_cons
                                                                                                                                                                                                                                                                                    (__toplevel_cons
                                                                                                                                                                                                                                                                                       'and
                                                                                                                                                                                                                                                                                       (__toplevel_cons
                                                                                                                                                                                                                                                                                          (__toplevel_cons 'prime-list (__toplevel_cons 'x ()))
                                                                                                                                                                                                                                                                                          (__toplevel_cons (__toplevel_cons 'prime-list (__toplevel_cons 'y ())) ())))
                                                                                                                                                                                                                                                                                    ())))
                                                                                                                                                                                                                                                                           (__toplevel_cons
                                                                                                                                                                                                                                                                              (__toplevel_cons
                                                                                                                                                                                                                                                                                 'equal
                                                                                                                                                                                                                                                                                 (__toplevel_cons
                                                                                                                                                                                                                                                                                    (__toplevel_cons
                                                                                                                                                                                                                                                                                       'equal
                                                                                                                                                                                                                                                                                       (__toplevel_cons
                                                                                                                                                                                                                                                                                          'z
                                                                                                                                                                                                                                                                                          (__toplevel_cons (__toplevel_cons 'times (__toplevel_cons 'w (__toplevel_cons 'z ()))) ())))
                                                                                                                                                                                                                                                                                    (__toplevel_cons
                                                                                                                                                                                                                                                                                       (__toplevel_cons
                                                                                                                                                                                                                                                                                          'and
                                                                                                                                                                                                                                                                                          (__toplevel_cons
                                                                                                                                                                                                                                                                                             (__toplevel_cons 'numberp (__toplevel_cons 'z ()))
                                                                                                                                                                                                                                                                                             (__toplevel_cons
                                                                                                                                                                                                                                                                                                (__toplevel_cons
                                                                                                                                                                                                                                                                                                   'or
                                                                                                                                                                                                                                                                                                   (__toplevel_cons
                                                                                                                                                                                                                                                                                                      (__toplevel_cons 'equal (__toplevel_cons 'z (__toplevel_cons (__toplevel_cons 'zero ()) ())))
                                                                                                                                                                                                                                                                                                      (__toplevel_cons (__toplevel_cons 'equal (__toplevel_cons 'w (__toplevel_cons 1 ()))) ())))
                                                                                                                                                                                                                                                                                                ())))
                                                                                                                                                                                                                                                                                       ())))
                                                                                                                                                                                                                                                                              (__toplevel_cons
                                                                                                                                                                                                                                                                                 (__toplevel_cons
                                                                                                                                                                                                                                                                                    'equal
                                                                                                                                                                                                                                                                                    (__toplevel_cons
                                                                                                                                                                                                                                                                                       (__toplevel_cons 'greatereqpr (__toplevel_cons 'x (__toplevel_cons 'y ())))
                                                                                                                                                                                                                                                                                       (__toplevel_cons
                                                                                                                                                                                                                                                                                          (__toplevel_cons
                                                                                                                                                                                                                                                                                             'not
                                                                                                                                                                                                                                                                                             (__toplevel_cons (__toplevel_cons 'lessp (__toplevel_cons 'x (__toplevel_cons 'y ()))) ()))
                                                                                                                                                                                                                                                                                          ())))
                                                                                                                                                                                                                                                                                 (__toplevel_cons
                                                                                                                                                                                                                                                                                    (__toplevel_cons
                                                                                                                                                                                                                                                                                       'equal
                                                                                                                                                                                                                                                                                       (__toplevel_cons
                                                                                                                                                                                                                                                                                          (__toplevel_cons
                                                                                                                                                                                                                                                                                             'equal
                                                                                                                                                                                                                                                                                             (__toplevel_cons
                                                                                                                                                                                                                                                                                                'x
                                                                                                                                                                                                                                                                                                (__toplevel_cons (__toplevel_cons 'times (__toplevel_cons 'x (__toplevel_cons 'y ()))) ())))
                                                                                                                                                                                                                                                                                          (__toplevel_cons
                                                                                                                                                                                                                                                                                             (__toplevel_cons
                                                                                                                                                                                                                                                                                                'or
                                                                                                                                                                                                                                                                                                (__toplevel_cons
                                                                                                                                                                                                                                                                                                   (__toplevel_cons 'equal (__toplevel_cons 'x (__toplevel_cons (__toplevel_cons 'zero ()) ())))
                                                                                                                                                                                                                                                                                                   (__toplevel_cons
                                                                                                                                                                                                                                                                                                      (__toplevel_cons
                                                                                                                                                                                                                                                                                                         'and
                                                                                                                                                                                                                                                                                                         (__toplevel_cons
                                                                                                                                                                                                                                                                                                            (__toplevel_cons 'numberp (__toplevel_cons 'x ()))
                                                                                                                                                                                                                                                                                                            (__toplevel_cons (__toplevel_cons 'equal (__toplevel_cons 'y (__toplevel_cons 1 ()))) ())))
                                                                                                                                                                                                                                                                                                      ())))
                                                                                                                                                                                                                                                                                             ())))
                                                                                                                                                                                                                                                                                    (__toplevel_cons
                                                                                                                                                                                                                                                                                       (__toplevel_cons
                                                                                                                                                                                                                                                                                          'equal
                                                                                                                                                                                                                                                                                          (__toplevel_cons
                                                                                                                                                                                                                                                                                             (__toplevel_cons
                                                                                                                                                                                                                                                                                                'remainder
                                                                                                                                                                                                                                                                                                (__toplevel_cons
                                                                                                                                                                                                                                                                                                   (__toplevel_cons 'times (__toplevel_cons 'y (__toplevel_cons 'x ())))
                                                                                                                                                                                                                                                                                                   (__toplevel_cons 'y ())))
                                                                                                                                                                                                                                                                                             (__toplevel_cons (__toplevel_cons 'zero ()) ())))
                                                                                                                                                                                                                                                                                       (__toplevel_cons
                                                                                                                                                                                                                                                                                          (__toplevel_cons
                                                                                                                                                                                                                                                                                             'equal
                                                                                                                                                                                                                                                                                             (__toplevel_cons
                                                                                                                                                                                                                                                                                                (__toplevel_cons
                                                                                                                                                                                                                                                                                                   'equal
                                                                                                                                                                                                                                                                                                   (__toplevel_cons
                                                                                                                                                                                                                                                                                                      (__toplevel_cons 'times (__toplevel_cons 'a (__toplevel_cons 'b ())))
                                                                                                                                                                                                                                                                                                      (__toplevel_cons 1 ())))
                                                                                                                                                                                                                                                                                                (__toplevel_cons
                                                                                                                                                                                                                                                                                                   (__toplevel_cons
                                                                                                                                                                                                                                                                                                      'and
                                                                                                                                                                                                                                                                                                      (__toplevel_cons
                                                                                                                                                                                                                                                                                                         (__toplevel_cons
                                                                                                                                                                                                                                                                                                            'not
                                                                                                                                                                                                                                                                                                            (__toplevel_cons
                                                                                                                                                                                                                                                                                                               (__toplevel_cons 'equal (__toplevel_cons 'a (__toplevel_cons (__toplevel_cons 'zero ()) ())))
                                                                                                                                                                                                                                                                                                               ()))
                                                                                                                                                                                                                                                                                                         (__toplevel_cons
                                                                                                                                                                                                                                                                                                            (__toplevel_cons
                                                                                                                                                                                                                                                                                                               'not
                                                                                                                                                                                                                                                                                                               (__toplevel_cons
                                                                                                                                                                                                                                                                                                                  (__toplevel_cons 'equal (__toplevel_cons 'b (__toplevel_cons (__toplevel_cons 'zero ()) ())))
                                                                                                                                                                                                                                                                                                                  ()))
                                                                                                                                                                                                                                                                                                            (__toplevel_cons
                                                                                                                                                                                                                                                                                                               (__toplevel_cons 'numberp (__toplevel_cons 'a ()))
                                                                                                                                                                                                                                                                                                               (__toplevel_cons
                                                                                                                                                                                                                                                                                                                  (__toplevel_cons 'numberp (__toplevel_cons 'b ()))
                                                                                                                                                                                                                                                                                                                  (__toplevel_cons
                                                                                                                                                                                                                                                                                                                     (__toplevel_cons
                                                                                                                                                                                                                                                                                                                        'equal
                                                                                                                                                                                                                                                                                                                        (__toplevel_cons
                                                                                                                                                                                                                                                                                                                           (__toplevel_cons '1- (__toplevel_cons 'a ()))
                                                                                                                                                                                                                                                                                                                           (__toplevel_cons (__toplevel_cons 'zero ()) ())))
                                                                                                                                                                                                                                                                                                                     (__toplevel_cons
                                                                                                                                                                                                                                                                                                                        (__toplevel_cons
                                                                                                                                                                                                                                                                                                                           'equal
                                                                                                                                                                                                                                                                                                                           (__toplevel_cons
                                                                                                                                                                                                                                                                                                                              (__toplevel_cons '1- (__toplevel_cons 'b ()))
                                                                                                                                                                                                                                                                                                                              (__toplevel_cons (__toplevel_cons 'zero ()) ())))
                                                                                                                                                                                                                                                                                                                        ())))))))
                                                                                                                                                                                                                                                                                                   ())))
                                                                                                                                                                                                                                                                                          (__toplevel_cons
                                                                                                                                                                                                                                                                                             (__toplevel_cons
                                                                                                                                                                                                                                                                                                'equal
                                                                                                                                                                                                                                                                                                (__toplevel_cons
                                                                                                                                                                                                                                                                                                   (__toplevel_cons
                                                                                                                                                                                                                                                                                                      'lessp
                                                                                                                                                                                                                                                                                                      (__toplevel_cons
                                                                                                                                                                                                                                                                                                         (__toplevel_cons
                                                                                                                                                                                                                                                                                                            'length
                                                                                                                                                                                                                                                                                                            (__toplevel_cons (__toplevel_cons 'delete (__toplevel_cons 'x (__toplevel_cons 'l ()))) ()))
                                                                                                                                                                                                                                                                                                         (__toplevel_cons (__toplevel_cons 'length (__toplevel_cons 'l ())) ())))
                                                                                                                                                                                                                                                                                                   (__toplevel_cons (__toplevel_cons 'member (__toplevel_cons 'x (__toplevel_cons 'l ()))) ())))
                                                                                                                                                                                                                                                                                             (__toplevel_cons
                                                                                                                                                                                                                                                                                                (__toplevel_cons
                                                                                                                                                                                                                                                                                                   'equal
                                                                                                                                                                                                                                                                                                   (__toplevel_cons
                                                                                                                                                                                                                                                                                                      (__toplevel_cons
                                                                                                                                                                                                                                                                                                         'sort2
                                                                                                                                                                                                                                                                                                         (__toplevel_cons (__toplevel_cons 'delete (__toplevel_cons 'x (__toplevel_cons 'l ()))) ()))
                                                                                                                                                                                                                                                                                                      (__toplevel_cons
                                                                                                                                                                                                                                                                                                         (__toplevel_cons
                                                                                                                                                                                                                                                                                                            'delete
                                                                                                                                                                                                                                                                                                            (__toplevel_cons 'x (__toplevel_cons (__toplevel_cons 'sort2 (__toplevel_cons 'l ())) ())))
                                                                                                                                                                                                                                                                                                         ())))
                                                                                                                                                                                                                                                                                                (__toplevel_cons
                                                                                                                                                                                                                                                                                                   (__toplevel_cons
                                                                                                                                                                                                                                                                                                      'equal
                                                                                                                                                                                                                                                                                                      (__toplevel_cons
                                                                                                                                                                                                                                                                                                         (__toplevel_cons 'dsort (__toplevel_cons 'x ()))
                                                                                                                                                                                                                                                                                                         (__toplevel_cons (__toplevel_cons 'sort2 (__toplevel_cons 'x ())) ())))
                                                                                                                                                                                                                                                                                                   (__toplevel_cons
                                                                                                                                                                                                                                                                                                      (__toplevel_cons
                                                                                                                                                                                                                                                                                                         'equal
                                                                                                                                                                                                                                                                                                         (__toplevel_cons
                                                                                                                                                                                                                                                                                                            (__toplevel_cons
                                                                                                                                                                                                                                                                                                               'length
                                                                                                                                                                                                                                                                                                               (__toplevel_cons
                                                                                                                                                                                                                                                                                                                  (__toplevel_cons
                                                                                                                                                                                                                                                                                                                     'cons
                                                                                                                                                                                                                                                                                                                     (__toplevel_cons
                                                                                                                                                                                                                                                                                                                        'x1
                                                                                                                                                                                                                                                                                                                        (__toplevel_cons
                                                                                                                                                                                                                                                                                                                           (__toplevel_cons
                                                                                                                                                                                                                                                                                                                              'cons
                                                                                                                                                                                                                                                                                                                              (__toplevel_cons
                                                                                                                                                                                                                                                                                                                                 'x2
                                                                                                                                                                                                                                                                                                                                 (__toplevel_cons
                                                                                                                                                                                                                                                                                                                                    (__toplevel_cons
                                                                                                                                                                                                                                                                                                                                       'cons
                                                                                                                                                                                                                                                                                                                                       (__toplevel_cons
                                                                                                                                                                                                                                                                                                                                          'x3
                                                                                                                                                                                                                                                                                                                                          (__toplevel_cons
                                                                                                                                                                                                                                                                                                                                             (__toplevel_cons
                                                                                                                                                                                                                                                                                                                                                'cons
                                                                                                                                                                                                                                                                                                                                                (__toplevel_cons
                                                                                                                                                                                                                                                                                                                                                   'x4
                                                                                                                                                                                                                                                                                                                                                   (__toplevel_cons
                                                                                                                                                                                                                                                                                                                                                      (__toplevel_cons
                                                                                                                                                                                                                                                                                                                                                         'cons
                                                                                                                                                                                                                                                                                                                                                         (__toplevel_cons
                                                                                                                                                                                                                                                                                                                                                            'x5
                                                                                                                                                                                                                                                                                                                                                            (__toplevel_cons (__toplevel_cons 'cons (__toplevel_cons 'x6 (__toplevel_cons 'x7 ()))) ())))
                                                                                                                                                                                                                                                                                                                                                      ())))
                                                                                                                                                                                                                                                                                                                                             ())))
                                                                                                                                                                                                                                                                                                                                    ())))
                                                                                                                                                                                                                                                                                                                           ())))
                                                                                                                                                                                                                                                                                                                  ()))
                                                                                                                                                                                                                                                                                                            (__toplevel_cons
                                                                                                                                                                                                                                                                                                               (__toplevel_cons
                                                                                                                                                                                                                                                                                                                  'plus
                                                                                                                                                                                                                                                                                                                  (__toplevel_cons 6 (__toplevel_cons (__toplevel_cons 'length (__toplevel_cons 'x7 ())) ())))
                                                                                                                                                                                                                                                                                                               ())))
                                                                                                                                                                                                                                                                                                      (__toplevel_cons
                                                                                                                                                                                                                                                                                                         (__toplevel_cons
                                                                                                                                                                                                                                                                                                            'equal
                                                                                                                                                                                                                                                                                                            (__toplevel_cons
                                                                                                                                                                                                                                                                                                               (__toplevel_cons
                                                                                                                                                                                                                                                                                                                  'difference
                                                                                                                                                                                                                                                                                                                  (__toplevel_cons
                                                                                                                                                                                                                                                                                                                     (__toplevel_cons 'add1 (__toplevel_cons (__toplevel_cons 'add1 (__toplevel_cons 'x ())) ()))
                                                                                                                                                                                                                                                                                                                     (__toplevel_cons 2 ())))
                                                                                                                                                                                                                                                                                                               (__toplevel_cons (__toplevel_cons 'fix (__toplevel_cons 'x ())) ())))
                                                                                                                                                                                                                                                                                                         (__toplevel_cons
                                                                                                                                                                                                                                                                                                            (__toplevel_cons
                                                                                                                                                                                                                                                                                                               'equal
                                                                                                                                                                                                                                                                                                               (__toplevel_cons
                                                                                                                                                                                                                                                                                                                  (__toplevel_cons
                                                                                                                                                                                                                                                                                                                     'quotient
                                                                                                                                                                                                                                                                                                                     (__toplevel_cons
                                                                                                                                                                                                                                                                                                                        (__toplevel_cons
                                                                                                                                                                                                                                                                                                                           'plus
                                                                                                                                                                                                                                                                                                                           (__toplevel_cons
                                                                                                                                                                                                                                                                                                                              'x
                                                                                                                                                                                                                                                                                                                              (__toplevel_cons (__toplevel_cons 'plus (__toplevel_cons 'x (__toplevel_cons 'y ()))) ())))
                                                                                                                                                                                                                                                                                                                        (__toplevel_cons 2 ())))
                                                                                                                                                                                                                                                                                                                  (__toplevel_cons
                                                                                                                                                                                                                                                                                                                     (__toplevel_cons
                                                                                                                                                                                                                                                                                                                        'plus
                                                                                                                                                                                                                                                                                                                        (__toplevel_cons
                                                                                                                                                                                                                                                                                                                           'x
                                                                                                                                                                                                                                                                                                                           (__toplevel_cons (__toplevel_cons 'quotient (__toplevel_cons 'y (__toplevel_cons 2 ()))) ())))
                                                                                                                                                                                                                                                                                                                     ())))
                                                                                                                                                                                                                                                                                                            (__toplevel_cons
                                                                                                                                                                                                                                                                                                               (__toplevel_cons
                                                                                                                                                                                                                                                                                                                  'equal
                                                                                                                                                                                                                                                                                                                  (__toplevel_cons
                                                                                                                                                                                                                                                                                                                     (__toplevel_cons 'sigma (__toplevel_cons (__toplevel_cons 'zero ()) (__toplevel_cons 'i ())))
                                                                                                                                                                                                                                                                                                                     (__toplevel_cons
                                                                                                                                                                                                                                                                                                                        (__toplevel_cons
                                                                                                                                                                                                                                                                                                                           'quotient
                                                                                                                                                                                                                                                                                                                           (__toplevel_cons
                                                                                                                                                                                                                                                                                                                              (__toplevel_cons
                                                                                                                                                                                                                                                                                                                                 'times
                                                                                                                                                                                                                                                                                                                                 (__toplevel_cons 'i (__toplevel_cons (__toplevel_cons 'add1 (__toplevel_cons 'i ())) ())))
                                                                                                                                                                                                                                                                                                                              (__toplevel_cons 2 ())))
                                                                                                                                                                                                                                                                                                                        ())))
                                                                                                                                                                                                                                                                                                               (__toplevel_cons
                                                                                                                                                                                                                                                                                                                  (__toplevel_cons
                                                                                                                                                                                                                                                                                                                     'equal
                                                                                                                                                                                                                                                                                                                     (__toplevel_cons
                                                                                                                                                                                                                                                                                                                        (__toplevel_cons
                                                                                                                                                                                                                                                                                                                           'plus
                                                                                                                                                                                                                                                                                                                           (__toplevel_cons 'x (__toplevel_cons (__toplevel_cons 'add1 (__toplevel_cons 'y ())) ())))
                                                                                                                                                                                                                                                                                                                        (__toplevel_cons
                                                                                                                                                                                                                                                                                                                           (__toplevel_cons
                                                                                                                                                                                                                                                                                                                              'if
                                                                                                                                                                                                                                                                                                                              (__toplevel_cons
                                                                                                                                                                                                                                                                                                                                 (__toplevel_cons 'numberp (__toplevel_cons 'y ()))
                                                                                                                                                                                                                                                                                                                                 (__toplevel_cons
                                                                                                                                                                                                                                                                                                                                    (__toplevel_cons
                                                                                                                                                                                                                                                                                                                                       'add1
                                                                                                                                                                                                                                                                                                                                       (__toplevel_cons (__toplevel_cons 'plus (__toplevel_cons 'x (__toplevel_cons 'y ()))) ()))
                                                                                                                                                                                                                                                                                                                                    (__toplevel_cons (__toplevel_cons 'add1 (__toplevel_cons 'x ())) ()))))
                                                                                                                                                                                                                                                                                                                           ())))
                                                                                                                                                                                                                                                                                                                  (__toplevel_cons
                                                                                                                                                                                                                                                                                                                     (__toplevel_cons
                                                                                                                                                                                                                                                                                                                        'equal
                                                                                                                                                                                                                                                                                                                        (__toplevel_cons
                                                                                                                                                                                                                                                                                                                           (__toplevel_cons
                                                                                                                                                                                                                                                                                                                              'equal
                                                                                                                                                                                                                                                                                                                              (__toplevel_cons
                                                                                                                                                                                                                                                                                                                                 (__toplevel_cons 'difference (__toplevel_cons 'x (__toplevel_cons 'y ())))
                                                                                                                                                                                                                                                                                                                                 (__toplevel_cons (__toplevel_cons 'difference (__toplevel_cons 'z (__toplevel_cons 'y ()))) ())))
                                                                                                                                                                                                                                                                                                                           (__toplevel_cons
                                                                                                                                                                                                                                                                                                                              (__toplevel_cons
                                                                                                                                                                                                                                                                                                                                 'if
                                                                                                                                                                                                                                                                                                                                 (__toplevel_cons
                                                                                                                                                                                                                                                                                                                                    (__toplevel_cons 'lessp (__toplevel_cons 'x (__toplevel_cons 'y ())))
                                                                                                                                                                                                                                                                                                                                    (__toplevel_cons
                                                                                                                                                                                                                                                                                                                                       (__toplevel_cons
                                                                                                                                                                                                                                                                                                                                          'not
                                                                                                                                                                                                                                                                                                                                          (__toplevel_cons (__toplevel_cons 'lessp (__toplevel_cons 'y (__toplevel_cons 'z ()))) ()))
                                                                                                                                                                                                                                                                                                                                       (__toplevel_cons
                                                                                                                                                                                                                                                                                                                                          (__toplevel_cons
                                                                                                                                                                                                                                                                                                                                             'if
                                                                                                                                                                                                                                                                                                                                             (__toplevel_cons
                                                                                                                                                                                                                                                                                                                                                (__toplevel_cons 'lessp (__toplevel_cons 'z (__toplevel_cons 'y ())))
                                                                                                                                                                                                                                                                                                                                                (__toplevel_cons
                                                                                                                                                                                                                                                                                                                                                   (__toplevel_cons
                                                                                                                                                                                                                                                                                                                                                      'not
                                                                                                                                                                                                                                                                                                                                                      (__toplevel_cons (__toplevel_cons 'lessp (__toplevel_cons 'y (__toplevel_cons 'x ()))) ()))
                                                                                                                                                                                                                                                                                                                                                   (__toplevel_cons
                                                                                                                                                                                                                                                                                                                                                      (__toplevel_cons
                                                                                                                                                                                                                                                                                                                                                         'equal
                                                                                                                                                                                                                                                                                                                                                         (__toplevel_cons
                                                                                                                                                                                                                                                                                                                                                            (__toplevel_cons 'fix (__toplevel_cons 'x ()))
                                                                                                                                                                                                                                                                                                                                                            (__toplevel_cons (__toplevel_cons 'fix (__toplevel_cons 'z ())) ())))
                                                                                                                                                                                                                                                                                                                                                      ()))))
                                                                                                                                                                                                                                                                                                                                          ()))))
                                                                                                                                                                                                                                                                                                                              ())))
                                                                                                                                                                                                                                                                                                                     (__toplevel_cons
                                                                                                                                                                                                                                                                                                                        (__toplevel_cons
                                                                                                                                                                                                                                                                                                                           'equal
                                                                                                                                                                                                                                                                                                                           (__toplevel_cons
                                                                                                                                                                                                                                                                                                                              (__toplevel_cons
                                                                                                                                                                                                                                                                                                                                 'meaning
                                                                                                                                                                                                                                                                                                                                 (__toplevel_cons
                                                                                                                                                                                                                                                                                                                                    (__toplevel_cons
                                                                                                                                                                                                                                                                                                                                       'plus-tree
                                                                                                                                                                                                                                                                                                                                       (__toplevel_cons (__toplevel_cons 'delete (__toplevel_cons 'x (__toplevel_cons 'y ()))) ()))
                                                                                                                                                                                                                                                                                                                                    (__toplevel_cons 'a ())))
                                                                                                                                                                                                                                                                                                                              (__toplevel_cons
                                                                                                                                                                                                                                                                                                                                 (__toplevel_cons
                                                                                                                                                                                                                                                                                                                                    'if
                                                                                                                                                                                                                                                                                                                                    (__toplevel_cons
                                                                                                                                                                                                                                                                                                                                       (__toplevel_cons 'member (__toplevel_cons 'x (__toplevel_cons 'y ())))
                                                                                                                                                                                                                                                                                                                                       (__toplevel_cons
                                                                                                                                                                                                                                                                                                                                          (__toplevel_cons
                                                                                                                                                                                                                                                                                                                                             'difference
                                                                                                                                                                                                                                                                                                                                             (__toplevel_cons
                                                                                                                                                                                                                                                                                                                                                (__toplevel_cons
                                                                                                                                                                                                                                                                                                                                                   'meaning
                                                                                                                                                                                                                                                                                                                                                   (__toplevel_cons (__toplevel_cons 'plus-tree (__toplevel_cons 'y ())) (__toplevel_cons 'a ())))
                                                                                                                                                                                                                                                                                                                                                (__toplevel_cons (__toplevel_cons 'meaning (__toplevel_cons 'x (__toplevel_cons 'a ()))) ())))
                                                                                                                                                                                                                                                                                                                                          (__toplevel_cons
                                                                                                                                                                                                                                                                                                                                             (__toplevel_cons
                                                                                                                                                                                                                                                                                                                                                'meaning
                                                                                                                                                                                                                                                                                                                                                (__toplevel_cons (__toplevel_cons 'plus-tree (__toplevel_cons 'y ())) (__toplevel_cons 'a ())))
                                                                                                                                                                                                                                                                                                                                             ()))))
                                                                                                                                                                                                                                                                                                                                 ())))
                                                                                                                                                                                                                                                                                                                        (__toplevel_cons
                                                                                                                                                                                                                                                                                                                           (__toplevel_cons
                                                                                                                                                                                                                                                                                                                              'equal
                                                                                                                                                                                                                                                                                                                              (__toplevel_cons
                                                                                                                                                                                                                                                                                                                                 (__toplevel_cons
                                                                                                                                                                                                                                                                                                                                    'times
                                                                                                                                                                                                                                                                                                                                    (__toplevel_cons 'x (__toplevel_cons (__toplevel_cons 'add1 (__toplevel_cons 'y ())) ())))
                                                                                                                                                                                                                                                                                                                                 (__toplevel_cons
                                                                                                                                                                                                                                                                                                                                    (__toplevel_cons
                                                                                                                                                                                                                                                                                                                                       'if
                                                                                                                                                                                                                                                                                                                                       (__toplevel_cons
                                                                                                                                                                                                                                                                                                                                          (__toplevel_cons 'numberp (__toplevel_cons 'y ()))
                                                                                                                                                                                                                                                                                                                                          (__toplevel_cons
                                                                                                                                                                                                                                                                                                                                             (__toplevel_cons
                                                                                                                                                                                                                                                                                                                                                'plus
                                                                                                                                                                                                                                                                                                                                                (__toplevel_cons
                                                                                                                                                                                                                                                                                                                                                   'x
                                                                                                                                                                                                                                                                                                                                                   (__toplevel_cons (__toplevel_cons 'times (__toplevel_cons 'x (__toplevel_cons 'y ()))) ())))
                                                                                                                                                                                                                                                                                                                                             (__toplevel_cons (__toplevel_cons 'fix (__toplevel_cons 'x ())) ()))))
                                                                                                                                                                                                                                                                                                                                    ())))
                                                                                                                                                                                                                                                                                                                           (__toplevel_cons
                                                                                                                                                                                                                                                                                                                              (__toplevel_cons
                                                                                                                                                                                                                                                                                                                                 'equal
                                                                                                                                                                                                                                                                                                                                 (__toplevel_cons
                                                                                                                                                                                                                                                                                                                                    (__toplevel_cons 'nth (__toplevel_cons (__toplevel_cons 'nil ()) (__toplevel_cons 'i ())))
                                                                                                                                                                                                                                                                                                                                    (__toplevel_cons
                                                                                                                                                                                                                                                                                                                                       (__toplevel_cons
                                                                                                                                                                                                                                                                                                                                          'if
                                                                                                                                                                                                                                                                                                                                          (__toplevel_cons
                                                                                                                                                                                                                                                                                                                                             (__toplevel_cons 'zerop (__toplevel_cons 'i ()))
                                                                                                                                                                                                                                                                                                                                             (__toplevel_cons (__toplevel_cons 'nil ()) (__toplevel_cons (__toplevel_cons 'zero ()) ()))))
                                                                                                                                                                                                                                                                                                                                       ())))
                                                                                                                                                                                                                                                                                                                              (__toplevel_cons
                                                                                                                                                                                                                                                                                                                                 (__toplevel_cons
                                                                                                                                                                                                                                                                                                                                    'equal
                                                                                                                                                                                                                                                                                                                                    (__toplevel_cons
                                                                                                                                                                                                                                                                                                                                       (__toplevel_cons
                                                                                                                                                                                                                                                                                                                                          'last
                                                                                                                                                                                                                                                                                                                                          (__toplevel_cons (__toplevel_cons 'append (__toplevel_cons 'a (__toplevel_cons 'b ()))) ()))
                                                                                                                                                                                                                                                                                                                                       (__toplevel_cons
                                                                                                                                                                                                                                                                                                                                          (__toplevel_cons
                                                                                                                                                                                                                                                                                                                                             'if
                                                                                                                                                                                                                                                                                                                                             (__toplevel_cons
                                                                                                                                                                                                                                                                                                                                                (__toplevel_cons 'listp (__toplevel_cons 'b ()))
                                                                                                                                                                                                                                                                                                                                                (__toplevel_cons
                                                                                                                                                                                                                                                                                                                                                   (__toplevel_cons 'last (__toplevel_cons 'b ()))
                                                                                                                                                                                                                                                                                                                                                   (__toplevel_cons
                                                                                                                                                                                                                                                                                                                                                      (__toplevel_cons
                                                                                                                                                                                                                                                                                                                                                         'if
                                                                                                                                                                                                                                                                                                                                                         (__toplevel_cons
                                                                                                                                                                                                                                                                                                                                                            (__toplevel_cons 'listp (__toplevel_cons 'a ()))
                                                                                                                                                                                                                                                                                                                                                            (__toplevel_cons
                                                                                                                                                                                                                                                                                                                                                               (__toplevel_cons
                                                                                                                                                                                                                                                                                                                                                                  'cons
                                                                                                                                                                                                                                                                                                                                                                  (__toplevel_cons
                                                                                                                                                                                                                                                                                                                                                                     (__toplevel_cons 'car (__toplevel_cons (__toplevel_cons 'last (__toplevel_cons 'a ())) ()))
                                                                                                                                                                                                                                                                                                                                                                     (__toplevel_cons 'b ())))
                                                                                                                                                                                                                                                                                                                                                               (__toplevel_cons 'b ()))))
                                                                                                                                                                                                                                                                                                                                                      ()))))
                                                                                                                                                                                                                                                                                                                                          ())))
                                                                                                                                                                                                                                                                                                                                 (__toplevel_cons
                                                                                                                                                                                                                                                                                                                                    (__toplevel_cons
                                                                                                                                                                                                                                                                                                                                       'equal
                                                                                                                                                                                                                                                                                                                                       (__toplevel_cons
                                                                                                                                                                                                                                                                                                                                          (__toplevel_cons
                                                                                                                                                                                                                                                                                                                                             'equal
                                                                                                                                                                                                                                                                                                                                             (__toplevel_cons
                                                                                                                                                                                                                                                                                                                                                (__toplevel_cons 'lessp (__toplevel_cons 'x (__toplevel_cons 'y ())))
                                                                                                                                                                                                                                                                                                                                                (__toplevel_cons 'z ())))
                                                                                                                                                                                                                                                                                                                                          (__toplevel_cons
                                                                                                                                                                                                                                                                                                                                             (__toplevel_cons
                                                                                                                                                                                                                                                                                                                                                'if
                                                                                                                                                                                                                                                                                                                                                (__toplevel_cons
                                                                                                                                                                                                                                                                                                                                                   (__toplevel_cons 'lessp (__toplevel_cons 'x (__toplevel_cons 'y ())))
                                                                                                                                                                                                                                                                                                                                                   (__toplevel_cons
                                                                                                                                                                                                                                                                                                                                                      (__toplevel_cons 'equal (__toplevel_cons 't (__toplevel_cons 'z ())))
                                                                                                                                                                                                                                                                                                                                                      (__toplevel_cons (__toplevel_cons 'equal (__toplevel_cons 'f (__toplevel_cons 'z ()))) ()))))
                                                                                                                                                                                                                                                                                                                                             ())))
                                                                                                                                                                                                                                                                                                                                    (__toplevel_cons
                                                                                                                                                                                                                                                                                                                                       (__toplevel_cons
                                                                                                                                                                                                                                                                                                                                          'equal
                                                                                                                                                                                                                                                                                                                                          (__toplevel_cons
                                                                                                                                                                                                                                                                                                                                             (__toplevel_cons
                                                                                                                                                                                                                                                                                                                                                'assignment
                                                                                                                                                                                                                                                                                                                                                (__toplevel_cons
                                                                                                                                                                                                                                                                                                                                                   'x
                                                                                                                                                                                                                                                                                                                                                   (__toplevel_cons (__toplevel_cons 'append (__toplevel_cons 'a (__toplevel_cons 'b ()))) ())))
                                                                                                                                                                                                                                                                                                                                             (__toplevel_cons
                                                                                                                                                                                                                                                                                                                                                (__toplevel_cons
                                                                                                                                                                                                                                                                                                                                                   'if
                                                                                                                                                                                                                                                                                                                                                   (__toplevel_cons
                                                                                                                                                                                                                                                                                                                                                      (__toplevel_cons 'assignedp (__toplevel_cons 'x (__toplevel_cons 'a ())))
                                                                                                                                                                                                                                                                                                                                                      (__toplevel_cons
                                                                                                                                                                                                                                                                                                                                                         (__toplevel_cons 'assignment (__toplevel_cons 'x (__toplevel_cons 'a ())))
                                                                                                                                                                                                                                                                                                                                                         (__toplevel_cons (__toplevel_cons 'assignment (__toplevel_cons 'x (__toplevel_cons 'b ()))) ()))))
                                                                                                                                                                                                                                                                                                                                                ())))
                                                                                                                                                                                                                                                                                                                                       (__toplevel_cons
                                                                                                                                                                                                                                                                                                                                          (__toplevel_cons
                                                                                                                                                                                                                                                                                                                                             'equal
                                                                                                                                                                                                                                                                                                                                             (__toplevel_cons
                                                                                                                                                                                                                                                                                                                                                (__toplevel_cons 'car (__toplevel_cons (__toplevel_cons 'gopher (__toplevel_cons 'x ())) ()))
                                                                                                                                                                                                                                                                                                                                                (__toplevel_cons
                                                                                                                                                                                                                                                                                                                                                   (__toplevel_cons
                                                                                                                                                                                                                                                                                                                                                      'if
                                                                                                                                                                                                                                                                                                                                                      (__toplevel_cons
                                                                                                                                                                                                                                                                                                                                                         (__toplevel_cons 'listp (__toplevel_cons 'x ()))
                                                                                                                                                                                                                                                                                                                                                         (__toplevel_cons
                                                                                                                                                                                                                                                                                                                                                            (__toplevel_cons 'car (__toplevel_cons (__toplevel_cons 'flatten (__toplevel_cons 'x ())) ()))
                                                                                                                                                                                                                                                                                                                                                            (__toplevel_cons (__toplevel_cons 'zero ()) ()))))
                                                                                                                                                                                                                                                                                                                                                   ())))
                                                                                                                                                                                                                                                                                                                                          (__toplevel_cons
                                                                                                                                                                                                                                                                                                                                             (__toplevel_cons
                                                                                                                                                                                                                                                                                                                                                'equal
                                                                                                                                                                                                                                                                                                                                                (__toplevel_cons
                                                                                                                                                                                                                                                                                                                                                   (__toplevel_cons
                                                                                                                                                                                                                                                                                                                                                      'flatten
                                                                                                                                                                                                                                                                                                                                                      (__toplevel_cons
                                                                                                                                                                                                                                                                                                                                                         (__toplevel_cons 'cdr (__toplevel_cons (__toplevel_cons 'gopher (__toplevel_cons 'x ())) ()))
                                                                                                                                                                                                                                                                                                                                                         ()))
                                                                                                                                                                                                                                                                                                                                                   (__toplevel_cons
                                                                                                                                                                                                                                                                                                                                                      (__toplevel_cons
                                                                                                                                                                                                                                                                                                                                                         'if
                                                                                                                                                                                                                                                                                                                                                         (__toplevel_cons
                                                                                                                                                                                                                                                                                                                                                            (__toplevel_cons 'listp (__toplevel_cons 'x ()))
                                                                                                                                                                                                                                                                                                                                                            (__toplevel_cons
                                                                                                                                                                                                                                                                                                                                                               (__toplevel_cons 'cdr (__toplevel_cons (__toplevel_cons 'flatten (__toplevel_cons 'x ())) ()))
                                                                                                                                                                                                                                                                                                                                                               (__toplevel_cons
                                                                                                                                                                                                                                                                                                                                                                  (__toplevel_cons
                                                                                                                                                                                                                                                                                                                                                                     'cons
                                                                                                                                                                                                                                                                                                                                                                     (__toplevel_cons (__toplevel_cons 'zero ()) (__toplevel_cons (__toplevel_cons 'nil ()) ())))
                                                                                                                                                                                                                                                                                                                                                                  ()))))
                                                                                                                                                                                                                                                                                                                                                      ())))
                                                                                                                                                                                                                                                                                                                                             (__toplevel_cons
                                                                                                                                                                                                                                                                                                                                                (__toplevel_cons
                                                                                                                                                                                                                                                                                                                                                   'equal
                                                                                                                                                                                                                                                                                                                                                   (__toplevel_cons
                                                                                                                                                                                                                                                                                                                                                      (__toplevel_cons
                                                                                                                                                                                                                                                                                                                                                         'quotient
                                                                                                                                                                                                                                                                                                                                                         (__toplevel_cons
                                                                                                                                                                                                                                                                                                                                                            (__toplevel_cons 'times (__toplevel_cons 'y (__toplevel_cons 'x ())))
                                                                                                                                                                                                                                                                                                                                                            (__toplevel_cons 'y ())))
                                                                                                                                                                                                                                                                                                                                                      (__toplevel_cons
                                                                                                                                                                                                                                                                                                                                                         (__toplevel_cons
                                                                                                                                                                                                                                                                                                                                                            'if
                                                                                                                                                                                                                                                                                                                                                            (__toplevel_cons
                                                                                                                                                                                                                                                                                                                                                               (__toplevel_cons 'zerop (__toplevel_cons 'y ()))
                                                                                                                                                                                                                                                                                                                                                               (__toplevel_cons
                                                                                                                                                                                                                                                                                                                                                                  (__toplevel_cons 'zero ())
                                                                                                                                                                                                                                                                                                                                                                  (__toplevel_cons (__toplevel_cons 'fix (__toplevel_cons 'x ())) ()))))
                                                                                                                                                                                                                                                                                                                                                         ())))
                                                                                                                                                                                                                                                                                                                                                (__toplevel_cons
                                                                                                                                                                                                                                                                                                                                                   (__toplevel_cons
                                                                                                                                                                                                                                                                                                                                                      'equal
                                                                                                                                                                                                                                                                                                                                                      (__toplevel_cons
                                                                                                                                                                                                                                                                                                                                                         (__toplevel_cons
                                                                                                                                                                                                                                                                                                                                                            'get
                                                                                                                                                                                                                                                                                                                                                            (__toplevel_cons
                                                                                                                                                                                                                                                                                                                                                               'j
                                                                                                                                                                                                                                                                                                                                                               (__toplevel_cons
                                                                                                                                                                                                                                                                                                                                                                  (__toplevel_cons 'set (__toplevel_cons 'i (__toplevel_cons 'val (__toplevel_cons 'mem ()))))
                                                                                                                                                                                                                                                                                                                                                                  ())))
                                                                                                                                                                                                                                                                                                                                                         (__toplevel_cons
                                                                                                                                                                                                                                                                                                                                                            (__toplevel_cons
                                                                                                                                                                                                                                                                                                                                                               'if
                                                                                                                                                                                                                                                                                                                                                               (__toplevel_cons
                                                                                                                                                                                                                                                                                                                                                                  (__toplevel_cons 'eqp (__toplevel_cons 'j (__toplevel_cons 'i ())))
                                                                                                                                                                                                                                                                                                                                                                  (__toplevel_cons
                                                                                                                                                                                                                                                                                                                                                                     'val
                                                                                                                                                                                                                                                                                                                                                                     (__toplevel_cons (__toplevel_cons 'get (__toplevel_cons 'j (__toplevel_cons 'mem ()))) ()))))
                                                                                                                                                                                                                                                                                                                                                            ())))
                                                                                                                                                                                                                                                                                                                                                   ())))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
         (tautologyp (lambda (x true-lst false-lst)
                       (<change>
                          @sensitivity:No
                          (if (truep x true-lst)
                             #t
                             (if (falsep x false-lst)
                                #f
                                (if (not (pair? x))
                                   #f
                                   (if (eq? (car x) 'if)
                                      (if (truep (cadr x) true-lst)
                                         (tautologyp (caddr x) true-lst false-lst)
                                         (if (not (falsep (cadr x) false-lst))
                                            (tautologyp (cadddr x) true-lst false-lst)
                                            (if (tautologyp (caddr x) (cons (cadr x) true-lst) false-lst)
                                               (tautologyp (cadddr x) true-lst (cons (cadr x) false-lst))
                                               #f)))
                                      #f)))))
                       (<change>
                          (if (truep x true-lst)
                             #t
                             (if (falsep x false-lst)
                                #f
                                (if (not (pair? x))
                                   #f
                                   (if (eq? (car x) 'if)
                                      (if (truep (cadr x) true-lst)
                                         (tautologyp (caddr x) true-lst false-lst)
                                         (if (falsep (cadr x) false-lst)
                                            (tautologyp (cadddr x) true-lst false-lst)
                                            (if (tautologyp (caddr x) (cons (cadr x) true-lst) false-lst)
                                               (tautologyp (cadddr x) true-lst (cons (cadr x) false-lst))
                                               #f)))
                                      #f))))
                          @sensitivity:No)))
         (tautp (lambda (x)
                  @sensitivity:FA
                  (tautologyp (rewrite x) () ())))
         (test (lambda ()
                 (<change>
                    @sensitivity:FA
                    (let ((ans #f)
                          (term #f))
                       (set! term (apply-subst
                                  (__toplevel_cons
                                     (__toplevel_cons
                                        'x
                                        (__toplevel_cons
                                           'f
                                           (__toplevel_cons
                                              (__toplevel_cons
                                                 'plus
                                                 (__toplevel_cons
                                                    (__toplevel_cons 'plus (__toplevel_cons 'a (__toplevel_cons 'b ())))
                                                    (__toplevel_cons
                                                       (__toplevel_cons 'plus (__toplevel_cons 'c (__toplevel_cons (__toplevel_cons 'zero ()) ())))
                                                       ())))
                                              ())))
                                     (__toplevel_cons
                                        (__toplevel_cons
                                           'y
                                           (__toplevel_cons
                                              'f
                                              (__toplevel_cons
                                                 (__toplevel_cons
                                                    'times
                                                    (__toplevel_cons
                                                       (__toplevel_cons 'times (__toplevel_cons 'a (__toplevel_cons 'b ())))
                                                       (__toplevel_cons (__toplevel_cons 'plus (__toplevel_cons 'c (__toplevel_cons 'd ()))) ())))
                                                 ())))
                                        (__toplevel_cons
                                           (__toplevel_cons
                                              'z
                                              (__toplevel_cons
                                                 'f
                                                 (__toplevel_cons
                                                    (__toplevel_cons
                                                       'reverse
                                                       (__toplevel_cons
                                                          (__toplevel_cons
                                                             'append
                                                             (__toplevel_cons
                                                                (__toplevel_cons 'append (__toplevel_cons 'a (__toplevel_cons 'b ())))
                                                                (__toplevel_cons (__toplevel_cons 'nil ()) ())))
                                                          ()))
                                                    ())))
                                           (__toplevel_cons
                                              (__toplevel_cons
                                                 'u
                                                 (__toplevel_cons
                                                    'equal
                                                    (__toplevel_cons
                                                       (__toplevel_cons 'plus (__toplevel_cons 'a (__toplevel_cons 'b ())))
                                                       (__toplevel_cons (__toplevel_cons 'difference (__toplevel_cons 'x (__toplevel_cons 'y ()))) ()))))
                                              (__toplevel_cons
                                                 (__toplevel_cons
                                                    'w
                                                    (__toplevel_cons
                                                       'lessp
                                                       (__toplevel_cons
                                                          (__toplevel_cons 'remainder (__toplevel_cons 'a (__toplevel_cons 'b ())))
                                                          (__toplevel_cons
                                                             (__toplevel_cons
                                                                'member
                                                                (__toplevel_cons 'a (__toplevel_cons (__toplevel_cons 'length (__toplevel_cons 'b ())) ())))
                                                             ()))))
                                                 ())))))
                                  (__toplevel_cons
                                     'implies
                                     (__toplevel_cons
                                        (__toplevel_cons
                                           'and
                                           (__toplevel_cons
                                              (__toplevel_cons 'implies (__toplevel_cons 'x (__toplevel_cons 'y ())))
                                              (__toplevel_cons
                                                 (__toplevel_cons
                                                    'and
                                                    (__toplevel_cons
                                                       (__toplevel_cons 'implies (__toplevel_cons 'y (__toplevel_cons 'z ())))
                                                       (__toplevel_cons
                                                          (__toplevel_cons
                                                             'and
                                                             (__toplevel_cons
                                                                (__toplevel_cons 'implies (__toplevel_cons 'z (__toplevel_cons 'u ())))
                                                                (__toplevel_cons (__toplevel_cons 'implies (__toplevel_cons 'u (__toplevel_cons 'w ()))) ())))
                                                          ())))
                                                 ())))
                                        (__toplevel_cons (__toplevel_cons 'implies (__toplevel_cons 'x (__toplevel_cons 'w ()))) ())))))
                       (set! ans (tautp term))
                       ans))
                 (<change>
                    (let ((ans #f)
                          (term #f))
                       (set! term (apply-subst
                                  (__toplevel_cons
                                     (__toplevel_cons
                                        'x
                                        (__toplevel_cons
                                           'f
                                           (__toplevel_cons
                                              (__toplevel_cons
                                                 'plus
                                                 (__toplevel_cons
                                                    (__toplevel_cons 'plus (__toplevel_cons 'a (__toplevel_cons 'b ())))
                                                    (__toplevel_cons
                                                       (__toplevel_cons 'plus (__toplevel_cons 'c (__toplevel_cons (__toplevel_cons 'zero ()) ())))
                                                       ())))
                                              ())))
                                     (__toplevel_cons
                                        (__toplevel_cons
                                           'y
                                           (__toplevel_cons
                                              'f
                                              (__toplevel_cons
                                                 (__toplevel_cons
                                                    'times
                                                    (__toplevel_cons
                                                       (__toplevel_cons 'times (__toplevel_cons 'a (__toplevel_cons 'b ())))
                                                       (__toplevel_cons (__toplevel_cons 'plus (__toplevel_cons 'c (__toplevel_cons 'd ()))) ())))
                                                 ())))
                                        (__toplevel_cons
                                           (__toplevel_cons
                                              'z
                                              (__toplevel_cons
                                                 'f
                                                 (__toplevel_cons
                                                    (__toplevel_cons
                                                       'reverse
                                                       (__toplevel_cons
                                                          (__toplevel_cons
                                                             'append
                                                             (__toplevel_cons
                                                                (__toplevel_cons 'append (__toplevel_cons 'a (__toplevel_cons 'b ())))
                                                                (__toplevel_cons (__toplevel_cons 'nil ()) ())))
                                                          ()))
                                                    ())))
                                           (__toplevel_cons
                                              (__toplevel_cons
                                                 'u
                                                 (__toplevel_cons
                                                    'equal
                                                    (__toplevel_cons
                                                       (__toplevel_cons 'plus (__toplevel_cons 'a (__toplevel_cons 'b ())))
                                                       (__toplevel_cons (__toplevel_cons 'difference (__toplevel_cons 'x (__toplevel_cons 'y ()))) ()))))
                                              (__toplevel_cons
                                                 (__toplevel_cons
                                                    'w
                                                    (__toplevel_cons
                                                       'lessp
                                                       (__toplevel_cons
                                                          (__toplevel_cons 'remainder (__toplevel_cons 'a (__toplevel_cons 'b ())))
                                                          (__toplevel_cons
                                                             (__toplevel_cons
                                                                'member
                                                                (__toplevel_cons 'a (__toplevel_cons (__toplevel_cons 'length (__toplevel_cons 'b ())) ())))
                                                             ()))))
                                                 ())))))
                                  (__toplevel_cons
                                     'implies
                                     (__toplevel_cons
                                        (__toplevel_cons
                                           'and
                                           (__toplevel_cons
                                              (__toplevel_cons 'implies (__toplevel_cons 'x (__toplevel_cons 'y ())))
                                              (__toplevel_cons
                                                 (__toplevel_cons
                                                    'and
                                                    (__toplevel_cons
                                                       (__toplevel_cons 'implies (__toplevel_cons 'y (__toplevel_cons 'z ())))
                                                       (__toplevel_cons
                                                          (__toplevel_cons
                                                             'and
                                                             (__toplevel_cons
                                                                (__toplevel_cons 'implies (__toplevel_cons 'z (__toplevel_cons 'u ())))
                                                                (__toplevel_cons (__toplevel_cons 'implies (__toplevel_cons 'u (__toplevel_cons 'w ()))) ())))
                                                          ())))
                                                 ())))
                                        (__toplevel_cons (__toplevel_cons 'implies (__toplevel_cons 'x (__toplevel_cons 'w ()))) ())))))
                       (set! ans (tautp term))
                       ans)
                    @sensitivity:FA)))
         (trans-of-implies (lambda (n)
                             (<change>
                                @sensitivity:FA
                                ())
                             (cons 'implies (cons (trans-of-implies1 n) (cons (cons 'implies (cons 0 (cons n ()))) ())))))
         (trans-of-implies1 (lambda (n)
                              @sensitivity:FA
                              (if (equal? n 1)
                                 (cons 'implies (cons 0 (cons 1 ())))
                                 (cons 'and (cons (cons 'implies (cons (- n 1) (cons n ()))) (cons (trans-of-implies1 (- n 1)) ()))))))
         (truep (lambda (x lst)
                  (<change>
                     @sensitivity:FA
                     ())
                  (let ((__or_res (equal? x (__toplevel_cons 't ()))))
                     (if __or_res __or_res (member x lst))))))
   (<change>
      ()
      (display (setup)))
   (setup)
   (test))