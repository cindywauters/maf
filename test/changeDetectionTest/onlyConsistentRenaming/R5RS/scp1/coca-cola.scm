;; renamed lambdas/lets: 6
 
(define foldr (lambda (f base lst)
      (define foldr-aux (lambda (lst)
            (if (null? lst)
               base
               (f (car lst) (foldr-aux (cdr lst))))))
      (foldr-aux lst)))
 
(define atom? (lambda (x)
      (not (pair? x))))
 
(define Coca-Cola-NV (__toplevel_cons
      'Coca-Cola-NV
      (__toplevel_cons
         (__toplevel_cons
            'Frisdranken
            (__toplevel_cons
               (__toplevel_cons
                  'Coca-Cola
                  (__toplevel_cons
                     (__toplevel_cons
                        'Regular-Coca-Cola
                        (__toplevel_cons (__toplevel_cons 'Coke (__toplevel_cons (__toplevel_cons 10000000 ()) ())) ()))
                     (__toplevel_cons
                        (__toplevel_cons
                           'light-Coca-Cola
                           (__toplevel_cons
                              (__toplevel_cons 'Coke-Light (__toplevel_cons (__toplevel_cons 800000 ()) ()))
                              (__toplevel_cons (__toplevel_cons 'Coke-Zero (__toplevel_cons (__toplevel_cons 200000 ()) ())) ())))
                        ())))
               (__toplevel_cons
                  (__toplevel_cons
                     'Fanta
                     (__toplevel_cons
                        (__toplevel_cons 'Fanta-Orange (__toplevel_cons (__toplevel_cons 800000 ()) ()))
                        (__toplevel_cons
                           (__toplevel_cons 'Fanta-Lemon (__toplevel_cons (__toplevel_cons 200000 ()) ()))
                           ())))
                  (__toplevel_cons
                     (__toplevel_cons
                        'Sprite
                        (__toplevel_cons
                           (__toplevel_cons 'Sprite-Zero (__toplevel_cons (__toplevel_cons 1000000 ()) ()))
                           ()))
                     ()))))
         (__toplevel_cons
            (__toplevel_cons
               'Sappen
               (__toplevel_cons
                  (__toplevel_cons
                     'Minute-Maid
                     (__toplevel_cons
                        (__toplevel_cons 'Minute-Maid-Sinaas (__toplevel_cons (__toplevel_cons 2000000 ()) ()))
                        (__toplevel_cons
                           (__toplevel_cons 'Minute-Maid-Tomaat (__toplevel_cons (__toplevel_cons 1000000 ()) ()))
                           ())))
                  ()))
            ()))))
 
(define omzetcijfer (<change>
      (lambda (categorie)
         (caadr categorie))
      (lambda (_categorie0)
         (caadr _categorie0))))
 
(define heeft-omzetcijfer (lambda (categorie)
      (if (pair? categorie)
         (if (pair? (cadr categorie))
            (if (atom? (caadr categorie))
               (number? (caadr categorie))
               #f)
            #f)
         #f)))
 
(define deel-categorien (<change>
      (lambda (categorie)
         (cdr categorie))
      (lambda (_categorie0)
         (cdr _categorie0))))
 
(define hoofdcategorie (lambda (categorie)
      (car categorie)))
 
(define bereken (lambda (lst)
      (if (null? lst)
         0
         (if (atom? lst)
            0
            (if (number? (car lst))
               (car lst)
               (+ (bereken (car lst)) (bereken (cdr lst))))))))
 
(define omzet (<change>
      (lambda (bedrijf categorie)
         (if (eq? (hoofdcategorie bedrijf) categorie)
            (bereken bedrijf)
            (omzet-in (deel-categorien bedrijf) categorie)))
      (lambda (_bedrijf0 _categorie0)
         (if (eq? (hoofdcategorie _bedrijf0) _categorie0)
            (bereken _bedrijf0)
            (omzet-in (deel-categorien _bedrijf0) _categorie0)))))
 
(define omzet-in (<change>
      (lambda (lst categorie)
         (if (null? lst)
            #f
            (let ((__or_res (omzet (car lst) categorie)))
               (if __or_res
                  __or_res
                  (omzet-in (cdr lst) categorie)))))
      (lambda (_lst0 _categorie0)
         (if (null? _lst0)
            #f
            (let ((___or_res0 (omzet (car _lst0) _categorie0)))
               (if ___or_res0
                  ___or_res0
                  (omzet-in (cdr _lst0) _categorie0)))))))
 
(define collect-pairs (lambda (bedrijf)
      (if (heeft-omzetcijfer bedrijf)
         (list (list (hoofdcategorie bedrijf) (omzetcijfer bedrijf)))
         (collect-pairs-in (deel-categorien bedrijf)))))
 
(define collect-pairs-in (<change>
      (lambda (lst)
         (if (null? lst)
            ()
            (append (collect-pairs (car lst)) (collect-pairs-in (cdr lst)))))
      (lambda (_lst0)
         (if (null? _lst0)
            ()
            (append (collect-pairs (car _lst0)) (collect-pairs-in (cdr _lst0)))))))
 
(define verdeel-democratisch (lambda (bedrijf budget)
      (let* ((pairs (collect-pairs bedrijf))
             (total (foldr + 0 (map cadr pairs)))
             (factor (/ budget total)))
         (map (lambda (x) (list (car x) (* factor (cadr x)))) pairs))))
 
(define verdeel (lambda (bedrijf budget)
      (if (heeft-omzetcijfer bedrijf)
         (list (hoofdcategorie bedrijf) budget)
         (let* ((rest (deel-categorien bedrijf))
                (new-budget (/ budget (length rest))))
            (cons (hoofdcategorie bedrijf) (verdeel-in rest new-budget))))))
 
(define verdeel-in (<change>
      (lambda (lst budget)
         (if (null? lst)
            ()
            (cons (verdeel (car lst) budget) (verdeel-in (cdr lst) budget))))
      (lambda (_lst0 _budget0)
         (if (null? _lst0)
            ()
            (cons (verdeel (car _lst0) _budget0) (verdeel-in (cdr _lst0) _budget0))))))
 
(if (= (omzet Coca-Cola-NV 'Coca-Cola) 11000000)
   (if (= (omzet Coca-Cola-NV 'Sprite) 1000000)
      (if (= (omzet Coca-Cola-NV 'Minute-Maid) 3000000)
         (if (equal? (verdeel-democratisch Coca-Cola-NV 128000000) (__toplevel_cons (__toplevel_cons 'Coke (__toplevel_cons 80000000 ())) (__toplevel_cons (__toplevel_cons 'Coke-Light (__toplevel_cons 6400000 ())) (__toplevel_cons (__toplevel_cons 'Coke-Zero (__toplevel_cons 1600000 ())) (__toplevel_cons (__toplevel_cons 'Fanta-Orange (__toplevel_cons 6400000 ())) (__toplevel_cons (__toplevel_cons 'Fanta-Lemon (__toplevel_cons 1600000 ())) (__toplevel_cons (__toplevel_cons 'Sprite-Zero (__toplevel_cons 8000000 ())) (__toplevel_cons (__toplevel_cons 'Minute-Maid-Sinaas (__toplevel_cons 16000000 ())) (__toplevel_cons (__toplevel_cons 'Minute-Maid-Tomaat (__toplevel_cons 8000000 ())) ())))))))))
            (equal?
               (verdeel Coca-Cola-NV 1200000)
               (__toplevel_cons
                  'Coca-Cola-NV
                  (__toplevel_cons
                     (__toplevel_cons
                        'Frisdranken
                        (__toplevel_cons
                           (__toplevel_cons
                              'Coca-Cola
                              (__toplevel_cons
                                 (__toplevel_cons
                                    'Regular-Coca-Cola
                                    (__toplevel_cons (__toplevel_cons 'Coke (__toplevel_cons 100000 ())) ()))
                                 (__toplevel_cons
                                    (__toplevel_cons
                                       'light-Coca-Cola
                                       (__toplevel_cons
                                          (__toplevel_cons 'Coke-Light (__toplevel_cons 50000 ()))
                                          (__toplevel_cons (__toplevel_cons 'Coke-Zero (__toplevel_cons 50000 ())) ())))
                                    ())))
                           (__toplevel_cons
                              (__toplevel_cons
                                 'Fanta
                                 (__toplevel_cons
                                    (__toplevel_cons 'Fanta-Orange (__toplevel_cons 100000 ()))
                                    (__toplevel_cons (__toplevel_cons 'Fanta-Lemon (__toplevel_cons 100000 ())) ())))
                              (__toplevel_cons
                                 (__toplevel_cons
                                    'Sprite
                                    (__toplevel_cons (__toplevel_cons 'Sprite-Zero (__toplevel_cons 200000 ())) ()))
                                 ()))))
                     (__toplevel_cons
                        (__toplevel_cons
                           'Sappen
                           (__toplevel_cons
                              (__toplevel_cons
                                 'Minute-Maid
                                 (__toplevel_cons
                                    (__toplevel_cons 'Minute-Maid-Sinaas (__toplevel_cons 300000 ()))
                                    (__toplevel_cons (__toplevel_cons 'Minute-Maid-Tomaat (__toplevel_cons 300000 ())) ())))
                              ()))
                        ()))))
            #f)
         #f)
      #f)
   #f)
 
