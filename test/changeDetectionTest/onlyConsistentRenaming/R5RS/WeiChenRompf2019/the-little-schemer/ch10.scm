;; renamed lambdas/lets: 16
 
(define atom? (lambda (x)
      (if (not (null? x)) (not (pair? x)) #f)))
 
(define first (<change>
      (lambda (l)
         (car l))
      (lambda (_l0)
         (car _l0))))
 
(define second (lambda (l)
      (car (cdr l))))
 
(define third (lambda (l)
      (caddr l)))
 
(define add1 (<change>
      (lambda (x)
         (+ x 1))
      (lambda (_x0)
         (+ _x0 1))))
 
(define sub1 (<change>
      (lambda (x)
         (- x 1))
      (lambda (_x0)
         (- _x0 1))))
 
(define build (<change>
      (lambda (a b)
         (cons a (cons b ())))
      (lambda (_a0 _b0)
         (cons _a0 (cons _b0 ())))))
 
(define new-entry build)
 
(define lookup-in-entry-help (lambda (name names values entry-f)
      (if (null? names)
         (entry-f name)
         (if (eq? (car names) name)
            (car values)
            (lookup-in-entry-help name (cdr names) (cdr values) entry-f)))))
 
(define lookup-in-entry (<change>
      (lambda (name entry entry-f)
         (lookup-in-entry-help name (first entry) (second entry) entry-f))
      (lambda (_name0 _entry0 _entry-f0)
         (lookup-in-entry-help _name0 (first _entry0) (second _entry0) _entry-f0))))
 
(define extend-table cons)
 
(define lookup-in-table (lambda (name table table-f)
      (if (null? table)
         (table-f name)
         (lookup-in-entry name (car table) (lambda (name) (lookup-in-table name (car table) table-f))))))
 
(define expression-to-action (<change>
      (lambda (e)
         (if (atom? e)
            (atom-to-action e)
            (list-to-action e)))
      (lambda (_e0)
         (if (atom? _e0)
            (atom-to-action _e0)
            (list-to-action _e0)))))
 
(define atom-to-action (lambda (e)
      (if (number? e)
         *const
         (if (eq? e #t)
            *const
            (if (eq? e #f)
               *const
               (if (eq? e 'cons)
                  *const
                  (if (eq? e 'car)
                     *const
                     (if (eq? e 'cdr)
                        *const
                        (if (eq? e 'null?)
                           *const
                           (if (eq? e 'eq?)
                              *const
                              (if (eq? e 'atom?)
                                 *const
                                 (if (eq? e 'zero?)
                                    *const
                                    (if (eq? e 'add1)
                                       *const
                                       (if (eq? e 'sub1)
                                          *const
                                          (if (eq? e '+)
                                             *const
                                             (if (eq? e '-)
                                                *const
                                                (if (eq? e 'number?) *const *identifier)))))))))))))))))
 
(define list-to-action (<change>
      (lambda (e)
         (if (atom? (car e))
            (if (eq? (car e) 'quote)
               *quote
               (if (eq? (car e) 'lambda)
                  *lambda
                  (if (eq? (car e) 'cond) *cond *application)))
            *application))
      (lambda (_e0)
         (if (atom? (car _e0))
            (if (eq? (car _e0) 'quote)
               *quote
               (if (eq? (car _e0) 'lambda)
                  *lambda
                  (if (eq? (car _e0) 'cond) *cond *application)))
            *application))))
 
(define value (lambda (e)
      (meaning e ())))
 
(define meaning (<change>
      (lambda (e table)
         ((expression-to-action e) e table))
      (lambda (_e0 _table0)
         ((expression-to-action _e0) _e0 _table0))))
 
(define *const (<change>
      (lambda (e table)
         (if (number? e)
            e
            (if (eq? e #t)
               #t
               (if (eq? e #f) #f (build 'primitive e)))))
      (lambda (_e0 _table0)
         (if (number? _e0)
            _e0
            (if (eq? _e0 #t)
               #t
               (if (eq? _e0 #f) #f (build 'primitive _e0)))))))
 
(define text-of second)
 
(define *quote (lambda (e table)
      (text-of e)))
 
(define *identifier (<change>
      (lambda (e table)
         (lookup-in-table e table initial-table))
      (lambda (_e0 _table0)
         (lookup-in-table _e0 _table0 initial-table))))
 
(define initial-table (lambda (name)
      (car ())))
 
(define *lambda (lambda (e table)
      (build 'non-primitive (cons table (cdr e)))))
 
(define table-of first)
 
(define formals-of second)
 
(define body-of third)
 
(define evcon (<change>
      (lambda (lines table)
         (if (else? (question-of (car lines)))
            (meaning (answer-of (car lines)) table)
            (if (meaning (question-of (car lines)) table)
               (meaning (answer-of (car lines)) table)
               (evcon (cdr lines) table))))
      (lambda (_lines0 _table0)
         (if (else? (question-of (car _lines0)))
            (meaning (answer-of (car _lines0)) _table0)
            (if (meaning (question-of (car _lines0)) _table0)
               (meaning (answer-of (car _lines0)) _table0)
               (evcon (cdr _lines0) _table0))))))
 
(define else? (<change>
      (lambda (x)
         (if (atom? x) (eq? x 'else) #f))
      (lambda (_x0)
         (if (atom? _x0) (eq? _x0 'else) #f))))
 
(define question-of first)
 
(define answer-of second)
 
(define *cond (<change>
      (lambda (e table)
         (evcon (cond-lines-of e) table))
      (lambda (_e0 _table0)
         (evcon (cond-lines-of _e0) _table0))))
 
(define cond-lines-of cdr)
 
(define evlis (<change>
      (lambda (args table)
         (if (null? args)
            ()
            (cons (meaning (car args) table) (evlis (cdr args) table))))
      (lambda (_args0 _table0)
         (if (null? _args0)
            ()
            (cons (meaning (car _args0) _table0) (evlis (cdr _args0) _table0))))))
 
(define *application (lambda (e table)
      (myapply (meaning (function-of e) table) (evlis (arguments-of e) table))))
 
(define function-of car)
 
(define arguments-of cdr)
 
(define primitive? (lambda (l)
      (eq? (first l) 'primitive)))
 
(define non-primitive? (<change>
      (lambda (l)
         (eq? (first l) 'non-primitive))
      (lambda (_l0)
         (eq? (first _l0) 'non-primitive))))
 
(define myapply (lambda (fun vals)
      (if (primitive? fun)
         (apply-primitive (second fun) vals)
         (if (non-primitive? fun)
            (apply-closure (second fun) vals)
            #f))))
 
(define apply-primitive (lambda (name vals)
      (if (eq? name 'cons)
         (cons (first vals) (second vals))
         (if (eq? name 'car)
            (car (first vals))
            (if (eq? name 'cdr)
               (cdr (first vals))
               (if (eq? name 'null?)
                  (null? (first vals))
                  (if (eq? name 'eq?)
                     (eq? (first vals) (second vals))
                     (if (eq? name 'atom?)
                        (:atom? (first vals))
                        (if (eq? name 'zero?)
                           (zero? (first vals))
                           (if (eq? name 'add1)
                              (add1 (first vals))
                              (if (eq? name 'sub1)
                                 (sub1 (first vals))
                                 (if (eq? name '+)
                                    (+ (first vals) (second vals))
                                    (if (eq? name '-)
                                       (- (first vals) (second vals))
                                       (if (eq? name 'numbers?)
                                          (number? (first vals))
                                          #f))))))))))))))
 
(define :atom? (lambda (x)
      (if (atom? x)
         #t
         (if (null? x)
            #f
            (if (eq? (car x) 'primitive)
               #t
               (if (eq? (car x) 'non-primitive) #t #f))))))
 
(define apply-closure (lambda (closure vals)
      (meaning (body-of closure) (extend-table (new-entry (formals-of closure) vals) (table-of closure)))))
 
(define eval2 value)
 
