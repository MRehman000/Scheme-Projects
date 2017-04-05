;Syntax Checker

;Does not work for nested lambda

(define (atom? x)
  (and (not (pair? x))
       (not (null? x))))


(define (ck e)
  (cond
    ((atom? e)
     (atom-check e))
    (else (list-check e))))

(define (atom-check e)
  (cond
    ((number? e) (const-check e))
    ((eq? e #t) (const-check e))
    ((eq? e #f) (const-check e))
    ((eq? e (quote cons)) (const-check e))
    ((eq? e (quote car)) (const-check e))
    ((eq? e (quote cdr)) (const-check e))
    ((eq? e (quote null?)) (const-check e))
    ((eq? e (quote eq?)) (const-check e))
    ((eq? e (quote atom?)) (const-check e))
    ((eq? e (quote zero?)) (const-check e))
    ((eq? e (quote add1)) (const-check e))
    ((eq? e (quote mul)) (const-check e))
    ((eq? e (quote sub1)) (const-check e))
    ((eq? e (quote number?)) (const-check e))
    (else #t)); This is an identifier
  )

(define (list-check e)
  (cond
    ((or (atom? (car e))
         (atom? (car (car e))))
     (cond
       ((eq? (car e) (quote quote))
        (quote-check e))
       ((or (eq? (car e) (quote lambda))
            (eq? (car (car e)) (quote lambda)))
        (lambda-check e))
       ((eq? (car e) (quote cond))
        (cond-check e))
       (else (application-check e))))
    (else (application-check e))))


(define (quote-check e)
  #t) ;If we have quote that means anything can go after it
; so it will be true always, we don't have to check the
; the text of quote

(define first car)

(define second cadr)

(define third caddr)



(define (member? a lst)
  (if (null? lst) #f
      (or (eq? a (car lst))
          (member? a (cdr lst)))))

(define (covered? los lexp)
  (cond
    ((null? lexp) #t)
    ((and (atom? los)
          (eq? (length lexp) 1))
     (eq? (car lexp) los))
    ((member? (car lexp) los)
     (and #t (covered? (cdr lexp) los)))
    (else
     #f
     )))

(define (lambda-check e)
  (cond
    ((eq? (length e) 3) ;Meaning we don't have values for the vars
     (ck (third e)))
    ((eq? (length e) 2) ;meaning we have values for the vars
     (and (covered? (second (first e)) (get-variables (third (first e))))
          (ck (third (first e)))))
    (else #f)))

(define los '(cons car cdr null? eq? atom? zero? add1 mul sub1 number? #t #f lambda))

(define (get-variables e)
  (cond
    ((null? e) (quote ()))
    ((atom? e)
     (cond
       ((and (not (member? e los))
              (not (number? e)))
         e)
       ))
    ((atom? (car e))
     (cond
       ((and (not (member? (car e) los))
              (not (number? (car e))))
         (cons (car e) (get-variables (cdr e))))
       (else
        (get-variables (cdr e)))))
    (else
     (cons (get-variables (car e)) (get-variables (cdr e))))))



(define (cond-check e)
  (evcon-check (cdr e)))

(define (evcon-check e)
  (cond
    ((else? (car (car e)))
     (ck (cadr (car e))))
    ((ck (car (car e)))
     (ck (cadr (car e))))
    (else (evcon-check (cdr e)))))

(define else?
  (lambda (x)
    (cond 
      ((atom? x) (eq? x (quote else)))
      (else #f))))


(define (const-check e)
  (cond
    ((number? e) #t)
    ((eq? e #t) #t)
    ((eq? e #f) #t)
    (else (primitive-check e))))

(define (primitive-check e)
  (if (atom? e) #t ;This means we just have the primitive thats all
      (let ((name (car e)))
        (cond
          ((eq? name (quote cons))
           (and (eq? (length e) 2)
                (ck (cadr e)) (ck (caddr e))))
          ((eq? name (quote car))
           (and (eq? (length e) 2)
                (ck (cdr e))))
          ((eq? name (quote cdr))
           (and (eq? (length e) 2)
                (ck (cdr e))))
          ((eq? name (quote null?))
           (and (eq? (length e) 2)
                (ck (cdr e))))
          ((eq? name (quote eq?))
           (and (eq? (length e) 2)
                (ck (cdr e))))
          ((eq? name (quote atom?))
           (and (eq? (length e) 2)
                (ck (cdr e))))
          ((eq? name (quote zero?))
           (and (eq? (length e) 2)
                (ck (cdr e))))
          ((eq? name (quote add1))
           (and (eq? (length e) 2)
                (ck (cdr e))))
          ((eq? name (quote mul))
           (and (eq? (length e) 3)
                (ck (cadr e)) (ck (caddr e))))
          ((eq? name (quote sub1))
           (and (eq? (length e) 2)
                (ck (cdr e))))
          ((eq? name (quote number?))
           (and (eq? (length e) 2)
                (ck (cdr e))))
          (else
           #t)))))




(define application-check primitive-check)

;(ck '1); -- throws an error

;(ck 'x) ;-- throws an error

;(ck 'mul); -- ditto

;(ck 'cons); -- ditto

;(ck '(mul 1 2)) ;-- returns #f, which is wrong

;(ck '(mul 1 2 3)) 

;(ck '(mul (add1 2 3) 3))

;(ck '(car 1))

;(ck '(cond (1 2))) ;-- returns #f, which is wrong

;(ck '(cond ((1 2) (3 4)))); --- returns #f, which is wrong 

;(ck '(quote x))

;(ck '(lambda (x) 1))


;(ck '(lambda (x) 1 2))

;(ck '(lambda (x) x))

;(ck '((lambda (x) x) 2))

;(ck '(lambda (x) y))

;(ck '((lambda (x) y) 2)) ;Should be false


(ck '((lambda (x) 
	(mul x ((lambda (y) (mul x y))
                       2)))
        3))

(ck '((lambda (x) 
        (mul x ((lambda (y) (mul w y))
                2)))
      3)) 




#|
Proof by induction:
Basis: This works for basic inputs such as
(ck '(cons 'a 'b)) = 
(ck '1)
(ck 'x)
(ck 'mul)

Induction hypothesis: We are inducting on the length of e, meaning we willl reduce the length of e as the program
                      continues by simplifying it and evaluating nested expression

Induction step: Since we assume that this works for the basic inputs. If we have a nested input such as
'(cons (car '(a b c)) '(cdr '(a b c))) we will end up dividing it into smaller steps. For example
first we will check is the length is the right length. Then we will perform a check on each of the
statements in the list. If at any point, the check fails, you will get#f.
 If it passes the test we return #t and move on to test the next item in the list.
Then if that returns #t, you will end up with (and #t #t #t) which evaluates to #t and gives you the result.
Therefore we are always getting closer and closer to termination by solving the nested inputs

Termination: The program terminates once we evaluate all statements in the and. This happens once we've
performed a check on all the eleents in the list. 



|#
