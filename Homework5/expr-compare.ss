#lang racket
(provide (all-defined-out))

(define (lambda? x)
  (member x '(lambda λ)))

(define (quote? x)
  (equal? 'quote x))

(define (macro? x)
  (member x '(if lambda λ quote)))

; map x according to dict
(define (map-var x dict)
  (cond
    [(not (list? x))
     (if (dict-ref dict x #f) (dict-ref dict x) x)]
    [(empty? x)
     '()]
    [#t (cons (map-var (car x) dict) (map-var (cdr x) dict))]))
  

; map each element in a two lists and compare each element
(define (map-var-list-helper x y dictx dicty head?)
  (cond
    [(and (equal? x '()) (equal? y '()))
     '()]
    ; if x and y are not lists, map and compare them directly
    [(and (not (list? x)) (not (list? y)))
     (expr-compare-helper (map-var x dictx) (map-var y dicty) #t)]
    ; if length of x and y are different, then use if directly
    [(or (or (not (list? x)) (not (list? y)))
      (not (equal? (length x) (length y))))
     (list 'if '% (map-var x dictx) (map-var y dicty))
     ]
    ; if the head of a list is a lambda, then we need to handle the lambda expression recursively
    [(and head? (lambda? (car x)) (lambda? (car y)))
     (combine-lambda-expr x y dictx dicty)]
    ; if the list start with quote, don't map the lists
    [(and head? (or (quote? (car x)) (quote? (car y))))
     (list 'if '% x y)]
    ; if one is macro and the other is not, then use if directly
    [(and head? (or (and (macro? (car x)) (not (macro? (car y))))
                    (and (macro? (car y)) (not (macro? (car x))))))
     (list 'if '% (map-var x dictx) (map-var y dicty))]
    ; otherwise split two lists into car and cdr, map recursively
    [#t (cons (map-var-list-helper (car x) (car y) dictx dicty #t) (map-var-list-helper (cdr x) (cdr y) dictx dicty #f))]))

(define (map-var-list x y dictx dicty)
  (cond
    [(and (empty? x) (empty? y))
     '()]
    [#t (cons (map-var-list-helper (car x) (car y) dictx dicty #t)
              (map-var-list (cdr x) (cdr y) dictx dicty))]))

; combine arguments of lambda function (without setting the dictionary)
(define (combine-arg x y)
  (cond
    [(equal? x y) x]
    [(equal? (car x) (car y))
     (cons (car x) (combine-arg (cdr x) (cdr y)))]
    [#t (cons (string->symbol (string-append (symbol->string (car x)) "!" (symbol->string (car y))))
              (combine-arg (cdr x) (cdr y)))]))

; first we create entries for the bounded variables in the dictionary
; and then call map-var-list, which maps lists according to dictionary
(define (update-dict-statement x y dictx dicty)
  (cond
    [(and (empty? (car x)) (empty? (car y)))
     (map-var-list (cdr x) (cdr y) dictx dicty)]
    [(equal? (caar x) (caar y))
     (update-dict-statement (cons (cdar x) (cdr x)) (cons (cdar y) (cdr y))
                            (dict-set dictx (caar x) (caar y))
                            (dict-set dicty (caar y) (caar y)))]
    [#t (update-dict-statement (cons (cdar x) (cdr x)) (cons (cdar y) (cdr y))
                               (dict-set dictx (caar x) (string->symbol (string-append (symbol->string (caar x)) "!" (symbol->string (caar y)))))
                               (dict-set dicty (caar y) (string->symbol (string-append (symbol->string (caar x)) "!" (symbol->string (caar y))))))]
    ))

(define (combine-lambda-expr x y dictx dicty)
  ; if different number of arguments, then don't combine
  (if (equal? (length (cadr x)) (length (cadr y)))
      (append (list (if (or (equal? 'λ (car x)) (equal? 'λ (car y))) 'λ 'lambda)
            (combine-arg (cadr x) (cadr y)))
            (update-dict-statement (cdr x) (cdr y) dictx dicty))
      (list 'if '% x y)))

; helper function
(define (expr-compare-helper x y head?)
  (cond
    ; handled all non-list cases
    [(equal? x y) x]
    [(and (boolean? x) (boolean? y))
     (if x '% '(not %))]
    [(or (not (list? x)) (not (list? y)))
     (list 'if '% x y)]
    ; handles all list cases
    [(not (equal? (length x) (length y)))
     (list 'if '% x y)]
    [(and head? (or (quote? (car x)) (quote? (car y))))
     (list 'if '% x y)]
    [(and head? (or (and (macro? (car x)) (not (macro? (car y))))
                    (and (macro? (car y)) (not (macro? (car x))))))
     (list 'if '% x y)]
    [(and head? (lambda? (car x)) (lambda? (car y)))
     (combine-lambda-expr x y '() '())]
    [#t (cons (expr-compare-helper (car x) (car y) #t) (expr-compare-helper (cdr x) (cdr y) #f))]))

; main function
(define (expr-compare x y) (expr-compare-helper x y #t))

(define (test-expr-compare x y)
  (and (equal? (eval x)
               (eval `(let ((% #t)) ,(expr-compare x y))))
       (equal? (eval y)
               (eval `(let ((% #f)) ,(expr-compare x y))))))

(define test-expr-x '(cons 12 ((lambda (f a) (list (f a) '(f a))) (lambda (a) (if a 1 0)) #f)))
(define test-expr-y '(cons 4 ((lambda (g c) (list (g c) '(g c))) (λ (a) (if a 1 0)) #t)))