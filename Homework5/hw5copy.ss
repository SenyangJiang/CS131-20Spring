#lang racket
(provide (all-defined-out))

(define (lambda? x)
  (member x '(lambda λ)))

(define (quote? x)
  (equal? 'quote x))

(define (macro? x)
  (member x '(if lambda λ quote)))

; map a single variable x according to dict
(define (map-var x dict)
  (if (dict-ref dict x #f) (dict-ref dict x) x))

; map all vars except "lambda at start of list"
; then call compare-expr-helper
; "store" mapped elements in mappedx and mappedy
; x and y are modified
(define (map-var-list x y dictx dicty head?)
  (cond
    [(and (equal? x '()) (equal? y '()))
     '()]
    ; if (car x) and (car y) are not lists
    ; then map car x and car y
    [(and (not (list? x)) (not (list? y)))
     (expr-compare-helper (map-var x dictx) (map-var y dicty) #t)]
    ; if the head of a list is a lambda, then we need to handle the lambda expression recursively
    [(and head? (lambda? (car x)) (lambda? (car y)))
     (combine-lambda-expr x y dictx dicty)]
    ; if the list start with quote, don't combine the lists
    [(or (quote? (car x)) (quote? (car y)))
     (list 'if '% x y)]
    ; otherwise split two lists into car and cdr, map recursively
    [#t (cons (map-var-list (car x) (car y) dictx dicty #t) (map-var-list (cdr x) (cdr y) dictx dicty #f))]))

(define (combine-arg x y)
  (cond
    [(equal? x y) x]
    [(equal? (car x) (car y))
     (cons (car x) (combine-arg (cdr x) (cdr y)))]
    [#t (cons (string->symbol (string-append (symbol->string (car x)) "!" (symbol->string (car y))))
              (combine-arg (cdr x) (cdr y)))]))

(define (update-dict-statement x y dictx dicty)
  ; first we create a entry for the bounded variable in the dictionary
  ; and then call map-var-list, which maps lists according to dictionary
  (cond
    [(and (empty? (car x)) (empty? (car y)))
     (map-var-list (cadr x) (cadr y) dictx dicty #t)]
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
      (list (if (or (equal? 'λ (car x)) (equal? 'λ (car y))) 'λ 'lambda)
            (combine-arg (cadr x) (cadr y))
            (update-dict-statement (cdr x) (cdr y) dictx dicty))
      (list 'if '% x y)))

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

(define (expr-compare x y) (expr-compare-helper x y #t))