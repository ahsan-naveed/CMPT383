(load "myeval.scm")
(load "err.scm")

; To simplify an expression, repeatedly apply the following rules to expr wherever possible (e is any valid expression):

; (0 + e) simplifies to e
; (e + 0) simplifies to e
; (0 * e) simplifies to 0
; (e * 0) simplifies to 0
; (1 * e) simplifies to e
; (e * 1) simplifies to e
; (e / 1) simplifies to e
; (e - 0) simplifies to e
; (e - e) simplifies to 0
; (e ** 0) simplifies to 1
; (e ** 1) simplifies to e
; (1 ** e) simplifies to 1
; if n is a literal number, then (inc n) simplifies to the value of n + 1
; if n is a literal number, then (dec n) simplifies to the value of n - 1

; Returns, if possible, a simplified version of expr.

; Reference: http://www.sfu.ca/~tjd/383summer2019/scheme-intro-cont.html

(define simplify-adds
    (lambda (l r)
        (cond 
            ((equal? 0 l)                               ;; 0 + e = e    
                r)
            ((equal? 0 r)                               ;; e + 0 = e
                l)
            (else
                (list l '+ r))
        )
    )
)

(define simplify-muls
    (lambda (l r)
        (cond 
            ((equal? 1 l)                               ;; 1 * e = e
                r)
            ((equal? 1 r)                               ;; e * 1 = e
                l)
            ((or (equal? l 0) (equal? r 0))             ;; e * 0 = 0 * e = 0
                0)
            (else
                (list l '* r))
        )
    )
)

(define simplify-divs
    (lambda (l r)
        (cond 
            ((equal? 1 r)                               ;; e / 1 = e
                l)
            (else
                (list l '/ r))
        )
    )
)

(define simplify-subs
    (lambda (l r)
        (cond
            ((equal? l r)                               ;; e - e = 0
                0)
            ((equal? 0 r)                               ;; e - 0 = e
                l)
            (else
                (list l '- r))
        )
    )
)

(define simplify-exps
    (lambda (l r)
        (cond
            ((equal? 0 r)                               ;; e ** 0 = 1
                1)
            ((equal? 1 r)                               ;; e ** 1 = e
                l)
            ((equal? 1 l)                               ;; 1 ** e = 1
                1)
            (else
                (list l '** r))
        )
    )
)

(define simplify-incs
    (lambda (operand)
        (cond
            ((is-number? operand)                       ;; n + 1
                (+ operand 1))
            (else
                (list 'inc operand))
        )
    )
)

(define simplify-decs
    (lambda (operand)
        (cond
            ((is-number? operand)                       ;; n - 1
                (- operand 1))
            (else
                (list 'dec operand))
        )
    )
)

(define simplify
    (lambda (e)
        (cond
            ((or (is-number? e) (is-var? e))
                e)
            ((is-add? e)
                (simplify-adds
                    (simplify (first e))
                    (simplify (third e)))
			)
            ((is-mul? e)                                
                (simplify-muls
                    (simplify (first e))
                    (simplify (third e)))	
			)
            ((is-div? e)                                
                (simplify-divs
                    (simplify (first e))
                    (simplify (third e)))
			)
            ((is-sub? e)                                			
                (simplify-subs 
                    (simplify (first e))
                    (simplify (third e)))
			)
            ((is-exp? e)                               
                (simplify-exps
                    (simplify (first e))
                    (simplify (third e)))
			)
            ((is-inc? e)                                
                (simplify-incs
                    (simplify (second e)))
			)
            ((is-dec? e)                                
                (simplify-decs
                    (simplify (second e)))
			)
            (else
                (display-error
                    "simplify: "
                    "invalid expression "
                    e)
            )        
        )
    )
)