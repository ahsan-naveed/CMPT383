(load "env1.scm")
(load "err.scm")

(define is-number? (lambda (e) (number? e )))
(define is-var? (lambda (e) (symbol? e )))

;; returns true iff lst is a list of length n
;; Reference: http://www.sfu.ca/~tjd/383summer2019/scheme-intro-cont.html
(define nlist?
    (lambda (n lst)
        (and (list? lst)
             (= n (length lst))
        )
    )
)

;; (dec expr)
;; Reference: http://www.sfu.ca/~tjd/383summer2019/scheme-intro-cont.html
(define is-dec?
    (lambda (e)
        (and (nlist? 2 e)
             (equal? 'dec (car e))
        )
    )
)

;; (inc expr)
;; Reference: http://www.sfu.ca/~tjd/383summer2019/scheme-intro-cont.html
(define is-inc?
    (lambda (e)
        (and (nlist? 2 e)
             (equal? 'inc (car e))
        )
    )
)

;; (expr op expr)
;; Reference: http://www.sfu.ca/~tjd/383summer2019/scheme-intro-cont.html
(define is-bin-op?
    (lambda (e op)
        (and (nlist? 3 e)
             (equal? op (second e))
        )
    )
)

;; Reference: http://www.sfu.ca/~tjd/383summer2019/scheme-intro-cont.html
(define is-add? (lambda (e) (is-bin-op? e '+)))     ;; (expr + expr)
(define is-sub? (lambda (e) (is-bin-op? e '-)))     ;; (expr - expr)
(define is-mul? (lambda (e) (is-bin-op? e '*)))     ;; (expr * expr)
(define is-div? (lambda (e) (is-bin-op? e '/)))     ;; (expr / expr)
(define is-exp? (lambda (e) (is-bin-op? e '**)))    ;; (expr ** expr)

;; Returns true iff e is a valid propositional expression.
;; Reference: http://www.sfu.ca/~tjd/383summer2019/scheme-intro-cont.html
(define is-expr?
    (lambda (e)
        (cond
            ((is-number? e)
                #t)
            ((is-var? e)
                #t)
            ((is-dec? e)
                (is-expr? (second e)))
            ((is-inc? e)
                (is-expr? (second e)))
            ((is-add? e)
                (and
                    (is-expr? (first e))
                    (is-expr? (third e))))
            ((is-sub? e)
                (and
                    (is-expr? (first e))
                    (is-expr? (third e))))
            ((is-mul? e)
                (and
                    (is-expr? (first e))
                    (is-expr? (third e))))
            ((is-div? e)
                (and
                    (is-expr? (first e))
                    (is-expr? (third e))))
            ((is-exp? e)
                (and
                    (is-expr? (first e))
                    (is-expr? (third e))))
            (else
                #f)
        )
    )
)

;; Returns x^y i.e. (2, 3) = 8
(define nth-power
    (lambda (x y)
        (cond
            ((< y 0)
                (error "nth-power: negative exponent is not allowed"))
            ((or (= y 0) (= x 1))
                1)
            (else
                (* x (nth-power x (- y 1))))
        )
    )
)

;; Evaluates the infix expression expr in the environment env. expr can contain variables from the environment.
;; Note: values of symbols are extracted from their respective environments.
;; 
;; Reference: http://www.sfu.ca/~tjd/383summer2019/scheme-intro-cont.html
(define myeval
    (lambda (e env)
        (cond
            ((is-number? e)
                e)
            ((is-var? e)
                (apply-env env e))
            ((is-dec? e)
                (- (myeval (second e) env) 1))
            ((is-inc? e)
                (+ (myeval (second e) env) 1))
            ((is-add? e)
                (+
                    (myeval (first e) env)
                    (myeval (third e) env)))
            ((is-sub? e)
                (-
                    (myeval (first e) env)
                    (myeval (third e) env)))
            ((is-mul? e)
                (*
                    (myeval (first e) env)
                    (myeval (third e) env)))
            ((is-div? e)
                (/
                    (myeval (first e) env)
                    (myeval (third e) env)))
            ((is-exp? e)
                (nth-power
                    (myeval (first e) env)
                    (myeval (third e) env)))
            (else
                (display-error
                    "myeval: "
                    "invalid expression "
                    e)
            )
        )
    )
)

;; Test environments
;; Reference: http://www.sfu.ca/~tjd/383summer2019/a2.html
(define env1
    (extend-env 'x -1
        (extend-env 'y 4
            (extend-env 'x 1
                (make-empty-env))))
)

(define env2
    (extend-env 'm -1
        (extend-env 'a 4
            (make-empty-env)))
)

(define env3
    (extend-env 'q -1
        (extend-env 'r 4
            (make-empty-env)))
)
