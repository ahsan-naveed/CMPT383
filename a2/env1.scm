;; This environemnt is implemented as a list of lists where each inner list consists of 2 elements.
;; First element is the variable and the second element is the value of this variable. 

;; Returns if v is defined in the env.
;; Pre-condition: env is of form ((a 1) (b 2) ...)
(define member?
    (lambda (v env)
        (cond
            ((null? env)
                #f)
            ((equal? (car (car env)) v)               ;; base case:
                 #t)                                  ;; Checks first element of nested list
            (else
                (member? v (cdr env)))                ;; recursive case
        )
    )
)

;; Returns value of v from the given env.
;; Pre-condition: env is of form ((a 1) (b 2) ...)
(define get_v
    (lambda (v env)
        (cond
            ((equal? (car (car env)) v)               ;; base case:
                (car (cdr (car env))))                ;; Checks first element of the nested list.
            (else                   
                (get_v v (cdr env)))                  ;; recursive case
        )
    )
)


;; Sets value of v in the given pair if v is not in pair then return the pair.
;; Input: a list of two elements i.e. (var value)
;; Output: a list of two elements i.e (var new-value)
(define set_v
    (lambda (v val)
        (lambda (pair)
            (cond
                ((equal? v (car pair))
                    (cons (car pair) (list val)))
            (else
                pair)
            )
        )
    )
)

;; Updates the given pair of the given env.
(define update_env
    (lambda (v val env)
        (map (set_v v val) env)
    )
)

;; Returns a new empty environment.
(define make-empty-env
    (lambda ()
        '()
    )
)

;; Returns the value of variable v in environment env.
(define apply-env
    (lambda (env v)
        (cond
            ((member? v env)
                (get_v v env))
            (else
                (display-error
                    "apply-env: "
                    "unknown variable"
                    v
                )
            )
        )
    )
)

;; Returns a new environment that is the same as env except that the value of v in it is val.
(define extend-env
    (lambda (v val env)
        (cond
            ((member? v env)                          ;; if var is already in env then just update its value.
                (update_env v val env))
            (else                                     ;; otherwise add the new (var val) in the environment.
                (cons (cons v (list val)) env))
        )
    )
)

;; Test environment
(define test-env
    (extend-env 'a 1
        (extend-env 'b 2
            (extend-env 'c 3
                (extend-env 'b 4
                    (make-empty-env)))))
)