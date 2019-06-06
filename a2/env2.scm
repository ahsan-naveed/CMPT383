;; This environment is implemented as a associative lists a main list contains two lists. First list is reponsible
;; for storing variables the second list stores values of all the variables. The in which variables are stored 
;; in first list is same as the order in which thier values are stored in the second list.

;; Example: ((a b ...) (1 2 ...)), here a = 1 & b = 2

(load "err.scm")

;;
;; Returns the item at index location i in lst.
;;
(define nth
    (lambda (lst i)
       (cond
            ((= i 0)                                            ;; base case:
                (car lst))                                      ;; If i is zero return the first element of the lst.
            (else
                (nth (cdr lst) (- i 1)))                        ;; recursive case:
       )                                                        ;; Recursion forces the desired element to come to the
    )                                                           ;; head of the lst so we can extract it using car.
)


;; Returns index of v in lst.
(define index-of
    (lambda (v env)
        (define helper
            (lambda (v env index)
                (cond
                    ((null? env)                                ;; boundary check:
                        -1)                                     ;; Return -1 if element is not found.
                    ((equal? (car env) v)                       ;; base case:
                        index)                                  ;; Return current index value if element is found.
                    (else                                       ;; Recursive case:
                        (helper v (cdr env) (+ 1 index)))       ;; Invoke function with new index as index + 1
                )
            )
        )
        (helper v env 0)                                        ;; Starts the function with index 0
    )
)

;;
;; Returns true if x is in lst, and false otherwise.
;; Pre-condition: env is of the form (a b c ...) i.e. only contains variables.
(define member?
    (lambda (v env)
        (cond
            ((null? env)
                #f)
            ((equal? v (car env))
                #t)
            (else
                (member? v (cdr env)))
        )
    )
)

;; Updates values at index i of an environment.
;; Pre-condition: env is only the list of values of variables i.e. (1 2 ...)
(define update-at
    (lambda (i val lst)
        (let (
            (head (car lst))
            (body (cdr lst))
        )
            (cond
                ((= i 0)                                            ;; base case:
                    (cons val body))                                ;; If i is zero update the first element of the lst.
                (else
                    (cons                                           ;; recursive case:
                        head                                        ;; Cons head of the list with the updated body.
                        (update-at (- i 1) val body)))        
            )
        ) 
    )
)

;;
;; Returns a new empty environment.
;;
(define make-empty-env
    (lambda ()
        (list () ())
    )
)


;; Returns the value of variable v in environment env.
(define apply-env
    (lambda (env v)
        (let (
            (vars (car env))
            (vals (car (cdr env)))
        )
            (cond
                ((member? v vars)                                   ;; if var exists in the environment
                    (nth                                            ;; then simply return its value from the second 
                        vals                                        ;; in our environment
                        (index-of v vars)))
                (else
                    (display-error                                  ;; var is not defined in the environment.
                        "apply-env: "
                        "unknown variable"
                        v
                    )
                )
            )
        )
    )
)

;;
;; Returns a new environment that is the same as env except that the value of v in it is val.
;;
(define extend-env
    (lambda (v val env)
        (let (
            (vars (car env))
            (vals (car (cdr env)))
        )
            (cond
                ((member? v vars)                                   ;; if var exists in the environment then simply
                    (list                                           ;; update its value with the new value.
                        vars
                        (update-at
                            (index-of v vars)
                            val
                            vals))
                    )
                (else                                               ;; otherwise add a new var in the var list and new
                    (list                                           ;; value in the val list of the environment.
                        (cons v vars)
                        (cons val vals)))
            )
        )
    )
)

(define test-env
    (extend-env 'a 1
        (extend-env 'b 2
            (extend-env 'c 3
                (extend-env 'b 4
                    (make-empty-env)))))
)