;; Error function
;; Displays error message with a helpful description.
;;
;; Reference: https://www.gnu.org/software/mit-scheme/documentation/mit-scheme-ref/Error-Messages.html
(define display-error
    (lambda (func message irritant)
        (error
            (error-irritant/noise func)
            (error-irritant/noise message)
            (error-irritant/noise irritant)
        )
    )
)