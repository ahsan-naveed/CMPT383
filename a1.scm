;; a1.scm
;; Name: Ahsan Naveed
;; Course: CMPT 383 Comparitive Programming Languages
;; Student number: 301228556

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;; Question 1 ;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;
;; Returns #t if a lst has exactly 1 element.
;;
(define singleton?
    (lambda (lst)
        (cond
            ((and
                (list? lst)
                (null? (cdr lst)))  
                #t)                                             ;; return #t if lst is list and cdr of lst is null.
            (else
                #f)
        )
    )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;; Question 2 ;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;
;; Returns a list containing n copies of x.
;;
(define my-make-list
    (lambda (n x)
        (cond
            ((equal? n 0)                                       ;; base case:
                '())                                            ;; An empty list is returned when n is 0.
            (else
                (cons x (my-make-list (- n 1) x)))              ;; recursive case:
        )                                                       ;; Else cons x and call my-make-list with with n-1 and x.
    )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;; Question 3 ;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;
;; Returns #t if lst is empty, or if all the elements in it are equal to each other.
;;
(define all-same?
    (lambda (lst)
        (cond
            ((null? lst)                                        ;; base case 1:
                #t)                                             ;; #t if lst is empty.
            ((= (my-length lst) 1)                              ;; base case 2:
                #t)                                             ;; #t if lst is singleton.          
            ((not (equal? (car lst) (car (cdr lst))))           ;; base case 3:
                #f)                                             ;; #f if first two elements are not equal.
            (else
                (all-same? (cdr lst)))                          ;; recursive case:
        )                                                       ;; Else all-same is called recursively with cdr of lst
    )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;; Question 4 ;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;
;; Returns a list containing the numbers from 0 to n-1.
;;
(define my-iota
    (lambda (n)
        (cond
            ((equal? n 0)                                       ;; base case:
                '())                                            ;; Empty lst is returned when n is zero.
            (else
               (my-sort (cons (- n 1) (my-iota (- n 1)))))      ;; recursive case:
        )                                                       ;; Return numbers from n-1 to 0 which are then sorted
    )                                                           ;; to return numbers from 0 to n-1.
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;; Question 5 ;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; References:
;; Discussed during Lecture.
;;
;; Returns the number of items in lst.
;;
(define my-length
    (lambda (lst)
        (cond
            ((null? lst)                                        ;; base case:
                0)                                              ;; Returns 0 for empty lst.
            (else (+ 1 (my-length (cdr lst))))                  ;; recursive case:
        )                                                       ;; Add 1 and call the function for the rest of the lst.
    )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;; Question 6 ;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;
;; Returns the item at index location i in lst.
;;
(define nth
    (lambda (lst i)
       (cond
            ((or (< i 0) (>= i (my-length lst)))                ;; Results in error if the index i is out of range or < 0
                (error "bad index"))
            ((= i 0)                                            ;; base case:
                (car lst))                                      ;; If i is zero return the first element of the lst.
            (else
                (nth (cdr lst) (- i 1)))                        ;; recursive case:
       )                                                        ;; Recursion forces the desired element to come to the
    )                                                           ;; head of the lst so we can extract it using car.
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;; Question 7 ;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;
;; Returns the last element of lst.
;;
(define my-last
    (lambda (lst)
        (cond
            ((null? lst)
                (error "my-last: empty list"))                  ;; Results in error when lst is null
            ((singleton? lst)                                   ;; base case:
                (car lst))                                      ;; Returns first element when we have a singelton lst.
            (else
                (my-last (cdr lst)))                            ;; recursive case:
        )                                                       ;; Else function is called recursively with the cdr of lst.
    )                                                           ;; Recursion continues until we have only one element left
)                                                               ;; and we return that.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;; Question 8 ;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;
;; Returns a list with first element and the last element removed.
;;
(define middle
    (lambda (lst)
        (cond
            ((<= (my-length lst) 2)                             ;; error check:
                '())                                            ;; lsts with 2 elements have no middle.
            ((= (my-length lst) 3)                              ;; base case:
                (cons (car (cdr lst)) '()))                     ;; Returns second when lst has 3 elements.
            (else
                (cons (car (cdr lst)) (middle (cdr lst))))      ;; recursive case:
        )                                                       ;; Else cons the second element of the lst and
    )                                                           ;; call function with the cdr of the lst.
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;; Question 9 ;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; References:
;; http://www.sfu.ca/~tjd/383summer2019/scheme-intro.html
;;
;; Returns a list containing just the elements of lst that satisfied the predicate function.
;; 
(define my-filter
    (lambda (pred? lst)
        (cond
            ((null? lst)                                        ;; base case:
                '())                                            ;; Return '() if lst is null. 
            ((pred? (car lst))
                (cons (car lst) (my-filter pred? (cdr lst))))   ;; recursive case:
            (else                                               ;; If car satisfies the pred add it to the resulting
                (my-filter pred? (cdr lst))                     ;; lst, otherwise call the function on the cdr of the
            )                                                   ;; without cons'ing it.
        )
    )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;; Question 10 ;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; References:
;; http://www.sfu.ca/~tjd/383summer2019/scheme-intro.html
;;
;; my-append helper: folds  a list.
;;
(define my-fold
    (lambda (f empty-value lst)
        (cond
            ((null? lst)                                        ;; base case:
                empty-value)                                    ;; Return empty-value if lst is null.
            (else 
                (f (car lst) (my-fold f empty-value (cdr lst))) ;; recursive case
            )                                                   ;; Applies f in car of lst and call my-fold ont the cdr of lst.
        )
    )
)

;;
;; Returns a list that has all the elements of A followed by all the elements of B.
;;
(define my-append
    (lambda (A B)
        (my-fold cons B A)                                      ;; B is empty-valiue.
    )                                                           ;; A is lst.
)                                                               ;; cons is f of my-fold.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;; Question 11 ;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; References:
;; http://www.sfu.ca/~tjd/383summer2019/scheme-intro.html
;;
;; Returns a list that has all the lists of lol appended into one list.
;;
(define append-all
    (lambda (lst)
        (cond
            ((null? lst)                                        ;; base case 1
                lst)                                            ;; Returns lst if lst is null.
            ((not (list? lst))                                  ;; base case 2
                lst)                                            ;; Returs lst if it is not a list.
            ((list? (car lst))                                  ;; recursive case 1
                (my-append                                      ;; append first half and second half of the lst. 
                    (append-all (car lst))
                    (append-all (cdr lst))))
            (else                                               ;; recursive case 2
                (cons (car lst) (append-all (cdr lst))))        ;; since car is not a list append it to the remaining lst.
        )
    )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;; Question 12 ;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;
;; my-sort helper: Returns numbers of a lst at even position (1-based index).
;;
(define even-position-elems
    (lambda (lst)
        (cond
            ((null? lst)                                        ;; base case 1:
                '())                                            ;; Returns empty list for null lst.
            ((singleton? lst)                                   ;; base case 2:
                '())                                            ;; Return empty list for singleton lst.
            (else                                               
                (cons                                           ;; recursive case:
                    (car (cdr lst))                             ;; cons second element of the lst then
                    (even-position-elems (cdr (cdr lst)))))     ;; skip first two elements of the lst and
        )                                                       ;; call my-evens on it.
    )
)

;;
;; my-sort helper: Returns numbers of a lst at odd position (1-based index).
;;
(define odd-position-elems
    (lambda (lst)
        (cond
            ((null? lst)                                        ;; base case 1:
                '())                                            ;; Returns empty list for null lst.
            ((singleton? lst)                                   ;; base case 2:
                lst)                                            ;; Return empty list for singleton lst.
            (else                                               
                (cons                                           ;; recursive case:
                    (car lst)                                   ;; cons first element of the lst then 
                    (odd-position-elems (cdr (cdr lst)))))      ;; jump over first two elements of the lst and
        )                                                       ;; call my-odds on it.
    )
)

;; References:
;; https://stackoverflow.com/questions/20506666/scheme-and-merge-sort
;;
;; my-sort helper: Merges two sorted lsts, each lst is in increasing order.
;;
(define odd-even-merge
    (lambda (A B)
        (cond
            ((null? A)                                          ;; base case 1:
                B)                                              ;; If A is empty return B.
            ((null? B)                                          ;; base case 2:
                A)                                              ;; Vice versa.
            ((if (< (car A) (car B))                            ;; recursive case:
                (cons (car A) (odd-even-merge (cdr A) B))       ;; cons smaller of the first element of both lsts
                (cons (car B) (odd-even-merge A (cdr B)))))     ;; and call the func with updated lsts i.e. don't
        )                                                       ;; include the element that has been cons'ed 
    )
)

;; References:
;; https://stackoverflow.com/questions/20506666/scheme-and-merge-sort
;;
;; Returns the numbers on lst in sorted order.
;;
(define my-sort
    (lambda (lst)
        (cond
            ((null? lst)                                        ;; base case 1:
                '())                                            ;; Return '() if lst is null.
            ((singleton? lst)                                   ;; base case 2:
                lst)                                            ;; Return lst if lst is singleton.
            (else
                (odd-even-merge                                 ;; recursive case:
                    (my-sort (even-position-elems lst))         ;; Recursively sort both halves and merge the soreted halves.
                    (my-sort (odd-position-elems lst))
                )
            )
        )
    )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;; Question 13 ;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; References:
;; https://www.geeksforgeeks.org/given-a-number-n-generate-bit-patterns-from-0-to-2n-1-so-that-successive-patterns-differ-by-one-bit/?fbclid=IwAR0J_RtULCBJ6VE8XtLU8f_6t7RiLs0yQ5mJSbVPmkWx0_YqZHeMERHuT0s
;;
;; Returns a list of 2^n sub-lists, where each sub-list is a different pattern of n 0s and 1s. 
;;
(define all-bits
    (lambda (n) 
        (cond
            ((<= n 0)                                           ;; base case 1:    
                '())                                            ;; return () when n <= 0.
            ((= n 1)                                            ;; base case 2:
                '((0) (1)))                                     ;; return ((0) (1)) when n is 1.   
            (else
                (my-append                                      ;; recursive case:
                    (prefix 0 (all-bits (- n 1)))               ;; prefix 0 on the result of (all-bits (- n 1)) 
                    (prefix 1 (rvrs (all-bits (- n 1))))))      ;; prefix 1 on the reverse of (all-bits (- n 1)) 
        )                                                       ;; append both lists.
    )
)

;;
;; all-bits helper: reverses a lst.
;;
(define rvrs
    (lambda (lst)   
        (cond
            ((null? lst)                                        ;; base case:        
                '())                                            ;; Returns empty list when lst is null.
            (else
                (my-append                                      ;; recursive case:
                    (rvrs (cdr lst))                            ;; Appends reverse of cdr of lst on to the
                    (list (car lst)))                           ;; the list of first element of lst.
            )
        )
    )
)

;;
;; all-bits helper: Prepends x to all the lst elements (assumes that lst is always a 2d lst).
;; 
(define prefix
    (lambda (x lst)
        (cond   
            ((null? lst)                                        ;; base case:
                '())                                            ;; Returns '() when lst is null.
            (else
                (cons (cons x (car lst)) (prefix x (cdr lst)))) ;; recursive case:
        )                                                       ;; cons x on to each element recursively starting
    )                                                           ;; from the head of the lst.
)