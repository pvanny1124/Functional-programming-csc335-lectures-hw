
; Fourth Homework Set
; CSc 335
; Fall 2018


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Homework4.scm

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Here are some homework problems to get you started with lists

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Note that I have sometimes deliberately offered incomplete specifications - if you find this
; to be the case, you will need to complete the specification as you deem best.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Give both recursive and iterative procedures (along with their arguments) for each

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


; 1.  Write your own version of length using the list functions we have discussed.  You can find
; length documented at http://www.schemers.org/Documents/Standards/R5RS/

; The idea is suggested by (my-length '(a b c d)) = 4.  

;recursively
(define (my-length lst)
    (cond ((null? lst) 0)
            (else (+ 1 (my-length (cdr lst))))))

;iteratively
(define (my-length lst)
        (define (iter lst count)
            (cond ((null? lst) count)
                    (else (iter (cdr lst) (+ count 1)))))
        (iter lst 0))


; 2.  Write your own version of list-ref using the list functions we have discussed.  You can find
; list-ref documented at http://www.schemers.org/Documents/Standards/R5RS/

; Briefly, the idea is indicated by this example:  (my-list-ref '(a b c d) 2) = c.  Note the 0-based
; indexing.  What happens if the input index exceeds the size of the input list?

;iteratively
(define (my-list-ref lst num)
        (cond ((zero? num) (car lst))
                (else (my-list-ref (cdr lst) (- num 1)))))

;recursively
(define (my-list-ref lst num)
        (cond ((zero? num) (car lst))
                (else (car (cons (my-list-ref (cdr lst) (- num 1)) '())))))


; 3. Write a function start that takes two arguments, lst and num, and which returns the
; first num elements of lst.

;recursively
(define (start lst num)
        (cond ((zero? num) '())
                (else (cons (car lst) (start (cdr lst) (- num 1))))))

;iteratively
(define (start lst num)
        (define (iter lst num result)
                (cond ((equal? num 1) result)
                      (else (iter (cdr lst) (- num 1) (append result (cons (car lst) '()))))
                )
        )
        (iter (cdr lst) num (cons (car lst) '())))

; 4.  Write a function but-last that takes two arguments, lst and num, and which returns the
; list of all but the last num elements of lst.


; 5.  Write a function end that takes two arguments, lst and num, and returns the last num
; elements of lst.

;iteratively
(define (end lst num)   
        (cond ((equal? (length lst) num) lst)
                (else (end (cdr lst) num))))
                
;recursively
(define (end lst num)
        (cond ((equal? (length lst) num) lst)
                (else (cons (car (end (cdr lst) num)) (cdr (end (cdr lst) num)))))
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


; suggested reading:

;;    http://en.wikipedia.org/wiki/Scheme_(programming_language)

