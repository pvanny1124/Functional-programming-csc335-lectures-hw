; Sixth Homework Set
; CSc 335
; Fall 2018

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; First Problem

; replace-nth

; Here is a tree recursion somewhat more complicated than those we have looked at until now


; develop and certify a scheme program replace-nth which takes as input

;        a list lst, not necessarily a list of atoms
;        a positive integer, n
;        an atom, old
;        an atom, new

; (replace-nth lst n old new) should replace the nth occurrence of old in 
; lst by new (and leave everything else unchanged)

(define (replace-nth lst n old new)
        (define (count-occurences lst old)
                (cond ((null? lst) 0)
                      ((atom? lst) (if (equal? lst old) 1 0))
                      (else (+ (count-occurences (car lst) old) (count-occurences (cdr lst) old)))))
)

; Additional Problems

; Abelson and Sussman, Exercise 2.27 
; Abelson and Sussman, Exercise 2.29
; Abelson and Sussman, Exercise 2.32
; Abelson and Sussman, Exercise 2.37
; Abelson and Sussman, Exercise 2.41
; Abelson and Sussman, Exercise 2.42







