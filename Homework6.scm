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
        (define (atom? x)
                  (and (not (pair? x)) (not (null? x))))

        (define (count-occurences lst old)
                (cond ((null? lst) 0)
                      ((atom? lst) (if (equal? lst old) 1 0))
                      (else (+ (count-occurences (car lst) old) (count-occurences (cdr lst) old)))))


        (define (replace-aux lst n old new count found)
                  (let ((occ_in_car (count-occurences (cond ((atom? lst) lst)
                                                            ((null? lst) lst)
                                                            (else (car lst))) 
                                                            old)))

                  (cond ((null? lst) '()) 
                        ((and (atom? lst) (equal? lst old) (equal? count n) (not found)) new)
                        ((atom? lst) lst)

                        (else (cons 
                                    (replace-aux (car lst) 
                                                n 
                                                old 
                                                new 
                                                (if (equal? (car lst) old) (+ count 1) count) 
                                                (if (equal? n count) #t #f))

                                    (if (> n occ_in_car) 
                                          (replace-aux (cdr lst) 
                                                      (- n occ_in_car) 
                                                      old 
                                                      new 
                                                      0 
                                                      found) 
                                          (cdr lst)))))))

(replace-aux lst n old new 0 #f))

; Additional Problems

; Abelson and Sussman, Exercise 2.27 
; Abelson and Sussman, Exercise 2.29
; Abelson and Sussman, Exercise 2.32
; Abelson and Sussman, Exercise 2.37
; Abelson and Sussman, Exercise 2.41
; Abelson and Sussman, Exercise 2.42







