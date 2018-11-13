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

; Exercise 2.27. Modify your reverse procedure of exercise 2.18 to produce a deep-reverse
; procedure that takes a list as argument and returns as its value the list with its elements reversed and
; with all sublists deep-reversed as well. For example,


(define (deep-reverse tree)
            (cond ((null? tree) '())
                  ((atom? tree) tree)
                  (else (append (deep-reverse (cdr tree))
                               (list (deep-reverse (car tree)))))))

;using map
(define (deep-reverse tree)
            (cond ((null? tree) '())
                  ((atom? tree) tree)
                  (else (reverse (map deep-reverse tree)))))

; Abelson and Sussman, Exercise 2.29


; Abelson and Sussman, Exercise 2.32

; We can represent a set as a list of distinct elements, and we can represent the set of all
; subsets of the set as a list of lists. For example, if the set is (1 2 3), then the set of all subsets is
; (() (3) (2) (2 3) (1) (1 3) (1 2) (1 2 3)). Complete the following definition of a
; procedure that generates the set of subsets of a set and give a clear explanation of why it works:\

(define (subsets s)
        (if (null? s)
            (list '()) 
             (let ((rest (subsets (cdr s))))
                 (append rest (map (lambda (seq) (append (list (car s)) seq)) rest)))))

;in every step of the cdr, we are saving the original value of say, (3) as s before () is returned,
;(2), before (3) is returned and etc from the recursive call to subsets cdr s.
; once the call is returned, we can simply append the car of s. say (3)
; to '().
;in the next step, we would append (2) or the car of (2 3) to (() (3)) to result in ((2) (2 3))
;which is then appended to (() (3)) to produce (() (3) (2) (2 3))
; then the program would continue to the original S (1 2 3) and append (1) to each item in (() (3) (2) (2 3))
; resulting in ((1) (1 3) (1 2) (1 2 3))
; which is appended to (() (3) (2) (2 3)) resulting in (() (3) (2) (2 3) (1) (1 3) (1 2) (1 2 3))

; Abelson and Sussman, Exercise 2.37

(define (dot-product v w)
 (accumulate + 0 (map * v w)))

(define (matrix-*-vector m v)
 (map <??> m))

(define (transpose mat)
 (accumulate-n <??> <??> mat))

(define (matrix-*-matrix m n)
 (let ((cols (transpose n)))
 (map <??> m)))

; Abelson and Sussman, Exercise 2.41
; Abelson and Sussman, Exercise 2.42







