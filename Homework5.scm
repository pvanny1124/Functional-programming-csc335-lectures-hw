

; Fifth Homework Set
; CSc 335
; Fall 2018


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Homework Problems

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




; 1.  Prove the correctness of the value function presented in lecture9.scm.  Your argument should be an
; induction on (the complexity of) the argument.  State clearly what assumptions you make.

(define value
    (lambda (aexp)
      (cond ((atom? aexp) aexp)
            (else ((atom-to-function (operator aexp))
                   (value (first-operand aexp))
                   (value (second-operand aexp)))))))

;induction
;1. basis-step: if aexp is an atom, the program returns the aexp.
;2. induction-hypothesis: assume that (value (first-operand aexp)) returns the (car) of the aexp
;       and that (value (second-operand aexp)) returns the (cdr (cdr)) of the aexp
;3. induction-step: (value (first-operand)) becomes (value (car aexp)) 
;                   (value (second-operand)) becomes (value (cdr (cdr aexp)))
;if either the first or second operand are themselves a-exps, then (atom-to-function (operator aexp))
;   will extract the function and perform the procedure to (value (car aexp)) and (value (cdr (cdr aexp)))




; 2.  Make all changes needed to allow the a-iexp calculator to work with numbers given in base 1.
; Explain carefully how you will represent such numbers; design plus, times, and exponent procedures
; for them.  Prove correctness of your functions.


(define operator
    (lambda (i-aexp) 
      (car (cdr i-aexp))))
  
  (define first-operand
    (lambda (i-aexp)
      (car i-aexp)))
  
  (define second-operand
    (lambda (i-aexp)
      (car (cdr (cdr i-aexp)))))


(define plus-aexp?
        (lambda (aexp)
          (eq? (operator aexp) '+)))
      
(define times-aexp?
        (lambda (aexp)
          (eq? (operator aexp) '*)))
      
(define power-aexp?
        (lambda (aexp)
          (eq? (operator aexp) '^)))


(define plus (lambda (m n) (append m n)))
          
(define times (lambda (m n) 
                (cond ((null? n) '())
                        (else (append m (times m (cdr n)))))))
          
(define expon
            (lambda (base exponent)
              (cond ((zero? exponent) 1)
                    (else (times base (expon base (sub1 exponent)))))))
          
      
; 3.  Exercises 2.2, 2.3 and 2.4 in Abelson and Sussman.  Proofs are not necessary.



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

