

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
      
(define plus (lambda (m n) (append m n)))
          
(define times (lambda (m n) 
                (cond ((null? n) '())
                        (else (append m (times m (cdr n)))))))
          
(define expon
            (lambda (base exponent)
              (times base exponent)))
          
      
; 3.  Exercises 2.2, 2.3 and 2.4 in Abelson and Sussman.  Proofs are not necessary.

; Consider the problem of representing line segments in a plane. Each segment is
; represented as a pair of points: a starting point and an ending point. Define a constructor
; make-segment and selectors start-segment and end-segment that define the representation
; of segments in terms of points. Furthermore, a point can be represented as a pair of numbers: the x
; coordinate and the y coordinate. Accordingly, specify a constructor make-point and selectors
; x-point and y-point that define this representation. Finally, using your selectors and
; constructors, define a procedure midpoint-segment that takes a line segment as argument and
; returns its midpoint (the point whose coordinates are the average of the coordinates of the endpoints).
; To try your procedures, you’ll need a way to print points:

;constructors
(define make-segment
  (lambda (p1 p2) (list p1 p2)))

(define make-point
  (lambda (x y) (list x y)))

;selectors
(define start-segment (lambda (x) (car x)))

(define end-segment (lambda (x) (car (cdr x))))

(define x-point (lambda (x) (car x)))

(define y-point (lambda (x) (car (cdr x))))


(define (midpoint-segment line-segment)
    (let ((x-value (/ (+ (x-point (start-segment line-segment)) (x-point (end-segment line-segment))) 2))
          (y-value (/ (+ (y-point (start-segment line-segment)) (y-point (end-segment line-segment))) 2)))
         (list x-value y-value) 
))

(define (print-point p)
 (newline)
 (display "(")
 (display (x-point p))
 (display ",")
 (display (y-point p))
 (display ")"))

;  Implement a representation for rectangles in a plane. (Hint: You may want to make use
; of exercise 2.2.) In terms of your constructors and selectors, create procedures that compute the
; perimeter and the area of a given rectangle. Now implement a different representation for rectangles.
; Can you design your system with suitable abstraction barriers, so that the same perimeter and area
; procedures will work using either representation? 

(define rectangle 
  (lambda (p1 p2) (list p1 p2)))

(define perimeter 
    (lambda (rec) 
      (let* ((s1 (- (y-point (end-segment rec)) (y-point (start-segment rec)))) 
             (s2 s1)
             (s3 (- (x-point (end-segment rec)) (x-point (start-segment rec))))
             (s4 s3))

             (+ s1 s2 s3 s4))))

(define area 
  (lambda (rec) (let ((width (- (y-point (end-segment rec)) (y-point (start-segment rec))))
                      (len (- (x-point (end-segment rec)) (x-point (start-segment rec)))))
                      (* width len))))

;exercise 2.4
; Here is an alternative procedural representation of pairs. For this representation, verify
; that (car (cons x y)) yields x for any objects x and y.
 (define (mycons x y)
  (lambda (m) (m x y)))

 (define (mycar z)
  (z (lambda (p q) p)))
; What is the corresponding definition of cdr? (Hint: To verify that this works, make use of the
; substitution model of section 1.1.5.) 

(define (mycdr z)
  (z (lambda (p q) q)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

