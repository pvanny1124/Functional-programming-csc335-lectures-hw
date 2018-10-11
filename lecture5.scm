

; CSc 335
; Lecture 5 
; September 20, 2018


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; class: 
;        look at ideas from Section 1.3 of Abelson and Sussman, folded  together with our ongoing
;        discussion of program proving

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Section 1.3  Formulating Abstractions with Higher-Order Procedures

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Section 1.3.1 Procedures as Arguments

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


; Consider 

(define (sigma a b)
  (cond ((> a b) 0)
	(else (+ a (sigma (+ a 1) b)))))

; As we have now developed careful proofs for both sigma and its corresponding
; iterative procedure, we will look at a sequence of procedures which increasingly
; generalize it.  In the course of doing so, we hope to shed some light on 
; higher order functions and their role in procedural abstraction.


; the first generalization is via a new parameter, term, which allows us to use
; the same code for many different computations

(define (sigma a b term)
  (cond ((> a b) 0)
	(else (+ (term a) (sigma (+ a 1) b term)))))

; if 

(define (term a)
  a)

(define (sum-integers a b)
  (sigma a b term))

; or

(define (sum-integers a b)
  (sigma a b (lambda (x) x)))


; we have again the original function. 


; but now we can do much more -- for example, instead of
; writing an entirely separate function to compute the sum of the squares
; of the integers from a to b, and another for the sum of the cubes of
; these integers, we can now reuse the sigma pattern:

(define (square x)
  (* x x))

(define (sum-squares a b)
  (sigma a b square))


(define (cube x)
  (* x (square x)))

(define (sum-cubes a b)
  (sigma a b cube))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; This is a good place to address the question of how one proves correct functions
; such as sigma which accept other functions as parameters.

; The short answer is this: one constructs a proof of (say) sigma which makes as few
; assumptions as possible about the parameter, term.  We can see from the code, for example, that
; term needs to be a function of one argument; if we assume that a is an integer, then
; clearly, term needs to allow integer arguments.  One would also want to say something
; about the value returned by the call (term a): for the current function, the best one
; can do is to say '(term a) must be a value which makes sense for +'.  

; Although we have ignored this issue up until now, the same thing needs to be stated
; for sigma itself: as part of the induction hypothesis, if one were being incredibly
; careful, one would say '(sigma a b term) returns a value which makes sense for +
; whenever gap(a,b) is less than ... '

; Continuing further in this direction would have us making assertions about +, >,
; and on and on through the implementation of recursion to the virtual memory system to ...

; It is understood, for all of our proofs, that at some reasonable point we stop -- at some
; point we say, simply, that the underlying systems are assumed correct.  In this sense,
; all of our proofs have been, implicitly, paremetrized proofs.  Now, with function
; parameters, the parametrization is explicit: we need to state clearly what we assume about
; the actual function parameters.

; Similarly, any termination argument we give for sigma must now be based on the assumption
; that the call (term a) terminates and returns a value. 


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; let's return to consider further enhancements of sigma.

; We can, for example, allow variation of the choice of
; the next term for inclusion in the summation

(define (sigma a next b term)
  (cond ((> a b) 0)
	(else (+ (term a) (sigma (next a) next b term)))))


; now we can obtain the original function as

(define (sum-integers a b)
  (sigma a (lambda (x) (+ x 1)) b (lambda (x) x)))


; and also


(define (sum-cubes a b)
  (sigma a (lambda (x) (+ x 1)) b (lambda (x) (* x x x))))



; we can even compute the sum

;    1         1          1 
;  -----  +  -----  +  -------  +  ...
;  1 * 3     5 * 7      9 * 11

; (which is known to converge to pi/8)

; as

(define (pi-sum a b)
  (define (pi-term x)
    (/ 1.0 (* x (+ x 2))))
  (define (pi-next x)
    (+ x 4))
  (sigma a pi-next b pi-term))


(* 8 (pi-sum 1 1000))
(* 8 (pi-sum 1 5000))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; a similar approach works for products

(define (prod a next b term)
  (cond ((> a b) 1)
	(else (* (term a) (prod (next a) next b term)))))


; allowing, for example

(define (factorial n)
  (prod 1 (lambda (x) (+ x 1)) n (lambda (x) x)))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; this suggests that we abstract + and * to a more general 'combiner', so that sigma and prod
; can be obtained as special cases of a more general function, which we call
; accumulate


(define (accumulate combiner init a next b term)
  (cond ((> a b) init)
	(else (combiner (term a) 
			(accumulate combiner init (next a) next b term)))))


(define (sigma a b)
  (accumulate (lambda (x y) (+ x y)) 0 a (lambda (x) (+ x 1))  b (lambda (x) x)))


(define (prod a b)
  (accumulate (lambda (x y) (* x y)) 1 a (lambda (x) (+ x 1)) b (lambda (x) x)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


; HOMEWORK A&S Exercises 1.29, 1.30, 1.31

;Exercise 1.30
; The sum procedure above generates a linear recursion. The procedure can be rewritten
; so that the sum is performed iteratively. Show how to do this by filling in the missing expressions in
; the following definition:

(define (sum term a next b)
 (if (> a b)
 0
 (+ (term a)
 (sum term (next a) next b))))


(define (sum term a next b)
 (define (iter a result)
 (if (> a b)
      result
 (iter (next a) (+ (term a) result))))
 (iter a 0))

;E
;  The sum procedure is only the simplest of a vast number of similar abstractions that can be
; captured as higher-order procedures. 51 Write an analogous procedure called product that returns
; the product of the values of a function at points over a given range. Show how to define factorial
; in terms of product. Also use product to compute approximations to using the formula 52
; b. If your product procedure generates a recursive process, write one that generates an iterative
; process. If it generates an iterative process, write one that generates a recursive process. 


(define (product term a b)
    (cond ((= a b) (term a))
          (else (* (term a) (product term (+ a 1) b))
    )
))

(define (product term a b)
    (define (product-iter term init result)
        (cond ((> init b) result)
              (else (product-iter term (+ init 1) (* (term init) result)))))
              (product-iter term a 1))

(define (factorial n)
    (product (lambda (x) x) 1 n))

    ; Exercise 1.32. 
    ; a. Show that sum and product (exercise 1.31) are both special cases of a still more
    ; general notion called accumulate that combines a collection of terms, using some general
    ; accumulation function:
    ; (accumulate combiner null-value term a next b)
    ; Accumulate takes as arguments the same term and range specifications as sum and product,
    ; together with a combiner procedure (of two arguments) that specifies how the current term is to be
    ; combined with the accumulation of the preceding terms and a null-value that specifies what base
    ; value to use when the terms run out. Write accumulate and show how sum and product can both
    ; be defined as simple calls to accumulate.
    ; b. If your accumulate procedure generates a recursive process, write one that generates an iterative
    ; process. If it generates an iterative process, write one that generates a recursive process. 

(define (accumulate combiner null-value term a next b)
      (cond ((> a b) null-value)
            (else (combiner (term a) (accumulate combiner null-value term (next a) next b)))))

(define (product term a b)
    (accumulate * 1 term a (lambda (x) (+ x 1)) b))

(define (sum term a b)
    (accumulate + 0 term a (lambda (x) (+ x 1)) b))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Section 1.3.2  The Relation between Let and Lambda

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



; Let allows one to bind variables as locally as possible to where they are to be used.

(define x 5)

(+ (let ((x 3))
     (+ x (* x 10)))
   x)


; let can be realized using lambda -- for example:

(+ ((lambda (x) (+ x (* x 10))) 3)
   x)


; One wants to be aware that a variable occurring in the definition part of a let will take its
; value from the context (environment) of the let.  In 


(define x 5)

(let ((x 3)
      (y (+ x 2)))
  (* x y))


; y will have the value 7 -- the outer x plus 2


; if one wants the second x to be bound by the first binding (x 3), one can use nested let, as
; follows

(let ((x 3))
  (let ((y (+ x 2)))
    (* x y)))


; perhaps more conveniently, use let*

(let* ((x 3)
       (y (+ x 2)))
  (* x y))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; HOMEWORK A&S Exercises 1.34, 1.37

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (simpsons-integral a b n)

    (define (alternate-coeff n k)
        (cond ((= k 0) 1)
              ((= k n) 1)
              ((if (= (modulo k 2) 0) #t #f) 2)
              (else 4)))

    (define (term a) a)
    (define (cube a) (* a a a))

    (define (h x y z) (/ (- y x) z))
  
    (define (simpson-iter coeff function k result)
                  (cond ((> k n) result)
                  (else (simpson-iter (alternate-coeff n (+ k 1)) 
                                      function 
                                      (+ k 1) 
                                      (+ result (* coeff (function (+ a (* k (h a b n))))))))))

    (* (/ (h a b n) 3) (simpson-iter 4 cube 0 (cube a))))
          
              


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;Exercise 1.37

;iterative
(define (cont-frac-iter n d k)
          (define (iter n d count result)
                  (cond ((= count 0) result)
                        (else (iter n d (- count 1) (/ (n count) (+ (d count) result))))))
        (iter n d (- k 1) (/ (n k) (d k))))

;recursive
(define (cont-frac n d k)
          (define (cont-frac-helper n d count)
              (cond ((= count k) (/ (n k) (d k)))
                    (else (+ (/ (n count) (+ (d count) (cont-frac-helper n d (+ count 1))))))
          ))
      (cont-frac-helper n d 1)
)

(cont-frac (lambda (i) 1.0)
 (lambda (i) 1.0)
 1000)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Introduction of Closures

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Section 1.3.4  Procedures as Returned Values

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (make-addConstant x)
  (lambda (y) (+ x y)))


((make-addConstant 4) 5)


(define add4 (make-addConstant 4))

(add4 3)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


; curried form of sigma

(define (curried-sigma term)
  (define (sum-term a b)
    (cond ((> a b) 0)
	  (else (+ (term a) (sum-term (+ a 1) b)))))

  sum-term)


((curried-sigma (lambda (x) x)) 1 10)


(define sum-of-squares
  (curried-sigma (lambda (x) (* x x))))


(sum-of-squares 1 10)


; contrast to

(sigma (lambda (x) (* x x)) 1 10)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(define (deriv f)
  (let ((dx .00000001))
    (lambda (x) (/ (- (f (+ x dx)) (f x))
		   dx))))

(define (cube x) (* x x x))


((deriv cube) 5)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


; We briefly introduce (what Abelson and Sussman call)
; the environment model of evaluation, as one means for lending operational
; intuition to our understanding of closures.

; Please see Chapter 3 in the A&S text. 


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; HOMEWORK A&S 1.41, 1.42, 1.43

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


; Exercise 1.41. Define a procedure double that takes a procedure of one argument as argument and
; returns a procedure that applies the original procedure twice. For example, if inc is a procedure that
; adds 1 to its argument, then (double inc) should be a procedure that adds 2. What value is
; returned by
; (((double (double double)) inc) 5)

(define (double proc) 
  (lambda (x) (proc (proc x))))

(((double (double double)) inc) 5)


; Exercise 1.42. Let f and g be two one-argument functions. The composition f after g is defined to be
; the function x f(g(x)). Define a procedure compose that implements composition. For example, if
; inc is a procedure that adds 1 to its argument,
; ((compose square inc) 6)
;

(define (compose f g) 
  (lambda (x) (f (g x))))

  ((compose square inc) 6)



  ; Exercise 1.43. If f is a numerical function and n is a positive integer, then we can form the nth
  ; repeated application of f, which is defined to be the function whose value at x is f(f(...(f(x))...)).
  ; For example, if f is the function x x + 1, then the nth repeated application of f is the function x x +
  ; n. If f is the operation of squaring a number, then the nth repeated application of f is the function that
  ; raises its argument to the 2
  ; n
  ; th power. Write a procedure that takes as inputs a procedure that computes
  ; f and a positive integer n and returns the procedure that computes the nth repeated application of f.
  ; Your procedure should be able to be used as follows:

  ((repeated square 2) 5)
  ;625

  ;recursive solution:

  (define (repeated f n)      
      (lambda (x) 
          (define (compose f g) 
             (lambda (x) (f (g x))))

          (define (helper f n)
              (cond ((= n 2) ((compose f f) x))
                    (else (f (helper f (- n 1))))))

          (helper f n)
      )
  )

  ;another way: 

  (define (repeated f n)
        (if (< n 1)
              (lambda (x) x)
        (lambda (x) (compose f (repeated f (- n 1)))))
  )


;iterative solution

  (define (repeated f n)      
    (lambda (x) 
        (define (compose f g) 
           (lambda (x) (f (g x))))

        (define (helper f n init result)
            (cond ((= init n) result)
                  (else (helper f n (+ init 1) (f result)))))

        (helper f n 2 ((compose f f) x))
    )
)

;another way (more elegantly without defining an extra parameter "init"):

(define (repeated f n)
        (if (< n 1)
              (lambda (x) x) ;returns a procedure that just returns x if n = 0
              (lambda (x) 
                  ;iterative procedure:
                  (define (iter f n result)
                      (cond ((= n 1) result)
                            (else (iter f (- n 1) ((compose f (lambda (x) x)) result)))))

                  (define (compose f g) 
                    (lambda (x) (f (g x))))

                  (iter f n ((compose f (lambda (x) x)) x))  
              )
        )
)


;homework 2
; Exercise 1.11:
; A function f is defined by the rule that f(n) = n if n<3 and f(n) = f(n - 1) + 2f(n - 2) +
; 3f(n - 3) if n> 3. Write a procedure that computes f by means of a recursive process. Write a procedure
; that computes f by means of an iterative process. 


f(0)
f(1)
f(2)

f(3) = f(2) + 2f(1) + 3f(0) = 0 + 2 + 2 = 4
f(4) = f(3) + 2f(2) + 3f(1) = 4 + 4 + 3 = 11

;we need previous 3 terms

;invariant -> result = prev + 2prev-prev + 3prev-prev-prev

;iterative (wrong) solution:
(define (f n)
     
      (define (iter prev pp ppp result init)
              (cond ((= init n) result)
                    (else (iter result prev pp (+ (* 3 pp) (* 2 prev) result) (+ init 1)))))

      (cond ((< n 4) n)
            (else (iter 3 2 1 (+ 3 (* 2 2) (* 3 1)) 4))))


; n < 3...
(define (f n)
     
            (define (iter prev pp ppp result init)
                    (cond ((= init n) result)
                          (else (iter result prev pp (+ (* 3 ppp) (* 2 pp) result) (+ init 1)))))
      
            (cond ((< n 3) n)
                  (else (iter 4 2 1 4 3))))

;analysis:

;invariant: result = f(n)
;design-roles: 
;       prev = f(n-1)
;       pp   = f(n-2)
;       ppp  = f(n-3)

;pre-cond: n > 0
;post: returns f(n) = f(n-1) + 2f(n-2) + 3f(n-3)

;test1: is invariant strong enough on termination?
;         on termination, result is returned
;         and is the f(nth) result or
;         result + 2prev + 3pp
;         where result is the result of the previous call f(n-1)
;         prev is the result of the prev-previous call f(n-2)
;         pp is the result of the prev-prev-previous call f(n-3)

;test2: passes on 1st call?
;         on k = 3, f(3)
;         result = f(2) + 2f(1) + 3f(0) = (3 + 2(2) + 3(0))
;         where f(2) = 3, f(1) = 2, f(0) = 0
;         effectively, f(2) is prev
;         f(1) is pp, and f(0) is ppp

;test3: does it pass on k+1st call?
;         on k+1 > 3,
;         result = RESULT + 2prev + 3pp
;         RESULT = (prev + 2pp + 3ppp)
;         where (prev + 2pp + 3ppp) = f(n-1), or rather, the previous result.
;         2prev = 2f(n-2), the prev-previous state
;         3pp = 3f(n-3), the prev-prev-previous state.
;         hence, the invariant: result = prev + 2prev-previous + 3prev-prev-previous
;         persists through each call.



;recursive (not so elegant) solution:

(define (f n)
        (define (helper prev pp ppp init)
                    (cond ((= init n) (+ 2 (* 2 pp) (* 3 ppp)))
                          (else (+ (* 2 pp) (* 3 ppp) 
                                   (helper (+ prev (* 2 pp) (* 3 ppp)) prev pp (+ init 1))))))
        (cond ((< n 3) n)
                  (else (helper 2 1 0 3))))
(f 4)


(define (f n)
        (cond ((< n 3) n)
                (+ (f (* 3 (- n 1))) (f (* 2 (- n 2))) (f (- n 1)))))



;analysis:

;basis-step: on n = 4, the helper function returns (+ 3 (* 2 pp) (* 3 ppp)).
;   which is (+ 2 (* 2 pp) (* 3 ppp)) 
;   which is f(3) + 2f(2) + 3f(1) 
;   which is f(4).

;induction hypothesis: assume (helper prev pp ppp init) returns f(n)

;induction step: (helper prev pp ppp (+ n 1)) =>
;             (+ (* 2 pp) (* 3 ppp) (+ (* 2 prev) (* 3 pp)))
;           => 2f(n-2-1) + 3f(n-3-1) + 2f(n-2) + 3f(n-3)
;           => 2f(n-3) + 3f(n-4) + 2f(n-2) + 3f(n-3)
;           => which is f(n+1) - f(3).
;           => if termination is reached, this becomes
;           => 2f(n-3) + 3f(n-4) + 2f(n-2) + 3f(n-3) + 3
;           => 3 accounts for f(3) = 3 added to the equation.
;           => hence,  f(n-2) + 2f(n-3) + 3f(n-4) + 2f(n-2) + 3f(n-3) = f(n+1)
;           => where f(n-2) + 2f(n-3) + 3f(n-4) = f(n)



; Exercise 1.12. The following pattern of numbers is called Pascal’s triangle.
; The numbers at the edge of the triangle are all 1, and each number inside the triangle is the sum of the
; two numbers above it. Write a procedure that computes elements of Pascal’s triangle by means of a
; recursive process. 

(define (pascal row col)
            (cond ((= row col) 1)
                  ((= col 0) 1)
                  (else (+ (pascal (- row 1) col) 
                           (pascal (- row 1) (- col 1))))))

;basis step: row = 0 and col = 0 => returns 1
;induction hypothesis: assume (pascal (- row 1) col) returns the element at row-1 and col
;and (pascal (- row 1) (- col 1)) returns the element at row-1 and col-1

;induction step: 

;quotient, remainder, truncate, zero?

;Problem 2:
; Write iterative and recursive scheme functions to return the sum of the digits within
; a non-negative integer.  For example, (sum-of-digits 345) is 12.

(define (pascal row col)
        (define (pascal-iter prev ppp result)
                    (cond (())
                          (else (+ (pascal-iter )))
                    )
        )

)
;iterative solution
(define (sum-of-digits n)
            (define (sum-iter rest result)
                      (cond ((= rest 0) result)
                            (else (sum-iter (quotient rest 10) (+ result (modulo rest 10))))))
          (sum-iter (quotient n 10) (modulo n 10))
)

;invariant: result = prev digit + next digit => (mod n 10) + (mod (quotient n 10) 10). 

;test1: is invariant strong enough on termination?
;     on termination, result = result + (mod rest 10)
;     (mod rest 10) is the last digit to add from right to left
;     result holds the result of the sum of the previous digits.

;test2: does it pass on 1st call?
;     on k=0,
;     result is (mod n 10) which is the first digit to be added

;test3: does it pass on k+1st call?
;     on k+1,
;     since RESULT = (mod n 10) on 1st call,
;     on next call, result = (mod n 10) + (mod (quotient n 10) 10)
;     (quotient n 10) returns the rest of the numbers immediately after the first (or previous),
;     which in this case, the first digit is RESULT.
;     (mod (quotient n 10) 10) will return the next digit to be added to the sum.
;     Hence result = (mod n 10) + (mod (quotient n 10) 10) returns the correct sum-of-digits.


;recursive solution

(define (sum-of-digits n)
            (cond ((= n 0) 0)
                  (else (+ (modulo n 10) (sum-of-digits (quotient n 10))))))

;basis-step: when n = 0, sum-of-digits return 0

;induction hypothesis: (sum-of-digits n) returns the first digit from right 
;                       to left of the rest of numbers from Ki+1 to Kn.
;                       from right to left, the digits are indexed as K0 < i < Kn

;induction step:  on (sum-of-digits (quotient n 10))



; 3.  Write iterative and recursive scheme programs to test whether the digits in a non-negative
; integer are in increasing order.  For example, the digits of 12348 are in increasing order, while
; those of 12343 are not.

;iterative solution:
(define (isIncreasing? n)
            (define (iter rest current prev result)
                    (cond ((not result) result) ;check is result is false
                          ((= rest 0) result) ;if result is true all the way to the end, return result.
                          (else (iter (quotient rest 10) 
                                      (modulo (quotient rest 10) 10) 
                                      (modulo rest 10)
                                      (and (if (> prev current) #t #f) result)))))

            (iter (quotient n 10) (modulo (quotient n 10) 10) (modulo n 10) #t))

;analysis:

;invariant: result = #t ^ (> (modulo n 10) (modulo (quotient n 10)))
;design-roles: rest: the rest of numbers from Ki+1 to K
;              current: the first digit from the rest of numbers from right to left
;              prev: the first digit from the original n

;1st test:  is the invariant strong enough on termination?
;           on termination, result is 
;           (and (if (> prev current) #t #f) RESULT)
;           in this case, RESULT is the previous result of result which must 
;           have been true for the code to reach this point.
;           prev = (modulo rest 10) where rest is (quotient n 10)
;           current = (modulo (quotient n 10) 10)
;           hence, the invariant holds true.

;2nd test:  on 1st call?
;           on first call, result = #t
;           to compute the next value of result,
;           we pass (and (if (> prev current) #t #f) result)
;           where again the invariant holds true.

;3rd test: on k+1st call?
;          ;if the result of the kth call returns true,
;          once again, the program passes into result
;          (and (if (> prev current) #t #f) result)
;          where again the invariant holds.

;termination occurs if the logical and of the previous result returns false or if the (quotient n 10) is 0.

;recursive solution:
(define (isIncreasing? n)
          (cond ((= n 0) #t)
                (else (and (if (> (modulo n 10) (modulo (quotient n 10) 10)) #t #f)
                           (isIncreasing? (quotient n 10))))))

; analysis:
; pre-cond: n >= 0, integer.

;(quotient n 10) returns the rest of the digits from Ki+1 to K
;Ki < i < K being the indices of the digits from right to left
;(modulo n 10) returns the first digit from right to left
;(module (quotient n 10) 10) returns the first digit from the rest of the digits from right to left.

;basis step: when n = 0 (smalles legal input), the program returns #t
;induction hypothesis: assume that (isIncreasing? (quotient n 10)) returns either true or false 
;                      depending on if (modulo n 10) of this new value of n
;                      is > than the (module (quotient n 10)) of the new value of n
;induction step:       on the k+1st call, we pass in (quotient n 10) to the
;                      recursive call of (isIncreasing?).
;                      this would result in 
;                      (and (if (> (modulo n 10) (modulo (quotient n 10) 10)))
;                       (and (if (> (modulo (quotient n 10) 10) (modulo (quotient (quotient n 10) 10)))))
;                      which will result in either true or false which is in unison with the
;                      induction hypothesis.