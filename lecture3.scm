
; Lecture 3 CSc 335
; September 4 2018



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


; Section 1.2 Procedures and the Processes they Generate


; recursive and iterative processes are distinguished.

; recursive processes are characterized by their building up a chain of
; deferred operations.

; in the process engendered by the fact procedure, for example, a chain
; of deferred multiplications is created

(define (fact x)
  (cond ((= x 0) 1)
	(else (* x (fact (- x 1))))))


; (fact 6)
; (* 6 (fact 5))
; (* 6 (* 5 (fact 4)))
; (* 6 (* 5 (* 4 (fact 3))))
; (* 6 (* 5 (* 4 (* 3 (fact 2)))))
; (* 6 (* 5 (* 4 (* 3 (* 2 (fact 1))))))
; (* 6 (* 5 (* 4 (* 3 (* 2 (* 1 (fact 0)))))))
; (* 6 (* 5 (* 4 (* 3 (* 2 (* 1 1))))))
; (* 6 (* 5 (* 4 (* 3 (* 2 1)))))
; (* 6 (* 5 (* 4 (* 3 2))))
; (* 6 (* 5 (* 4 6)))
; (* 6 (* 5 24))
; (* 6 120)
; 720


; as you might expect, one certifies the correctness of fact using
; induction on the non-negative integer input, x

; basis step: start with the smallest legal input, that is, x = 0
;             and observe that (fact 0) = 1, which is correct.  

; induction hypothesis:  assume (fact k) works correctly, that is, 
;             that the value returned by the call (fact k) is exactly
;             the factorial of k (written k!, as you know)

; induction step:  we show, using the induction hypothesis, that (fact (+ k 1))
;             works correctly.  According to the code, (fact (+ k 1)) returns
;             (* (+ k 1) (fact k)).  If (fact k) = k!, as it must by the 
;             induction hypothesis, then multiplying it by (+ k 1) certainly
;             returns the factorial of (+ k 1).



; we would want to check in addition that the program terminates - that is, that it
; does eventually return an answer.  we do this without using induction,
; as follows: as n >= 0 is an integer,
; and as each call to fact reduces the value of this parameter by 1, and as the procedure
; halts when n = 0, we see that any call (fact n) will terminate.  





; another design for a factoral procedure does not create a chain of
; deferred operations

(define (new-fact x)
  (fact-iter x 0 1))

(define (fact-iter x count result)
  (if (= count x)
      result
      (fact-iter x (+ count 1) (* (+ count 1) result))))


; here we see that the work is done by updating the values of count and
; result

;; (new-fact 6)
;; (fact-iter 6 0 1)
;; (fact-iter 6 1 1)
;; (fact-iter 6 2 2)
;; (fact-iter 6 3 6)
;; (fact-iter 6 4 24)
;; (fact-iter 6 5 120)
;; (fact-iter 6 6 720)



; such processes are said to be linear recursive, or iterative



; an alternate organization of the iterative version  casts fact-iter as a local function:

(define (new-fact n)
  (define (fact-iter count result)
    (if (= count n)
	result
	(fact-iter (+ count 1) (* (+ count 1) result))))
  (fact-iter 0 1))


; can you see the advantages of using a local function in this instance?  observe, for example,
; that there is no need to pass the parameter x to the looping function fact-iter when it is
; defined inside new-fact.  observe as well that the second form results in less clutter
; in the global namespace. 


; now for a proof that the iterative version works as advertised


; one certifies the correctness of (either version of) fact-iter by
; exploiting an invariant relationship which holds among the program's
; variables:

;  as the program runs, result = count!

; what good is this?  if it holds when the program exits, and if count = n
; when the program exits, then the value returned (namely, result) is
; equal to n!  that is, we can use the relation to show that the program
; works correctly.

; we use induction - this time on the number of calls to fact-iter -
; to show that the invariant relationship, namely result = count!, holds 
; every time fact-iter is called.

; the basis step is then to show that result = count! the first time
; fact-iter is called.  as we initialize count to 0 and result to 1,
; this is clear.

; the induction hypothesis: assume result = count! on the kth call to
; fact-iter

; the induction step: on the (k+1)st call to fact-iter, the value of
; count in the body of fact-iter is k and - by the induction hypothesis -
; the value of result is k!.  so now look at the code: count is replaced
; by (+ count 1) and result by (* (+ count 1) result).  so we need to check
; that (* (+ count 1) k!) is in fact (k + 1)!.  but this is clear.

; it follows then that count! = result, each time fact-iter is called.
; In particular, this equation holds the last time fact-iter is called -
; that is, just before it exits.  looking at the code, we see that the
; value returned (ie, result) is n!, just as we wanted. 

; we've demonstrated - modulo the correctness of the underlying arithmetic
; operations and number representations - that (new-fact n) computes n!

; (providing n >= 0 -- more about input restrictions - called
; pre-conditions - later)




; such relationships are not always this easy to spot, but we shall be on
; the lookout for them when designing iterative processes




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; another example showing the difference between recursion and iteration -
; material from section 1.2.4 in sicp (the proof discussion is added locally!)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; exponentiation 

; compute b^n = b * b^(n-1)
;         b^0 = 1
; assuming b <> 0, and assuming n >= 0 is an integer


(define (expt b n)
  (if (= n 0)
      1
      (* b (expt b (- n 1)))))


; what kind of process is this?
; can we transform it, if recursive, to an iterative process?  (alternately,
; if it is iterative, can we transform it to a recursive process?)


(define (expt b n)
  (expt-iter b n 1))

(define (expt-iter b exponent result-so-far)
  (if (= exponent 0)
      result-so-far
      (expt-iter b (- exponent 1) (* b result-so-far))))


; practice: we prove both versions correct



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


; induction argument for the recursive expt

; induction on what?  how about: induction on n

; basis step: does expt work correctly when n = 0?  in this case, we see
; that the value returned is 1, and since b^0 is always 1 for b <> 0, that this value is correct

; induction hypothesis: we assume (expt b n) returns the correct value, ie, b^n

; induction step: we argue, using the induction hypothesis, that (expt b (+ n 1)) returns
; the correct value.  looking at the code, and noting that (+ n 1) is not 0 -- as we assumed
; that n >= 0 -- we see that the value returned is (* b (expt b n)).  but, by the induction
; hypothesis, this is b times b^n -- clearly correct.

; this completes the induction showing correctness of (expt b n), for expt defined as above,
; when b <> 0 and n >= 0 an integer


; actually, there is one more thing to check: does the program ever terminate?  Does it
; in fact return a value?

; for this, we argue informally: the input n is a non-negative integer on start, and each
; recursive call reduces the value of the n parameter by 1. Eventually n is 0, at which point
; the program terminates.



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; given the importance of the input restrictions for both the factorial and exponentiation
; procedures, we introduce the concept of _specification_

; a specification - for now, in this course - will be a triple of the form

; {input restrictions} procedure-call {description of returned value}

; frequently we will refer to {input restrictions} as the pre-condition, and
; to {description of returned value} as the post-condition


; for example: if the precondition (b <> 0 AND n >= 0 AND n is an integer) is satisfied,
; the call (expt b n) will correctly compute the value b^n

; for example: if the precondition (n >=0 AND n is an integer) is satisfied, then
; (new-fact n) will correctly compute n!


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; preconditions are better, the weaker (less specific) they are, provided they are still
; strong enough for the proof to work

; for example, (b <> 0 AND b is an integer AND n >= 0 AND n is an integer) is stronger
; than the precondition we gave above - the second implies the first - but it is unnecessarily
; restrictive: the program and its proof work even if b is not an integer.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; again, triples of the form

; {precondition} proc {postcondition} 

; will be said to be specifications

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;







;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; let's now display a proof sketch for expt-iter

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



; guess-invariant for expt-iter:  b^N = b^exponent * result, where N is the initial value of n


; first check:

; is the guess-invariant strong enough to imply the desired result when termination occurs?

; termination occurs when exponent = 0.  When exponent = 0, the guess-invariant
; is b^N = b^0 * result, ie, b^N = result


; so now it is worth looking into whether the guess-invariant really is invariant.  This is
; a two-step process:

; is the guess-invariant achieved initially?  Clearly, b^N = b^N * 1, so YES.

; is the guess-invariant preserved from one call of expt-iter to the next?
; suppose b^N = b^exponent * result,
; and consider whether b^N = b^(exponent - 1) * (b * result).

; Of course this is true


; We conclude that our guess-invariant really is invariant. 



; does termination actually occur?  We need to assume here that N >= 0, and that N is an
; integer.  So decreasing exponent by 1 with each call will eventually reach 0.  Termination
; does occur.


; and when it does, we know - from the invariant - that the value returned is b^N.  The
; program works!



; what is the specification?

; pre: b is non-zero and n >= 0 is an integer
; post: b^n is returned





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; what test cases would you run, for any of these programs?  What is the
; role of testing when we have proofs?  Do we abandon testing?

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; absolutely not!

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


; practice: develop an exponentiation program resulting from this design idea

;   b^exponent = result-so-far  is maintained invariant as exponent
;   is increased from 0 to n

(define (expo b x)
    (define (exp-iter b x n result-so-far)
        (cond ((> n x) result-so-far)
              ((= n 0) (exp-iter b x (+ n 1) 1)) ;need this check for b^0
              (exp-iter b x (+ n 1) (* result-so-far b))))
    (exp-iter b x 0 1))

;guess-invariant: result-so-far = b^exp
;1st test: Strong enough? (is program correct upon termination?)
      ;as n is incremented, the program terminates when n is greater than x
      ;resulting in b^exp

;2nd test: Passes on 1st call?
      ;On first pass, when n = 0, the program returns result-so-far = b^0 = 1
      ;by passing 1 as an argument to result-so-far

;3rd test: True on (k+1)st call?
      ;on n = (N + 1)st call, the argument passed into result-so-far in the tail recursion:
      ;(exp-iter b x (+ n 1) (* result-so-far b))
      ;(* result-so-far b) computes b^n


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; homework exercises to be completed for the next class

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; 1. Exercise 1.7 from A&S
; 2. Exercise 1.8 from A&S

; 3. Exercise 1.9 from A&S, with additional requirements

; consider two procedures for adding two positive integers

(define (my-plus-version-1 a b)
  (if (= a 0)
      b
      (inc (my-plus-version-1 (dec a) b))))

(define (my-plus-version-2 a b)
  (if (= a 0)
      b
      (my-plus-version-2 (dec a) (inc b))))

; where

(define (inc x) (+ x 1))

(define (dec x) (- x 1))



; Using the substitution model, illustrate the process generated by each
; procedure in evaluating (my-plus 4 5).  Are the processes recursive or
; iterative?

; Certify each procedure, using the appropriate technique, as determined
; by your answer to the classification question above: for the recursive
; procedure, give a proof based on the size of (one of) the arguments; for
; the iterative procedure, give a proof which uses an invariant (which you
; must first discover)

;Exercise 1.7. The good-enough? test used in computing square roots will not be very effective for
; finding the square roots of very small numbers. 

;Also, in real computers, arithmetic operations are
; almost always performed with limited precision. This makes our test inadequate for very large
; numbers. Explain these statements, with examples showing how the test fails for small and large
; numbers. An alternative strategy for implementing good-enough? is to watch how guess changes
; from one iteration to the next and to stop when the change is a very small fraction of the guess. Design
; a square-root procedure that uses this kind of end test. Does this work better for small and large
;numbers? 

;test fails with (square (my-sqrt 1000)) because this ends up with a float.
;test fails with (square (my-sqrt .0025)) and gives you .0029... same as before, a float.

;need to stop when the change is a small fraction of guess.

;guess is updated through every iteration by taking the average of x/guess 
;and the guess.

(define (my-sqrt x)

  ; main auxilliary function

  (define (sqrt-iter old-guess new-guess)
    (if (good-enough? old-guess new-guess 0.000000000000001)
	new-guess
	(sqrt-iter new-guess (improve new-guess))))


  (define (good-enough? old-guess new-guess delta) ;delta is a small fraction such as 0.000001 for example
    (<= (abs (- old-guess new-guess)) delta)) ;stop when change is a small enough

  (define (improve guess)
    (average guess (/ x guess)))

  (define (average x y) (/ (+ x y) 2))

  (define (square x) (* x x))

  (define (abs x)
    (if (>= x 0)  
	x
	(- x)))

 (sqrt-iter 0.0 1.0))


; Exercise 1.8. Newton’s method for cube roots is based on the fact that if y is an approximation to the
; cube root of x, then a better approximation is given by the value
; Use this formula to implement a cube-root procedure analogous to the square-root procedure. (In
; section 1.3.4 we will see how to implement Newton’s method in general as an abstraction of these
; square-root and cube-root procedures.) 

(define (cube-root x)

(define (good-enough? old-guess new-guess delta) ;delta is a small fraction such as 0.000001 for example
  (<= (abs (- old-guess new-guess)) delta))

(define (cube x) (* x x x))

(define (square x) (* x x))

(define (average-by-3 x y)
    (/ (+ x y) 3))

(define (improve y)
    (average-by-3 (/ x (square y)) (+ (* 2 y)) ))

(define (abs x)
    (if (>= x 0)  
	        x
        (- x)))

(define (cube-iter old-guess new-guess)
  (if (good-enough? old-guess new-guess 0.000000001)
        new-guess
  (cube-iter new-guess (improve new-guess))))
  
  (cube-iter 0.0 1.0))


; Exercise 1.9. Each of the following two procedures defines a method for adding two positive integers
; in terms of the procedures inc, which increments its argument by 1, and dec, which decrements its
; argument by 1.
(define (+ a b)
 (if (= a 0)
 b
 (inc (+ (dec a) b))))

(define (+ a b)
 (if (= a 0)
 b
 (+ (dec a) (inc b))))

;Can't re-define pre-defined functions in the global namespace...
;Redo these like this:

(define (my-plus-version-1 a b)
  (if (= a 0)
      b
      (inc (my-plus-version-1 (dec a) b))))

(define (my-plus-version-2 a b)
  (if (= a 0)
      b
      (my-plus-version-2 (dec a) (inc b))))

; Using the substitution model, illustrate the process generated by each procedure in evaluating (+ 4
; 5). Are these processes iterative or recursive?

;Since these are user-defined functions, they do not have their own order of evaluation and follow applicative order

;Evaluating (+ 4 5) with 1st function:

;For the sake of simplicity, use + instead of my-plus-version-1 for better readibility.

  (inc (+ (dec 4) 5))
  (inc (+ 3 5)) 
  (inc (inc (+ (dec 3) 5)))
  (inc (inc (+ 2 5)))
  (inc (inc (inc (+ (dec 2) 5))))
  (inc (inc (inc (+ 1 5))))
  (inc (inc (inc (inc (+ (dec 1) 5)))))
  (inc (inc (inc (inc (+ 0 5)))))
  (inc (inc (inc (inc 5))))
  (inc (inc (inc 6)))
  (inc (inc 7))
  (inc 8)
  9

;Since this is a series of deffered operations, this is a recursive process

;Basis step:
;On first pass, if a = 0, the sum is properly returned as 0, effectively a + b

;Induction Hypthothesis:
;Assume that (my-plus-version-1 (dec a) b) returns (a - 1) + b 

;Induction Step:
;(inc (my-plus-version-1 (dec a) b)) returns (1 + ((a - 1) + b)) which is just (a + b)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;Evaluating (+ 4 5) with 2nd function:

(my-plus-version-2 (dec 4) (inc 5))
(my-plus-version-2 3 6)
(my-plus-version-2 (dec 3) (inc 6))
(my-plus-version-2 2 7)
(my-plus-version-2 (dec 2) (inc 7))
(my-plus-version-2 1 8)
(my-plus-version-2 (dec 1) (inc 8))
(my-plus-version-2 0 9)
9

;Since there are no deferred operations, this is an iterative process.

;Guess-invariant: A + B = a + b


;1st test: Strong enough? (is program correct upon termination?)
      ;a is decremented on each pass and the program terminates when a reaches 0
      ;program terminates when a = 0 and b effectively results in  a + b

;2nd test: Passes on 1st call?
      ;On first pass, a = A and b = B
      ;A + B = a + b still holds because, for example, 
      ;3 + 6 = 9 on first call. 

;3rd test: True on (k+1)st call?
      ;Since a = A and b = B,
      ;a = (a - 1) and b = (b + 1) on (k+1)st call
      ;A + B = (a - 1) + (b + 1) = a + b