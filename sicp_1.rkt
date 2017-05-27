#lang racket

(define (square x)
  (* x x))

(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x)
                 x)))

(define (improve guess x)
  (average guess (/ x guess)))

(define (average x y)
  (/ (+ x y) 2))

(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))

(define (sqrt x)
  (sqrt-iter 1.0 x))

(define (cubic-iter guess x)
  (if (good-enough? guess x)
      guess
      (cubic-iter (improve-cubic guess x)
                 x)))

(define (improve-cubic guess-cubic x)
  (/ (+ (/ x (square guess-cubic)) guess-cubic guess-cubic) 3))

(define (cubic x)
  (cubic-iter 1.0 x))

(define (f-iter n) 
  (if (< n 3)
      n
      (+ (f-iter (- n 1)) (* 2 (f-iter (- n 2))) (* 3 (f-iter (- n 3))))
  )
)

(define (factorial n)
  (if (= n 1)
      1
      (* n (factorial (- n 1)))
  )
)

(define (Accahman x y)
  (cond ((= y 0) 0)
        ((= x 0) (* 2 y))
        ((= y 1) 2)
        (else (Accahman (- x 1) (Accahman x (- y 1))))
  )
)

(define (fast-expt b n)
  (cond ((= n 0) 1)
        ((even? n) (square (fast-expt b (/ n 2))))
        (else (* b (fast-expt b (- n 1))))
  )
)

(define (even? n)
  (= (remainder n 2) 0))

(define (double x) (+ x x))
(define (halve x) (/ x 2))

(define (cube x) (* x x x))

(define (fib n)
  (fib-iter 1 0 0 1 n))
(define (fib-iter a b p q count)
  (cond ((= count 0) b)
        ((even? count)
        (fib-iter a
                  b
                  (+ (* p p) (* q q))
                  (+ (* q q) (* 2 p q))
                  (/ count 2)))
        (else (fib-iter (+ (* b q) (* a q) (* a p))
                        (+ (* b p) (* a q))
                        p
                        q
                        (- count 1)
))))

(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))
  )
)

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m)) m))
        (else (remainder (* base (expmod base (- exp 1) m)) m))))

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else false)))

(define (smallest-divisor n) (find-divisor n 2))
(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))
(define (divides? a b) (= (remainder b a) 0))
(define (prime? n) (= n (smallest-divisor n)))

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a) (sum term (next a) next b))))

(define (product term a next b)
  (if (> a b)
      1
      (* (term a) (sum term (next a) next b))))

(define (integral f a b dx)
  (define (add-dx x)
    (+ x dx))
  (* (sum f (+ a (/ dx 2.0)) add-dx b) dx))

(define (symp f a b n)
  (define h (/ (- b a) n))
  (/ (* h (+ (f a)
     (* 4 (sum f (+ a h) (lambda (x) (+ x h h)) (- b h)))
     (* 2 (sum f (+ a h h) (lambda (x) (+ x h h)) (- b h h)))
     (f b))) 3))

(define (solve-pi n)
  (* 4
     (product (lambda (x) (/ x (+ x 1))) 2 (lambda (x) (+ x x)) n)
     (product (lambda (x) (/ (+ x 1) x)) 3 (lambda (x) (+ x x)) (- n 1))
  )
)

