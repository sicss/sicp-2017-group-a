#lang racket

(define (square x) (* x x))
(define (smallest-divisor n) (find-divisor n 2))
(define (find-divisor n test-divisor)
(cond ((> (square test-divisor) n) n)
((divides? test-divisor n) test-divisor)
(else (find-divisor n (+ test-divisor 1)))))
(define (divides? a b) (= (remainder b a) 0))

(define (prime? n)
(if (< 1 n) (= n (smallest-divisor n)) false))

(define (cube x) (* x x x))

;(define (sum term a next b)
;(if (> a b)
;0
;(+ (term a)
;(sum term (next a) next b))))

(define (inc n) (+ n 1))

(define (sum-cubes a b)
(sum cube a inc b))

(define (identity x) x)

(define (sum-integers a b)
(sum identity a inc b))

(define (pi-sum a b)
(define (pi-term x)
(/ 1.0 (* x (+ x 2))))
(define (pi-next x)
(+ x 4))
(sum pi-term a pi-next b))

(define (integral f a b dx)
(define (add-dx x)
(+ x dx))
(* (sum f (+ a (/ dx 2.0)) add-dx b)
dx))

(define (symp f a b n)
  (define h (/ (- b a) n))
  (/ (* h (+ (f a) (f b)
             (* 4 (sum f (+ a h)   (lambda (x) (+ x h h)) (- b h)))
             (* 2 (sum f (+ a h h) (lambda (x) (+ x h h)) (- b h h)))
             ))
     3))

(define (sum term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (+ result (term a)))))
  (iter a 0))

;(define (product term a next b)
;  (define (iter a result)
;    (if (> a b)
;        result
;        (iter (next a) (* result (term a)))))
;  (iter a 1))

(define (product term a next b)
  (if (> a b)
      1
      (* (product term (next a) next b) (term a)))
)

(define pi
  (+ (/ (* 4 (product identity 2 (lambda (x) (+ x 2)) 20002) (product identity 4 (lambda (x) (+ x 2)) 20004))
     (* 1 (product identity 3 (lambda (x) (+ x 2)) 20003) (product identity 3 (lambda (x) (+ x 2)) 20003)))
   0.0))

;(define (accumulate combiner null-value term a next b)
;  (if (> a b)
;      null-value
;      (combiner (accumulate combiner null-value term (next a) next b) (term a)))
;)

(define (accumulate combiner null-value term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (combiner result (term a)))))
  (iter a null-value))

(define (filtered-accumulate combiner null-value term a next b filter)
  (define (iter a result)
    (if (> a b)
        result
        (if (filter a)
            (iter (next a) (combiner result (term a)))
            (iter (next a) result))))
  (iter a null-value))

(define (prime-sq-plus a b) (filtered-accumulate + 0 square a inc b prime?))
(define (1-33-b n) (filtered-accumulate * 1 identity 2 inc (- n 1) (lambda (x) (= (gcd n x) 1))))

(define (average x y) (/ (+ x y) 2))

(define (close-enough? x y) (< (abs (- x y)) 0.001))

(define (search f neg-point pos-point)
(let ((midpoint (average neg-point pos-point)))
(if (close-enough? neg-point pos-point)
midpoint
(let ((test-value (f midpoint)))
(cond ((positive? test-value)
(search f neg-point midpoint))
((negative? test-value)
(search f midpoint pos-point))
(else midpoint))))))

(define (half-interval-method f a b)
(let ((a-value (f a))
(b-value (f b)))
(cond ((and (negative? a-value) (positive? b-value))
(search f a b))
((and (negative? b-value) (positive? a-value))
(search f b a))
(else (error "Values are not of
opposite sign" a b)))))

(define tolerance 0.00001)
(define (fixed-point f first-guess)
  (define (close-enough? v1 v2) (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (display next) (newline)
      (if (close-enough? guess next)
      next
      (try next))
    )
  )
  (try first-guess)
)

;(define (cont-frac n d k)
;  (if (<= k 1)
;      (/ (n k) (d k))
;      (/ (n k) (+ (d k) (cont-frac n d (- k 1))))))
(define (cont-frac n d k)
  (define (iter k result)
    (if (<= k 1)
      result
      (iter (- k 1) (/ (n k) (+ (d k) result)))))
  (iter k (/ (n k) (d k)))
)

(define (int? x)
  (define (iter x)
    (cond ((= x 0) #t)
          ((< x 0) #f)
          (else iter (- x 1))
    )
  )
  (if (>= x 0) (iter x) (iter (- 0 x)))
)

(define (make-rat n d)
  (cond ((> (* n d) 0) (let ((g (gcd n d))) (cons (/ n g) (/ d g))))
        ((< (* n d) 0) (let ((g (gcd n d))) (cons (- 0 (/ n g)) (/ d g))))
        (else (error "You can't set 0 for the numer and the denom"))
  )
)
(define one-third (make-rat 1 3))
(define one-half (make-rat 1 2))
(define (numer x) (car x))
(define (denom x) (cdr x))
(define (add-rat x y) (make-rat (+ (* (numer x) (denom y)) (* (numer y) (denom x))) (* (denom x) (denom y))))
(define (sub-rat x y) (make-rat (- (* (numer x) (denom y)) (* (numer y) (denom x))) (* (denom x) (denom y))))
(define (mul-rat x y) (make-rat (* (numer x) (numer y)) (* (denom x) (denom y))))
(define (div-rat x y) (make-rat (* (numer x) (denom y)) (* (denom x) (numer y))))
(define (equal-rat? x y) (= (* (numer x) (denom y)) (* (numer y) (denom x))))
(define (print-rat x) (newline) (display (numer x)) (display "/") (display (denom x)))

(define (make-point x y) (cons x y))
(define (x-point p) (car p))
(define (y-point p) (cdr p))

(define (make-segment a b) (cons a b))
(define (start-segment l) (car l))
(define (end-segment l) (cdr l))

(define (rectangle seg shita) (cons seg shita))
(define (rec-start-point rec) (car (car rec)))
(define (rec-end-point rec) (cdr (car rec)))
(define (rec-shita rec) (cdr rec))
(define (length-around rec) (+ (abs (- (x-point (rec-start-point rec)) (x-point (rec-end-point rec))))
                               (abs (- (x-point (rec-start-point rec)) (x-point (rec-end-point rec))))
                               (abs (- (y-point (rec-start-point rec)) (y-point (rec-end-point rec))))
                               (abs (- (y-point (rec-start-point rec)) (y-point (rec-end-point rec))))))
(define (area rec) (abs (* (- (x-point (rec-start-point rec)) (x-point (rec-end-point rec)))
                           (- (y-point (rec-start-point rec)) (y-point (rec-end-point rec))))))

(define (mid-point l)
  (make-point (/ (+ (x-point (start-segment l)) (x-point (end-segment l))) 2)
              (/ (+ (y-point (start-segment l)) (y-point (end-segment l))) 2)))
(define (print-point p) (newline) (display "(") (display (x-point p)) (display ",") (display (y-point p)) (display ")"))


(define (storage-2-3 a b)
  (define (power r t) (if (> t 1) (* r (power r (- t 1))) r))
  (* (power 2 a) (power 3 b))
)
(define (storage-2-3-former s)
  (define (iter s t) (if (= (gcd s 2) 2) (iter (/ s 2) (+ t 1)) t))
  (iter s 0))
(define (storage-2-3-latter s)
  (define (iter s t) (if (= (gcd s 3) 3) (iter (/ s 3) (+ t 1)) t))
  (iter s 0))

(define (make-interval l u) (cons l u))
(define (lower-bound i) (car i))
(define (upper-bound i) (cdr i))

(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y)))
)
(define (sub-interval x y)
  (make-interval (- (lower-bound x) (upper-bound y))
                 (- (lower-bound x) (upper-bound y)))
)

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
       (make-interval (min p1 p2 p3 p4) (max p1 p2 p3 p4)))
) 

(define (div-interval x y)
  (if (= (lower-bound y) (upper-bound y))
  (error "Width must be positive")
  (mul-interval
   x
   (make-interval (/ 1.0 (upper-bound y))
                  (/ 1.0 (lower-bound y))))))

(define (make-center-width c w)
  (make-interval (- c w) (+ c w)))
(define (make-center-percent c p)
  (make-interval (* c (- 1.0 p)) (* c (+ 1.0 p))))
(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))
(define (width i)
  (/ (- (upper-bound i) (lower-bound i)) 2))

(define (get-list l n)
  (if (null? l) (error "Over the size of the list!")
  (if (= n 0)
      (car l)
      (get-list (cdr l) (- n 1)))))

(define (length l)
  (define (length-iter l n)
    (cond ((null? l) n)
          ((not (pair? l)) (+ n 1))
          (else (length-iter (cdr l) (+ n 1)))))
  (length-iter l 0))

(define (last-pair l)
  (cons (get-list l (- (length l) 1)) null))

(define (reverse l)
  (define num (length l))
  (define (iter l new-l n)
    (if (= n num)
        new-l
        (iter l (cons (get-list l n) new-l) (+ n 1))))
  (iter l null 0))

(define (make-list n)
  (define (iter m)
  (if (= m 0)
      null
      (cons m (iter (- m 1)))))
  (reverse (iter n)))

(define (no-more? list) (null? list))
;(define (no-more? list) (if (null? list) #t (null? (car list))))
(define (first-denomination coin-values)
  (car coin-values))
(define (except-first-denomination coin-values)
  (cdr coin-values))

(define (calc-list list f)
  (define (iter new-list)
    (if (no-more? new-list)
        new-list
        (cons (f (first-denomination new-list)) (iter (except-first-denomination new-list)))))
  (iter list)
)

(define (filter list condition)
  (define (iter list new-list)
    (if (no-more? list)
        new-list
        (if (condition (first-denomination list))
            (iter (except-first-denomination list) (cons (first-denomination list) new-list))
            (iter (except-first-denomination list) new-list)))
    )
  (reverse (iter list null)))

(define us-coins (list 50 25 10 5 1))
(define uk-coins (list 100 50 20 10 5 2 1 0.5))
(define jp-coins (list 500 100 50 10 5 1))

(define (cc amount coin-values)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (no-more? coin-values)) 0)
        (else
         (+ (cc amount
                (except-first-denomination coin-values))
            (cc (- amount (first-denomination coin-values))
                coin-values)))))

(define (same-parity x . list)
  (define parity (remainder x 2))
  (define (iter list new-list)
    (if (no-more? list)
        new-list
        (if (= parity (remainder (first-denomination list) 2))
            (iter (except-first-denomination list) (cons (first-denomination list) new-list))
            (iter (except-first-denomination list) new-list)))
    )
  (reverse (iter list (cons x null))))

;(define (t-map tree f)
;  (if (list? tree)
;      (t-map (lambda (tr) (t-map tr f)) f)
;      (f tree)))

(define (for-each f list)
  (define (iter list)
    (if (no-more? list)
        null
        ((f (car list)) (iter (cdr list)))
    )
  )
  (iter list)
)

(define (count-leaves x)
  (cond ((null? x) 0)
        ((not (pair? x)) 1)
        (else (+ (count-leaves (car x)) (count-leaves (cdr x))))))

(define (deep-reverse l)
  (define num (length l))
  (define (iter l new-l n)
    (if (= n num)
        new-l
        (iter l (cons (if (pair? l) (deep-reverse (get-list l n)) l) new-l) (+ n 1))))
  (if (pair? l) (iter l null 0) l)
)

(define (add-list a b)
  (define (iter a b)
    (if (null? a)
      b
      (iter (cdr a) (cons (car a) b))))
  (iter (reverse a) b)
)
(define (remove-null l)
  (define (iter l new-l)
    (if (null? l)
        new-l
        (if (null? (car l))
          (remove-null (cdr l))
          (add-list (list (car l)) (remove-null (cdr l))))))
  (iter l null)
  )

(define (fringe l)
  (define (iter l new-l)
    (if (pair? l)
      (add-list (iter (car l) new-l) (iter (cdr l) new-l))
      (cons l new-l))
  )
  (remove-null (iter l null))
)

(define x (list (list 1 2) (list 3 4)))


















