(define TOLERANCE 0.0001)

(define (square x) (* x x))

(define (pow x y)
    (cond ((= y 1) x)
    (else (* x (pow x (- y 1))))))

(define (abs x)
    (cond
        ((< x 0) (- x))
        (else x)))

(define (average-iter good-enough? improve guess x)
(if (good-enough? guess x)
    guess
    (average-iter good-enough? improve (improve guess x) x)))

(define (average x y)
    (/ (+ x y) 2))

(define (sqrt-improve guess x)
    (average guess (/ x guess)))

(define (good-enough? guess x)
    (< (abs (- (square guess) x)) TOLERANCE))

(define (sqrt x)
    (average-iter good-enough? sqrt-improve 1.0 x))

(display (sqrt 9))
(newline)

;;; Newton's method for cube roots
;;; Exercise 1.8

(define (cube-root-improve guess x)
  (/ (+ (/ x (square guess)) (* guess 2)) 3))

(define (cube-root-good-enough? guess x)
    (< (abs (- (pow guess 3) x)) TOLERANCE))

(define (cube-root x)
    (average-iter cube-root-good-enough? cube-root-improve 1.0 x))

(display (cube-root 8))
(newline)
