;;; Defining constants
(define a 5)

;;; Defining procedures
(define (square x) (* x x))

(define (fib n)
    (cond ((= n 0) 0)
          ((= n 1) 1)
          (else (+ (fib (- n 1))
                   (fib (- n 2))))))

;;; Some list fun

(define l1 (list 1 2 3)) ;;; (1 2 3)
(define segment1 (cons 1 (cons 2 (cons 3 '())))) ;;; (1 2 3)

; (display (= segment1 l1)) ;;; => Wrong type argument in position 1: (1 2 3)
; (display (eq? l1 segment1)) ;;; => #f
; (newline)

;;; Ok, let's write it then...

(define (lists-have-same-values? list1 list2)
  (cond
    ;;; True if 2 null lists are passed in
    ((and (null? list1) (null? list2)) #t)
    ;;; False if only one of the lists is null but the other is not
    ((or (null? list1) (null? list2)) #f)
    ;;; False if heads are not equal
    ((not (eq? (car list1) (car list2))) #f)
    ;;; True if current elements are equal and next of both elements are null
    ((and (null? (cdr list1))
          (null? (cdr list2))
          (eq? (car list1) (car list2))) #t)
    ;;; Recursively check the remainders
    (else (lists-have-same-values? (cdr list1) (cdr list2)))))


(define (run-test test-name actual expected)
  (display test-name)
  (display " | actual: ")
  (display actual)
  (display " | expected: ")
  (display expected)
  (newline)
  (assert-equal actual expected))


(define (assert-equal actual expected)
  (if (eq? actual expected)
      (begin
          (display "Test passed.")
          (newline))
      (begin
        (display "Test failed.")
        (newline))))


(run-test "Test 1" (lists-have-same-values? (list 1) (list 2)) #f)
(run-test "Test 2" (lists-have-same-values? (list 1) (list 1)) #t)
(run-test "Test 3" (lists-have-same-values? (list 1 2) (list 1 3)) #f)
(run-test "Test 4" (lists-have-same-values? (list 1 2 3) (list 1 2 3)) #t)
(run-test "Test 5" (lists-have-same-values? (list 1 2 3 4) (list 1 2 3)) #f)
(run-test "Test 6" (lists-have-same-values? '() '()) #t)
(run-test "Test 7" (lists-have-same-values? '() '(1)) #f)
(run-test "Test 8" (lists-have-same-values? '(1 2 3) '(1 2 3 4)) #f)
(run-test "Test 9" (lists-have-same-values? '(a b c) '(a b c)) #t)
(run-test "Test 10" (lists-have-same-values? '(1 2 3) '(1 2 "3")) #f)
(run-test "Test 11" (lists-have-same-values? segment1 l1) #t)
