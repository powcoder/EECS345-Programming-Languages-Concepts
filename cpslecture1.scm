; Tail recursion

; Accumulator Passing Style
; Compute the length of a list
(define len
  (lambda (lis)
    (if (null? lis)
        0
        (+ 1 (len (cdr lis))))))

(define len-acc
  (lambda (lis acc)
    (if (null? lis)
        acc
        (len-acc (cdr lis) (+ 1 acc)))))

; sum the numbers in a list
(define sumnumbers
  (lambda (lis)
    (cond
      ((null? lis) 0)
      ((number? (car lis)) (+ (car lis) (sumnumbers (cdr lis))))
      (else (sumnumbers (cdr lis))))))

(define sumnumbers-acc
  (lambda (lis acc)
    (cond
      ((null? lis) acc)
      ((number? (car lis)) (sumnumbers-acc (cdr lis) (+ (car lis) acc)))
      (else (sumnumbers-acc (cdr lis) acc)))))

(define sumnumbers
  (lambda (lis)
    (sumnumbers-acc lis 0)))

; an accumulator passing version of append
(define myappend-acc
  (lambda (l1 l2 acc)
    (if (null? l1)
        acc
        (myappend-acc (cdr l1) l2 (cons (car l1) acc)))))

(define myappend
  (lambda (l1 l2)
    (myappend l1 l2 l2)))

; Continuation passing style
; sum the numbers in a list
(define sumnumbers-cps
  (lambda (lis return)
    (cond
      ((null? lis) (return 0))
      ((number? (car lis)) (sumnumbers-cps (cdr lis) (lambda (v) (return (+ (car lis) v)))))
      (else (sumnumbers-cps (cdr lis) return)))))

; factorial
(define factorial
  (lambda (n)
    (if (zero? n)
        1
        (* n (factorial (- n 1))))))

(define factorial-cps
  (lambda (n return)
    (if (zero? n)
        (return 1)
        (factorial-cps (- n 1) (lambda (v) (return (* n v)))))))

; append
(define myappend
  (lambda (l1 l2)
    (if (null? l1)
        l2
        (cons (car l1) (myappend (cdr l1) l2)))))

(define myappend-cps
  (lambda (l1 l2 return)
    (if (null? l1)
        (return l2)
        (myappend-cps (cdr l1) l2 (lambda (v) (return (cons (car l1) v)))))))

; removeall
(define removeall
  (lambda (x lis)
    (cond
      ((null? lis) '())
      ((eq? x (car lis)) (removeall x (cdr lis)))
      (else (cons (car lis) (removeall x (cdr lis)))))))

(define removeall-cps
  (lambda (x lis return)
    (cond
      ((null? lis) (return '()))
      ((eq? x (car lis)) (removeall-cps x (cdr lis) return))
      (else (removeall-cps x (cdr lis) (lambda (v) (return (cons (car lis) v))))))))