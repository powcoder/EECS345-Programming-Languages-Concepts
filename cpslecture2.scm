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

; replaces x with y in the list (no sublists)

; reverse
(define myreverse
  (lambda (lis)
    (if (null? lis)
        '()
        (myappend (myreverse (cdr lis)) (cons (car lis) '())))))

(define myreverse-cps
  (lambda (lis return)
    (if (null? lis)
        (return '())
        (myreverse-cps (cdr lis) (lambda (v) (myappend-cps v (cons (car lis) '()) return))))))

; count all the numbers in a list that contains lists
(define countnums*
  (lambda (lis)
    (cond
      ((null? lis) 0)
      ((list? (car lis)) (+ (countnums* (car lis)) (countnums* (cdr lis))))
      ((number? (car lis)) (+ 1 (countnums* (cdr lis))))
      (else (countnums* (cdr lis))))))

(define countnums*-cps
  (lambda (lis return)
    (cond
      ((null? lis) (return 0))
      ((list? (car lis)) (countnums*-cps (car lis) (lambda (v1) (countnum*-cps (cdr lis) (lambda (v2) (return (+ v1 v2)))))))
      ((number? (car lis)) (countnums*-cps (cdr lis) (lambda (v) (return (+ 1 v)))))
      (else (countnums*-cps (cdr lis) return)))))

; the cps version of split.  call with the initial continution
; (lambda (v1 v2) (list v1 v2))
(define split-cps
  (lambda (lis return)
    (cond
      ((null? lis) (return '() '()))
      ((null? (cdr lis)) (return lis '()))
      (else (split-cps (cddr lis) (lambda (v1 v2) (return (cons (car lis) v1) (cons (cadr lis) v2))))))))

; removeall*
(define removeall*-cps
  (lambda (x lis return)
    (cond
      ((null? lis) (return '()))
      ((list? (car lis)) (removeall*-cps x (car lis) (lambda (v1) (removeall*-cps x (cdr lis) (lambda (v2) (return (cons v1 v2)))))))
      ((eq? (car lis) x) (removeall*-cps x (cdr lis) return))
      (else (removeall*-cps x (cdr lis) (lambda (v) (return (cons (car lis) v))))))))

; (flatten '((a) (((b) (c)) d ((e f)))) g))  => (a b c d e f g)
(define flatten-cps
  (lambda (lis return)
    (cond
      ((null? lis) (return '()))
      ((list? (car lis)) (flatten-cps (car lis) (lambda (v1) (flatten-cps (cdr lis) (lambda (v2) (myappend-cps v1 v2 return))))))
      (else (flatten-cps (cdr lis) (lambda (v) (return (cons (car lis) v))))))))
