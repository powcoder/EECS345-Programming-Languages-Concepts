; This is needed if you do not already have call/cc defined in your Scheme implementation
(define call/cc call-with-current-continuation)

; A multiply that uses tail recursion so we can immediately break
(define multiply-cps
  (lambda (lis return break)
    (cond
      ((null? lis) (return 1))
      ((zero? (car lis)) (break 0))
      (else (multiply-cps (cdr lis) (lambda (v) (return (* (car lis) v))) break)))))

; a multiply that does not use tail recursion but takes in a break continuation
(define multiply
  (lambda (lis break)
    (cond
      ((null? lis) 1)
      ((zero? (car lis)) (break 0))
      (else (* (car lis) (multiply (cdr lis) break))))))

; creates a break continuation that throws away the call stack to this point, and calls multiply with that continuation
(define goodmult
  (lambda (lis)
    (call/cc
     (lambda (break)
       (multiply lis break)))))

;; (indexof 'x '(a b x c d e))  ==> 3
;; (indexof 'x '(a b c d)) ==> -1
(define indexof-helper
  (lambda (x lis break)
    (cond
      ((null? lis) (break -1))
      ((eq? x (car lis)) 0)
      (else (+ 1 (indexof x (cdr lis)))))))

(define indexof
  (lambda (x lis)
    (call/cc
     (lambda (break)
       (indexof-helper x lis break)))))



(define mycont (lambda (v) v))

; a replace all that saves the call stack at the top point into mycont so that we can reaccess that call stack anytime
;  we call mycont
(define replaceall
  (lambda (x y lis)
    (call/cc 
     (lambda (k)
       (cond
         ((null? lis) (begin (set! mycont k) '()))
         ((eq? x (car lis)) (cons y (replaceall x y (cdr lis))))
         (else (cons (car lis) (replaceall x y (cdr lis)))))))))

; a crazy version of append that uses the mycont call stack instead of its own!
(define myappendit
  (lambda (l1 l2)
    (if (null? l1)
        (mycont l2)
        (cons (car l1) (myappendit (cdr l1) l2)))))
             

; An even crazier replaceall from the 2nd lecture.  Saves the point part way through the execution of replaceall*
;  so that if we call mycont, it embeds the input into the location of the first x in the running of replaceall*

(define mycont2 0)

(define replaceall*
  (lambda (x y lis)
    (call/cc
     (lambda (break)
       (cond
         ((null? lis) (begin (if (eq? mycont2 0) (set! mycont2 break)) '()))
         ((eq? x (car lis)) (cons y (replaceall* x y (cdr lis))))
         ((list? (car lis)) (cons (replaceall* x y (car lis)) (replaceall* x y (cdr lis))))
         (else (cons (car lis) (replaceall* x y (cdr lis)))))))))
