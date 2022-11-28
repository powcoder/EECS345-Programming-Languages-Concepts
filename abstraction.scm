; A function computing the denotational semantics for the value of an expression.
;  The function uses abstraction for the operand and operator locations in the expression
;  The abstraction makes the code easier to read and easier to modify should we need to
;  change expressions between infix, prefix, or postfix notation
(define M_value_int
  (lambda (expression)
    (cond
      ((number? expression) expression)
      ((eq? '+ (operator expression)) (+ (M_value_int (operand1 expression)) (M_value_int (operand2 expression))))
      ((eq? '- (operator expression)) (- (M_value_int (operand1 expression)) (M_value_int (operand2 expression))))
      ((eq? '* (operator expression)) (* (M_value_int (operand1 expression)) (M_value_int (operand2 expression))))
      ((eq? '/ (operator expression)) (quotient (M_value_int (operand1 expression)) (M_value_int (operand2 expression))))
      ((eq? '% (operator expression)) (remainder (M_value_int (operand1 expression)) (M_value_int (operand2 expression))))
      (else (error 'badoperator "illegal arithmetic expression")))))

; Here is one way to define the abstraction function
(define operator 
  (lambda (expression)
     (cadr expression)

; And here is another equivalent way
(define operand1 car)
(define operand2 caddr)
      
