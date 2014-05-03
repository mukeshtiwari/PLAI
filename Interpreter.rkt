#lang plai-typed
(define-type MisspelledAnimal 
  [ caml ( humps  : number ) ]
  [ yacc ( heigth : number ) ] )


(define  (good? [ma : MisspelledAnimal ]) : boolean 
  ( type-case MisspelledAnimal ma 
     [ caml ( h ) (>= h 2 )]
     [ yacc ( h ) (>  h 2 )] ))

(define maone (caml 2 ))
(define matwo (yacc 2.2 ))

;;(test (good? maone ) #t)
;;(test (good? matwo ) #f)

(define-type ArithC 
  [numC  ( n : number )]
  [plusC ( l : ArithC ) ( r : ArithC )]
  [multC ( l : ArithC ) ( r : ArithC )])

(define (parse [s : s-expression ] ) : ArithC 
  (cond
    [(s-exp-number? s) (numC (s-exp->number s))]
    [(s-exp-list? s)
     (let ([sOne (s-exp->list s)])
       (case (s-exp->symbol (first sOne))
         [(+) (plusC (parse (second sOne)) (parse (third sOne)))]
         [(*) (multC (parse (second sOne)) (parse (third sOne)))]
         [else (error 'parse "invalide input" )]))]
     [else (error 'parse "invalid input" )]))


(define (interp [ e : ArithC ]) : number 
  (type-case ArithC e 
    [numC ( n ) n ]
    [plusC (l r) (+ ( interp l ) ( interp r ) )]
    [multC (l r) (* ( interp l ) ( interp r ) )]))
    

(define-type ArithS
  [numS    ( n : number)]
  [plusS   ( l : ArithS ) ( r : ArithS )]
  [minusS  ( l : ArithS ) ( r : ArithS )]
  [multS   ( l : ArithS ) ( r : ArithS )]
  [uminusS ( l : ArithS )])
    
(define (desugar [as : ArithS ] ) : ArithC 
  (type-case ArithS as
    [numS    ( n ) ( numC n )]
    [plusS   (l r) (plusC (desugar l) (desugar r))]
    [multS   (l r) (multC (desugar l) (desugar r))]
    [minusS  (l r) (plusC (desugar l) (multC (numC -1) (desugar r)))]
    [uminusS ( e ) (multC (numC -1) (desugar e))]))


