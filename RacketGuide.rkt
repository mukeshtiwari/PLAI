#lang racket
(require net/base64)

(define pie 3.14)
(define (piece str)
  (substring str 0 3))

(define (bake flavor)
  (printf "pre-heating oven...\n")
  (string-append flavor " pie"))

(define (nobake flavor)
  (string-append flavor " jello"))

(string-append "rope" "twine" "yarn")
(substring "corduroys" 0 4)
(string-length "shoelaces")
(string? "is this string")
(string? 1)
(string? 16)
(sqrt 16)
(sqrt -16)
(- 2 1)
(>= 2 1)
(< 2 1)
(number? 1)
(number? "hello")

(if (> 2 3) 
    "bigger"
    "smaller")

(define (reply s)
  (if (equal? "hello" (substring s 0 5))
      "hi"
      "huh"))
(reply "hello racket")
(reply "λx:(μα.α→α).xx")


(define (reply- s)
  (if (string? s) 
      (reply s)
      "huh?"))



(define (reply-more s)
  (cond
    [(equal? "hello" (substring s 0 5 )) "hi"]
    [(equal? "goodbye" (substring s 0 7)) "bye!"]
    [(equal? "?" (substring s (- (string-length s) 1))) "I don't know"]
    [else "huh?"]))


(reply-more "hello racket")
(reply-more "goodbye cruel world")
(reply-more "what is your favorite color?")
(reply-more "mine is lime green")


(define (double v)
  ( (cond 
      [(string? v) string-append]
      [else +]) v v))


((lambda (s) (string-append s "!")) "hello")
(define (twice f v)
  (f (f v)))
(define (make-add-suffix stwo)
  (lambda (s) (string-append s stwo)))

(twice (make-add-suffix "!") "hello")


(define louder
  (lambda (s) 
    (string-append s "!")))

(define (converse s)
  (define (start? stwo)
    (define len (string-length stwo))
    (and (>= (string-length s) len)
    (equal? stwo (substring s 0 len))))
  (cond
    [(start? "hello") "hi"]
    [(start? "goodbye") "bye"]
    [else "huh?"]))

(converse "hello!")
(converse "urp")

(let 
    ([x (random 4)]
     [o (random 4)])
  (cond
    [(> x o) "X wins"]
    [(> o x) "o wins"]
    [else "game tie"]
    ))


(let* ([x (random 4)]
       [o (random 4)]
       [diff (number->string (abs (- x o)))])
    (cond
     [(> x o) (string-append "X wins by " diff)]
     [(> o x) (string-append "O wins by " diff)]
     [else "cat's game"]))

(list 1 2 3 "hello")
(map sqrt '(1 4 16 25))

(map (lambda (i) 
       (string-append i "!"))
     '("1" "2" "3"))

(map number->string '(1 2 3))

(first '(1 2 3))
(rest '(1 2 3))

empty

(cons 1 empty)
(cons 1 ( cons 2 empty))

(define (reverse-list l)
  (define (reverse-list-internal l s)
    (cond
      [(empty? l) s]
      [else (reverse-list-internal (rest l) (cons (first l) s))]))
  (reverse-list-internal l empty))


(reverse-list '(1 2 3))

(define (my-length l)
  (cond 
    [(empty? l) 0]
    [else (+ 1 (my-length (rest l)))]))

  
(my-length '(1 2 3))

(define (my-map f lst)
  (cond
    [(empty? lst) empty]
    [else (cons (f (first lst)) (my-map f (rest lst)))]))


(my-map sqrt '(1 4 16 25))

(cons 0 (cons 1 2))

(cons 0 (cons 1 (cons 2 '())))

(define (sigma f a b)
  (if (= a b)
      0
      (+ (f a) (sigma f (+ a 1) b))))

(time (round (sigma (lambda (x) (/ 1 x)) 1 2000)))
(time (round (sigma (lambda (x) (/ 1.0 x)) 1 2000)))


(inexact->exact 0.1)
 (= 1/10 0.1)




(require openssl/sha1)

(base64-encode (hex-string->bytes "49276d206b696c6c696e6720796f757220627261696e206c696b65206120706f69736f6e6f7573206d757368726f6f6d"))


(map (lambda (i) (= 1 (modulo (sqr i ) 8))) '(1 2 3 4 5 6 7))





















