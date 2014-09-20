#lang racket
(require math)

(define (break-rsa N e d)
  (define (break-rsa-helper N e d k t g)
    (cond
      [(odd? t) (break-rsa-helper N e d k k (random-integer 2 N))]
      (else 
       (let ([x (modular-expt g (/ t 2) N)])
         (let ([y (gcd (- x 1) N)])
           (cond
             [(and (> x 1) (> y 1)) (cons y (/ N y))]
             (else (break-rsa-helper N e d k (/ t 2) g))))))))
  (let ([k (- (* e d) 1)]) 
    (break-rsa-helper N e d k k (random-integer 2 N))))

(break-rsa 25777 3 16971)
(break-rsa #xa66791dc6988168de7ab77419bb7fb0c001c62710270075142942e19a8d8c51d053b3e3782a1de5dc5af4ebe99468170114a1dfe67cdc9a9af55d655620bbab #x10001 #x123c5b61ba36edb1d3679904199a89ea80c09b9122e1400c09adcf7784676d01d23356a7d44d6bd8bd50e94bfc723fa87d8862b75177691c11d757692df8881)
