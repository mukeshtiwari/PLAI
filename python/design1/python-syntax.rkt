#lang plai-typed

(define-type PyExpr
  [PySeq (es : (listof PyExpr))]
  [