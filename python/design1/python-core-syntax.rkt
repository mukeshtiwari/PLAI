#lang plai-typed

(define-type PrimVal
  [VNum (n : number)]
  [VStr (s : string)]
  [VNone]
  [VTrue]
  [VFalse]
  [VList (mutable : boolean) (data : (listof CVal))]
  [VClosure (env : Env) (args : (listof symbol)) (body : CExp)])

(define-type CExp
  [CSeq (eone : CExp) (etwo : CExp)]
  [CError (e : CExp)]
  [CIf (t : CExp) (eone : CExp) (etwo : CExp)]
  [CId (x : symbol)]
  [CLet (x : symbol) (bind : CExp) (body : CExp)]
  [CList (mutable : boolean) (elts : (listof CExp))]
  [CSet! (x : symbol) (e : CExp)]
  [CApp (fun : CExp) (args : (listof CExp))]
  [CFunc (args : (listof symbol)) (body : CExp)]
  [CPrimone (prim : symbol) (arg : CExp)]
  [CPrimtwo (prim : symbol) (left : CExp) (right : CExp)]
  [CPass]
  [CObject (type : PrimVal) (val : PrimVal) (fields : (hashof string CExp))]
  [CSetField (obj : CExp) (field : CExp) (value : CExp)]
  [CGetField (obj : CExp) (field : CExp)])

(define-type CVal
  [VObject (val : PrimVal) (fields : (hashof string CVal))])


(define-type-alias Env (hashof symbol CVal))
  