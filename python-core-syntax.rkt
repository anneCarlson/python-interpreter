#lang plai-typed

(define-type CExp
  [CUndefined]
  [CNone]
  [CTrue]
  [CFalse]
  [CLocals (l : (listof symbol))]
  [CNum (n : number)]
  [CStr (s : string)]
  [CBox (v : CExp)]
  [CObj (dict : CExp) (class : CExp)]
  [CPrimMap (vals : (listof (CExp * CExp)))] ;; an immutable map
  [CTuple (l : (listof CExp))] ;; need a way to make tuples without
                           ;; applying a primf (which needs a tuple)
  [CPrimF (id : symbol)] ;; primitive functions -- applied like lambdas,
                         ;; defined in racket
  [CSeq (e1 : CExp) (e2 : CExp)]
  [CId (id : symbol)]
  [CLet (id : symbol) (bind : CExp) (body : CExp)]
  [CAddGlobal (id : symbol) (bind : CExp)] ;; hack to let the
                                           ;; primitive runtime know
                                           ;; what the values defined
                                           ;; in lib are (basically hack to put all lib functions in scope before callinganything else)
  [CSet! (id : symbol) (value : CExp)]
  [CMultSet! (id : (listof symbol)) (value : CExp)]
  [CApp (fun : CExp) (args : (listof CExp)) (varargs : CExp)]
  [CFunc (args : (listof symbol)) (vararg : (optionof symbol)) (body : CExp)]
  [CReturn (value : CExp)]
  [CIf (test : CExp) (then : CExp) (else : CExp)]
  [CRaise (type : string) (msg : (listof string))]
  [CTryFinal (try : CExp) (final : CExp)]
  [CTryExcp (try :  CExp) (name : string) (except : CExp) (e : CExp)  (as : symbol)]
  [CList (elts : (listof CExp))]
  [CDict (keys : (listof CExp)) (values : (listof CExp))]
  [CDictM (box : CExp)]
  [CDictLoad (dict : CExp) (key : CExp)]
  [CDictStore (dict : CExp) (key : CExp)]
  [CAssign (to : CExp) (from : CExp)]
  [CSlice (val : CExp) (lower : CExp) (upper : CExp) (step : CExp)]
  [CError (val : CExp)]
  [CClassDef (name : symbol) (super : (listof symbol)) (body : CExp)]
  [CCmp (iter : CExp) (func : CExp)])


(define-type CVal
  [VUndefined];;initial value for variables
  [VNone]
  [VBool (n : number)]
  [VNum (n : number)]
  [VStr (s : string)]
  [VBox (v : Location)];just number
  [VObj (dict : CVal) (class : CVal)]
  [VPrimMap (m : (hashof CVal CVal))]
  [VTuple (l : (listof CVal))]
  [VList (elts : (listof CVal))]
  [VDict (hashes : (hashof CVal CVal))]
  [VDictM (b : CVal)]
  [VClosure (env : Env) (args : (listof symbol)) (vararg : (optionof symbol)) (body : CExp)]
  [VPrimF (id : symbol)])

(define-type-alias Location number)
(define-type-alias Env (hashof symbol Location))
(define-type-alias Store (hashof Location CVal))

