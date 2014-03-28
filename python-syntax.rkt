#lang plai-typed

(define-type PyExp
  [PyNum (n : number)]
  [PyTuple (l : (listof PyExp))]
  [PySeq (es : (listof PyExp))]
  [PyId (x : symbol)]
  [PyGlobal (x : symbol)]
  [PySet! (id : symbol) (value : PyExp)]
  [PyMultSet! (id : (listof symbol)) (value : PyExp)]
  [PyApp (fun : PyExp) (args : (listof PyExp)) (starargs : PyExp)]
  [PyFunc (args : (listof symbol)) (vararg : (optionof symbol)) (body : PyExp)]
  [PyReturn (value : PyExp)]
  [PyIf (test : PyExp) (then : PyExp) (else : PyExp)]
  [PyOp (id : symbol) (args : (listof PyExp))]
  [PyRaise (type : string) (message :(listof string))]
  [PyStr (s : string)]
  [PyComp (ops : (listof symbol)) (l : PyExp) (c : (listof PyExp))]
  [PyTryFinal (try : PyExp) (final : PyExp)]
  [PyTryExcp (try : PyExp) (except : PyExp) (e : PyExp) ]
  [PyExcept (type : PyExp) (as : symbol) (body : PyExp)]
  [PyList (elts : (listof PyExp))]
  [PyDict (keys : (listof PyExp)) (values : (listof PyExp))]
  [PyAssign (to : PyExp) (from : PyExp)]
  [PyDictLoad (dict : PyExp) (ind : PyExp)]
  [PyDictStore (dict : PyExp) (ind : PyExp)]
  [PySlice (val : PyExp) (lower : PyExp) (upper : PyExp) (step : PyExp)]
  [PyCmp (id : symbol) (iter : PyExp) (func : PyExp)]
  [PyNonLocal (id : symbol)]
  [PyClassDef (name : symbol) (super : (listof symbol)) (body : PyExp)]
  [PyFieldLookup (field : string) (value : PyExp)]
  [PyPass])

(define someF some)
(define noneF none)

