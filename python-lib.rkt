#lang plai-typed

(require "python-core-syntax.rkt")

#|

Here is a suggestion for how to implement shared runtime functionality -
write it as core expression forms and use python-lib to wrap your
desugared expressions in an environment that will contain useful
bindings.  For example, this sample library binds `print` to a function
that calls the primitive `print`.

|#
#|(define comprehension
  (CFunc (list 'x 'l 'f)
         (none)
         
         )|#
(define make-bool
  (CFunc empty
         (some 'f)
         (CIf (CApp (CApp (CPrimF 'class-lookup)
                          (list (CApp (CApp (CPrimF 'class-lookup)
                                            (list (CId 'f)
                                                  (CStr "__len__"))
                                            (CTuple empty))
                                      empty
                                      (CTuple empty))
                                (CStr "=")) (CTuple empty))
                    (list (CNum 0))
                    (CTuple empty))
              (CReturn (CFalse))
              (CIf (CApp (CApp (CPrimF 'class-lookup)
                               (list (CApp (CPrimF 'get-tuple-val)
                                           (list (CId 'f)
                                                 (CNum 0))
                                           (CTuple empty))
                                     (CStr "__bool__"))
                               (CTuple empty))
                         empty
                         (CTuple empty))
                   (CReturn (CTrue))
                   (CReturn (CFalse)))
              )))
         
  
 #| (CFunc empty
         (some 'f)
         (CIf (CApp (CApp (CPrimF 'class-lookup)
                          (list (CId 'f)
                                (CStr "__bool__"))
                          (CTuple empty))
                    empty
                    (CTuple empty))
              (CReturn (CTrue))
              (CReturn (CFalse)))
              ))|#

(define assert-false
  (CFunc (list 'f)
         (none)
         (CIf (CApp (CApp (CPrimF 'class-lookup)
                          (list (CId 'f)
                                (CStr "__bool__"))
                          (CTuple empty))
                    empty
                    (CTuple empty))
              (CError (CStr "assertion failed"))
              (CNone)
              )))

     
(define assert-true
  (CFunc (list 'f)
         (none)
         (CIf (CApp (CApp (CPrimF 'class-lookup)
                          (list (CId 'f)
                                (CStr "__bool__"))
                          (CTuple empty))
                    empty
                    (CTuple empty))
              (CNone)
              (CError (CStr "assertion failed")))))

(define fail
  (CFunc (list 'f)
         (none)
             (CError (CId 'f))))

(define assert-equal
  (CFunc (list 'a 'b)
         (none)
         (CIf (CApp (CPrimF 'equal)
                    (list (CId 'a)
                          (CId 'b))
                    (CTuple empty))
              (CNone)
              (CError (CStr "assertion failed")))))
(define assert-not-equal
    (CFunc (list 'a 'b)
         (none)
         (CIf (CApp (CPrimF 'equal)
                    (list (CId 'a)
                          (CId 'b))
                    (CTuple empty))
              (CError (CStr "assertion failed"))
              (CNone))))
(define assert-not-is
    (CFunc (list 'a 'b)
         (none)
         (CIf (CApp (CPrimF 'is)
                    (list (CId 'a)
                          (CId 'b))
                    (CTuple empty))
              (CError (CStr "assertion failed"))
              (CNone))))
(define assert-is
  (CFunc (list 'a 'b)
         (none)
         (CIf (CApp (CPrimF 'is)
                    (list (CId 'a)
                          (CId 'b))
                    (CTuple empty))
              (CNone)
              (CError (CStr "assertion failed")))))
(define assert-in
  (CFunc (list 'a 'b)
         (none)
         (CIf (CApp (CPrimF 'in)
                    (list (CId 'b)
                          (CId 'a))
                    (CTuple empty))
              (CNone)
              (CError (CStr "assertion failed")))))
(define assert-not-in
  (CFunc (list 'a 'b)
         (none)
         (CIf (CApp (CPrimF 'in)
                    (list (CId 'b)
                          (CId 'a))
                    (CTuple empty))
              (CError (CStr "assertion failed"))
              (CNone)
              )))

(define assert-raises 
  (CFunc (list 'a)
         (none)
          ;(CTryExcp 
         (CId 'a)
                    ;"ExceptAll"
                    ;(CNone)
         ;(CError (CStr "assertion failed")))
         ))

  
(define partial-apply
  (CAddGlobal;create global scope
   'partial-apply
   (CFunc (list 'func)
          (some 'args)
                 (CReturn
           (CFunc empty
                  (some 'more-args)
                  (CReturn
                   (CApp (CId 'func)
                         empty
                         (CApp (CPrimF 'tuple-append)
                               (list (CId 'args) (CId 'more-args))
                               (CTuple empty)))))))))

(define class-type
  (CAddGlobal
   'class-type
   (CLet 'class-box (CBox (CUndefined))
         (CApp (CPrimF 'set-box)
               (list (CId 'class-box)
                     (CObj 
                      (CBox (CPrimMap 
                             (map (lambda (pair)
                                    (local [(define-values (name val) pair)]
                                      (values (CStr name) val))) 
                                  ;;methods for all objects
                                  (list
                                   (values "__call__"
                                           (CPrimF 'constructor)))
                                  )))
                      (CId 'class-box)))
               (CTuple empty)))))


(define-syntax-rule (make-type id fs)
  (define id
    (CAddGlobal
     'id
     (CObj (CBox (CPrimMap (map (lambda (pair)
                                  (local [(define-values (name val) pair)]
                                         (values (CStr name) val))) fs)))
           (CBox (CId 'type))))))

(make-type none-type
           (list (values "__bool__"
                         (CFunc (list 'this)
                                (none)
                                (CReturn (CFalse))))
                 (values "="
                         (CPrimF 'none-eq))
                 (values "is"
                         (CPrimF 'is))
                 ))

(make-type bool-type
           (list (values "__bool__"
                         (CFunc (list 'this)
                                (none)
                                (CReturn (CId 'this))))
                 
                 (values "__add__"
                         (CPrimF 'int-add))
                 (values "__sub__"
                         (CPrimF 'int-sub))
                 (values "__mult__"
                         (CPrimF 'int-mult))
                 (values "__div__"
                         (CPrimF 'int-div))
                 (values "__neg__"
                         (CPrimF 'int-neg))
                 (values "__pls__"
                         (CPrimF 'int-pls))
                 (values "__inv__"
                         (CPrimF 'int-inv))
                 (values ">"
                         (CPrimF 'int-gt))
                 (values ">="
                         (CPrimF 'int-gte))   
                 (values "="
                         (CPrimF 'int-eq))
                 (values "is"
                         (CPrimF 'is))
                 (values "absv"
                         (CPrimF 'absv))
                 ))


(make-type num-type
           (list (values "__bool__"
                         (CFunc (list 'this)
                                (none)
                                (CIf (CApp (CPrimF 'int-eq)
                                           (list (CId 'this)
                                                 (CNum 0))
                                           (CTuple empty))
                                     (CReturn (CFalse))
                                     (CReturn (CTrue)))))
                 (values "__add__"
                         (CPrimF 'int-add))
                 (values "__sub__"
                         (CPrimF 'int-sub))
                 (values "__mult__"
                         (CPrimF 'int-mult))
                 (values "__div__"
                         (CPrimF 'int-div))
                 (values "__div-floor__"
                         (CPrimF 'div-floor))
                 (values "__mod__"
                         (CPrimF 'mod))
                 (values "__neg__"
                         (CPrimF 'int-neg))
                 (values "__pls__"
                         (CPrimF 'int-pls))
                 (values "__inv__"
                         (CPrimF 'int-inv))
                 (values ">"
                         (CPrimF 'int-gt))
                 (values ">="
                         (CPrimF 'int-gte))   
                 (values "="
                         (CPrimF 'int-eq))
                 (values "is"
                         (CPrimF 'is))
                 (values "absv"
                         (CPrimF 'absv))
                 (values "str"
                         (CPrimF 'str))
                 ))

(make-type str-type
           (list (values "__bool__"
                         (CFunc (list 'this)
                                (none)
                                (CIf (CApp (CPrimF 'str-eq)
                                           (list (CId 'this)
                                                 (CStr ""))
                                           (CTuple empty))
                                     (CReturn (CFalse))
                                     (CReturn (CTrue)))))
                 (values "__add__"
                         (CPrimF 'str-add))

                 (values "__mult__"
                         (CPrimF 'str-mult))
                 (values ">"
                         (CPrimF 'str-gt))
                 (values ">="
                         (CPrimF 'str-gte))   
                 (values "="
                         (CPrimF 'str-eq)) 
                 (values "in"
                         (CPrimF 'in))
                 (values "tuple"
                         (CPrimF 'tup))
                 (values "min-f"
                         (CPrimF 'min-f))
                 (values "max-f"
                         (CPrimF 'max-f))
                 (values "is"
                         (CPrimF 'is))));(11/15)now basically everything-mult

(make-type func-type
           (list
            (values "="
                         (CPrimF 'equal))
            (values "is"
                         (CPrimF 'is))))

(make-type obj-type
           (list (values "__bool__"
                         (CFunc (list 'this)
                                (none)
                                (CReturn
                                 (CIf (CApp (CPrimF 'class-has-member?)
                                            (list (CId 'this)
                                                  (CStr "__len__"))
                                            (CTuple empty))
                                      (CIf (CApp (CPrimF 'equal)
                                                 (list (CApp (CApp (CPrimF 'class-lookup)
                                                                   (list (CId 'this)
                                                                         (CStr "__len__"))
                                                                   (CTuple empty))
                                                             empty
                                                             (CTuple empty))
                                                       (CNum 0))
                                                 (CTuple empty))
                                           (CFalse)
                                           (CTrue))
                                      (CTrue)))))
                 (values "="
                         (CPrimF 'equal))
                 (values "is"
                         (CPrimF 'is))))

(make-type tuple-type
           (list (values "__bool__"
                         (CFunc (list 'this)
                                (none)
                                (CIf (CApp (CPrimF 'equal)
                                           (list (CId 'this)
                                                 (CTuple (list)))
                                           (CTuple empty))
                                     (CReturn (CFalse))
                                     (CReturn (CTrue)))))
                 (values "__len__"
                         (CPrimF 'tuple-length))
                 (values "__add__"
                         (CPrimF 'tuple-append))
                 (values "__mult__"
                         (CPrimF 'tuple-mult))
                 (values "="
                         (CPrimF 'equal))
                 (values "is"
                         (CPrimF 'is))
                 (values "tuple"
                         (CPrimF 'tup))
                 ))
(make-type list-type
           (list (values "__bool__"
                         (CFunc (list 'this)
                                (none)
                                (CIf (CApp (CPrimF 'equal)
                                           (list (CId 'this)
                                                 (CList (list)))
                                           (CTuple empty))
                                     (CReturn (CFalse))
                                     (CReturn (CTrue)))))
                 (values "__len__"
                         (CPrimF 'list-length))
                 (values "__add__"
                         (CPrimF 'list-append))
                 (values "__mult__"
                         (CPrimF 'list-mult))
                 (values "__bitand__"
                         (CPrimF 'bit-and))
                 (values "__bitor__"
                         (CPrimF 'bit-or))
                 (values "__bitxor__"
                         (CPrimF 'bit-xor))
                 (values "__sub__"
                         (CPrimF 'bit-sub))
                 (values "="
                         (CPrimF 'equal))
                 (values "is"
                         (CPrimF 'is))
                 (values "tuple"
                         (CPrimF 'tup))
                 (values "in"
                         (CPrimF 'in))
                 ))

(make-type mutable-dict-type
           (list (values "__bool__"
                         (CFunc (list 'this)
                                (none)
                                (CIf (CApp (CPrimF 'equal)
                                           (list (CId 'this)
                                                 (CDictM (CBox (CDict empty empty))))
                                           (CTuple empty))
                                     (CReturn (CFalse))
                                     (CReturn (CTrue)))))
                 (values "__len__"
                         (CPrimF 'dict-length))
                 (values "="
                         (CPrimF 'equal))
                 (values "in"
                         (CPrimF 'in))
                 (values "del"
                         (CPrimF 'del))
                 (values "is"
                         (CPrimF 'is))
                 (values "items" 
                         (CPrimF 'items))
                 (values "clear" 
                         (CPrimF 'clear))
                 (values "values" 
                         (CPrimF 'value))
                 (values "update" 
                         (CPrimF 'update))
                 (values "keys" 
                         (CPrimF 'keys))
                 (values "get" 
                         (CPrimF 'get))
                 (values "set" 
                         (CPrimF 'list-f))))  

(define lib-binds;put all default vals/keywords
  (list
   (values 'print (CPrimF 'print))
   (values 'isinstance (CPrimF 'isinstance))
   (values 'get (CPrimF 'get))
   (values 'min (CPrimF 'min-f))
   (values 'max (CPrimF 'max-f))
   (values 'LIST (CPrimF 'list-f))
   (values 'set (CPrimF 'list-f))
   (values 'items (CPrimF 'items))
   (values 'clear (CPrimF 'clear))
   (values 'values (CPrimF 'value))
   (values 'update (CPrimF 'update))
   (values 'keys (CPrimF 'keys))
   (values 'len (CPrimF 'gen-length))
   (values 'any (CPrimF 'any))
   (values 'all (CPrimF 'all))
   (values 'callable (CPrimF 'callable))
   (values 'filter (CPrimF 'filter))
   (values 'range (CPrimF 'range))
   (values 'True (CTrue))
   (values 'False (CFalse))
   (values 'None (CNone))
   (values '___assertRaises assert-raises)
   (values '___assertTrue assert-true)
   (values '___assertFalse assert-false)
   (values '___assertEqual assert-equal)
   (values '___assertNotEqual assert-not-equal)
   (values '___assertIs assert-is)
   (values '___assertIn assert-in)
   (values '___assertNotIn assert-not-in)
   (values '___assertIsNot assert-not-is)
   (values '___fail fail)
   ;(values 'any any
   (values 'abs (CPrimF 'absv))
   (values 'str (CPrimF 'str))
   (values 'type class-type)
   (values 'bool bool-type)
   (values 'int num-type)
   (values 'string str-type)
   (values 'tuple tuple-type)
   (values 'list list-type)
   (values 'dict mutable-dict-type)
   (values 'int (CPrimF 'int))
   (values 'float (CPrimF 'float))
   (values 'BOOL make-bool)
   ;(values 'BOOL (CPrimF 'bool))
   (values 'tuple (CPrimF 'tup))
   (values 'object obj-type)))

(define lib-exprs
  (list
   partial-apply
   none-type
   func-type))


(define (python-lib expr)
  (foldr (lambda (pair expr)
           (local [(define-values (name value) pair)]
                  (CLet name value
                        expr)))
         (foldr CSeq
                expr
                lib-exprs);add all the lib func to a CSeq + return
         lib-binds))
#|
(foldr CSeq (CApp (CPrimF 'int-add) (list (CNum 1) (CNum 1)) (CNone)) lib-exprs)



 (foldr CSeq (CApp (CPrimF 'int-add) (list (CNum 1) (CNum 1)) (CNone) lib-exprs)

->

(CSeq partial-apply (CSeq none-type (CSeq func-type CSeq

->


|#



