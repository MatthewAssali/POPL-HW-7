#lang plait
#|4/17/23
Homework #7 - Group Project
Mathyo Abou Asali - Razie Hyria |#
;; ============================================================
 
(define-type Value
  (numV [n : Number])
  (boolV [b : Boolean])
  (pairV [fst : Value] ; part 2
         [snd : Value])
  (closV [args : (Listof Symbol)]
         [body : Exp]
         [env : Env]))

(define-type Exp
  (numE [n : Number])
  (trueE) ; part 1
  (falseE) ; part 1
  (ifE [tst : Exp]; part 1
       [thn : Exp]
       [els : Exp])
  (idE [s : Symbol])
  (plusE [l : Exp] 
         [r : Exp])
  (multE [l : Exp]
         [r : Exp])
  (lamE [ns : (Listof Symbol)]
        [arg-types : (Listof Type)]
        [body : Exp])
  (appE [fun : Exp]
        [args : (Listof Exp)])
  (eqE [l : Exp] ; part 1
       [r : Exp])
  ; part 2, adding coverage for pair, first, and second
  (pairE [fst : Exp] 
           [snd : Exp])
  (fstE [pair : Exp]) 
  (sndE [pair : Exp]))

(define-type Type
  (numT)
  (boolT)
  (arrowT [args : (Listof Type)]
          [result : Type])
  ; part 2, adding fst snd pair type coverage
  (crossT [fst : Type]
          [snd : Type]))

(define-type Binding
  (bind [name : Symbol]
        [val : Value]))

(define-type-alias Env (Listof Binding))

(define-type Type-Binding
  (tbind [name : Symbol]
         [type : Type]))

(define-type-alias Type-Env (Listof Type-Binding))


(define mt-env empty)
(define extend-env cons)

;; parse ----------------------------------------
(define (parse [s : S-Exp]) : Exp
  (cond
    [(s-exp-match? `NUMBER s) (numE (s-exp->number s))]
    [(s-exp-match? `true s) (trueE)]                  ;; part 1 added a case for trueE
    [(s-exp-match? `false s) (falseE)]                ;; part 1 added a case for falseE
    [(s-exp-match? `SYMBOL s) (idE (s-exp->symbol s))]
    [(s-exp-match? `{+ ANY ANY} s)
     (plusE (parse (second (s-exp->list s)))
            (parse (third (s-exp->list s))))]
    [(s-exp-match? `{* ANY ANY} s)
     (multE (parse (second (s-exp->list s)))
            (parse (third (s-exp->list s))))]
    [(s-exp-match? `{= ANY ANY} s)           ;; part 1 eqE check if the input matches the = operator
     (eqE (parse (second (s-exp->list s)))   ;; parse the first operand
          (parse (third (s-exp->list s))))]  ;; parse the second operand and create an equal expression
    [(s-exp-match? `{if ANY ANY ANY} s)    ;; part 1 added a case for ifE
     (ifE (parse (second (s-exp->list s)))
            (parse (third (s-exp->list s)))
            (parse (fourth (s-exp->list s))))]
    [(s-exp-match? `{pair ANY ANY} s)          ;; part 2 added a case for pairE
     (pairE (parse (second (s-exp->list s)))
            (parse (third (s-exp->list s))))]
    [(s-exp-match? `{fst ANY} s)                ;; part 2 added a case for fstE
     (fstE (parse (second (s-exp->list s))))]
    [(s-exp-match? `{snd ANY} s)                 ;; part 2 added a case for sndE
     (sndE (parse (second (s-exp->list s))))]   
    [(s-exp-match? `{let {[SYMBOL : ANY ANY]} ANY} s)
     (let ([bs (s-exp->list (first
                             (s-exp->list (second
                                           (s-exp->list s)))))])
       (appE (lamE (list (s-exp->symbol (first bs)))
                   (list (parse-type (third bs)))
                   (parse (third (s-exp->list s))))
             (list (parse (fourth bs)))))]
    [(s-exp-match? `{lambda {[SYMBOL : ANY] ...} ANY} s)
     (let ([args (map s-exp->list
                 (s-exp->list 
                         (second (s-exp->list s))))])
       (lamE (map s-exp->symbol (map first args))
             (map parse-type (map third args))
             (parse (third (s-exp->list s)))))]
    [(s-exp-match? `{ANY ANY ...} s)
     (appE (parse (first (s-exp->list s)))
           (map parse (rest (s-exp->list s))))]
    [else (error 'parse "invalid input")]))

(define (parse-type [s : S-Exp]) : Type
  (cond
   [(s-exp-match? `num s) 
    (numT)]
   [(s-exp-match? `bool s)
    (boolT)]
   [(s-exp-match? `(ANY ... -> ANY) s)
    (arrowT (map parse-type (reverse
                             (rest (rest (reverse (s-exp->list s))))))
            (parse-type (first (reverse (s-exp->list s)))))]
   [(s-exp-match? `(ANY * ANY) s)                 ;; added a type case for crossT
    (crossT (parse-type (first (s-exp->list s)))
            (parse-type (third (s-exp->list s))))]
   [else (error 'parse-type "invalid input")]))

;; interp ----------------------------------------
(define (interp [a : Exp] [env : Env]) : Value
  (type-case Exp a
    [(numE n) (numV n)]
    [(idE s) (lookup s env)]
    [(trueE) (boolV #t)]; part 1
    [(falseE) (boolV #f)]; part 1
    [(plusE l r) (num+ (interp l env) (interp r env))]
    [(multE l r) (num* (interp l env) (interp r env))]
     ;; part 1 eqE evaluate the left side and right-hand side expressions recursively
    ;; then checks whether the two resulting values are equal usingequal? function and returns a boolean value accordingly.
    [(eqE l r) (boolV (equal? (interp l env) (interp r env)))]      
    [(ifE tst thn els)            ;; part 1 added a case for ifE
     (type-case Value (interp tst env)
       [(boolV b) (interp (if b thn els) env)]
       [else (error 'interp "not a boolean")])]
    [(lamE ns arg-types body)
     (closV ns body env)]
   [(appE fun args) (type-case Value (interp fun env)
                       [(closV ns body c-env)
                        (interp body
                                (append
                                 (map2 bind
                                       ns
                                       (map (lambda(arg) (interp arg env)) args))
                                 c-env))]
                       [else (error 'interp "not a function")])]
    [(pairE fst snd) (pairV (interp fst env) (interp snd env))]    ;; part 2 added a case for pairE
    [(fstE pair)         ;; part 2 added a case for fstE                                       
     (type-case Value (interp pair env)
       [(pairV f s) f]
       [else (error 'interp "not a pair")])]
    [(sndE pair)    ;; part 2 added a case for sndE                               
     (type-case Value (interp pair env)
       [(pairV f s) s]
       [else (error 'interp "not a pair")])]))

;; num+ and num* ----------------------------------------
(define (num-op [op : (Number Number -> Number)] [l : Value] [r : Value]) : Value
  (cond
   [(and (numV? l) (numV? r))
    (numV (op (numV-n l) (numV-n r)))]
   [else
    (error 'interp "not a number")]))
(define (num+ [l : Value] [r : Value]) : Value
  (num-op + l r))
(define (num* [l : Value] [r : Value]) : Value
  (num-op * l r))

;; lookup ----------------------------------------
(define (make-lookup [name-of : ('a -> Symbol)] [val-of : ('a -> 'b)])
  (lambda ([name : Symbol] [vals : (Listof 'a)]) : 'b
    (type-case (Listof 'a) vals
      [empty (error 'find "free variable")]
      [(cons val rst-vals) (if (equal? name (name-of val))
                               (val-of (first vals))
                               ((make-lookup name-of val-of) name rst-vals))])))

(define lookup
  (make-lookup bind-name bind-val))
;; typecheck -------------------------------------
(define (typecheck [a : Exp] [tenv : Type-Env])
  (type-case Exp a
    [(lamE ns arg-types body)
     (arrowT arg-types
             (typecheck body 
                        (append (map2 tbind ns arg-types)
                                    tenv)))]
    [(appE fun args)
     (type-case Type (typecheck fun tenv)
       [(arrowT arg-types result-type)
        (if (equal? arg-types
                    (map (lambda (arg) (typecheck arg tenv)) args))
            result-type
            (type-error args
                        (to-string arg-types)))]
       [else (type-error fun "function")])]
    [(numE n) (numT)]
    [(trueE) (boolT)] ;; part 1
    [(falseE) (boolT)] ;; part 1
    [(pairE f s) ;; part 2
     (crossT (typecheck f tenv) (typecheck s tenv))]
    [(fstE pair) ;; part 2
     (type-case Type (typecheck pair tenv)
       [(crossT f s) f]
       [else (type-error pair "pair")])]
    [(sndE pair) ;; part 2
     (type-case Type (typecheck pair tenv)
       [(crossT f s) s]
       [else (type-error pair "pair")])]
    [(plusE l r) ;; part 1 changed plusE because changes has been made to the typecheck-nums the plus expression has type numT if both operands have type numT
     (type-case Type (typecheck l tenv)
       [(numT) 
        (type-case Type (typecheck r tenv)
          [(numT) (numT)]
          [else (type-error r "num")])]
       [else (type-error l "num")])]
    [(multE l r)  ;; the multiplication expression has type numT if both operands have type numT
     (type-case Type (typecheck l tenv)
       [(numT) 
        (type-case Type (typecheck r tenv)
          [(numT) (numT)]
          [else (type-error r "num")])]
       [else (type-error l "num")])]
    [(ifE tst thn els)
     (type-case Type (typecheck tst tenv)
       [(boolT) 
        (local [(define thnt (typecheck thn tenv))
                (define elst (typecheck els tenv))]
          (if (equal? thnt elst)
              thnt
              (type-error thnt (to-string elst))))]
       [else (type-error tst "boolean")])]
    [(eqE l r) (typecheck-nums l r tenv)]   ; typecheck the operands as numbers and return a boolean type
    [(idE n) (type-lookup n tenv)]))


(define (typecheck-nums l r tenv)   ;; changed this function for part 2 to produce a boolean if it gives two numbers
  (type-case Type (typecheck l tenv)
    ; If the left operand is of type `numT`, then type check the right operand `r`
    [(numT)
     (type-case Type (typecheck r tenv)
       ; If the right operand is also of type `numT`, then the expression is of type `boolT`
       [(numT) (boolT)]
       ; Otherwise, throw a type error indicating that the right operand should be of type `num`
       [else (type-error r "num")])]
    ; If the left operand is not of type `numT`, then throw a type error indicating that the left operand should be of type `num`
    [else (type-error l "num")]))


(define (type-error a msg)
  (error 'typecheck (string-append
                     "no type: "
                     (string-append
                      (to-string a)
                      (string-append " not "
                                     msg)))))

(define type-lookup
  (make-lookup tbind-name tbind-type))

;; module test-----------------------------------------
(module+ test
  (test (typecheck (parse `10) mt-env)
        (numT))
  (test (typecheck (parse `{+ 10 17}) mt-env)
        (numT))
  (test (typecheck (parse `{* 10 17}) mt-env)
        (numT))
  (test (typecheck (parse `{lambda {[x : num]} 12}) mt-env)
        (arrowT (list (numT)) (numT)))
  (test (typecheck (parse `{lambda {[x : num]} {lambda {[y : bool]} x}}) mt-env)
        (arrowT (list (numT)) (arrowT (list (boolT))  (numT))))

  (test (typecheck (parse `{{lambda {[x : num]} 12}
                            {+ 1 17}})
                   mt-env)
        (numT))

  (test (typecheck (parse `{let {[x : num 4]}
                             {let {[f : (num -> num)
                                      {lambda {[y : num]} {+ x y}}]}
                               {f x}}})
                   mt-env)
        (numT))

  (test/exn (typecheck (parse `{1 2})
                       mt-env)
            "no type")
  (test/exn (typecheck (parse `{{lambda {[x : bool]} x} 2})
                       mt-env)
            "no type")
  (test/exn (typecheck (parse `{+ 1 {lambda {[x : num]} x}})
                       mt-env)
            "no type")
  (test/exn (typecheck (parse `{* {lambda {[x : num]} x} 1})
                       mt-env)
            "no type"))
(module+ test
  (test/exn (lookup 'x mt-env)
            "free variable")
  (test (lookup 'x (extend-env (bind 'x (numV 8)) mt-env))
        (numV 8))
  (test (lookup 'x (extend-env
                    (bind 'x (numV 9))
                    (extend-env (bind 'x (numV 8)) mt-env)))
        (numV 9))
  (test (lookup 'y (extend-env
                    (bind 'x (numV 9))
                    (extend-env (bind 'y (numV 8)) mt-env)))
        (numV 8)))
(module+ test
  (test (num+ (numV 1) (numV 2))
        (numV 3))
  (test (num* (numV 2) (numV 3))
        (numV 6)))
(module+ test
  (test (interp (parse `2) mt-env)
        (numV 2))
  (test/exn (interp (parse `x) mt-env)
            "free variable")
  (test (interp (parse `x) 
                (extend-env (bind 'x (numV 9)) mt-env))
        (numV 9))
  (test (interp (parse `{+ 2 1}) mt-env)
        (numV 3))
  (test (interp (parse `{* 2 1}) mt-env)
        (numV 2))
  (test (interp (parse `{+ {* 2 3} {+ 5 8}})
                mt-env)
        (numV 19))
  (test (interp (parse `{lambda {[x : num]} {+ x x}})
                mt-env)
        (closV (list 'x) (plusE (idE 'x) (idE 'x)) mt-env))
  (test (interp (parse `{let {[x : num 5]}
                          {+ x x}})
                mt-env)
        (numV 10))
  (test (interp (parse `{let {[x : num 5]}
                          {let {[x : num {+ 1 x}]}
                            {+ x x}}})
                mt-env)
        (numV 12))
  (test (interp (parse `{let {[x : num 5]}
                          {let {[y : num 6]}
                            x}})
                mt-env)
        (numV 5))
  (test (interp (parse `{{lambda {[x : num]} {+ x x}} 8})
                mt-env)
        (numV 16))

  (test/exn (interp (parse `{1 2}) mt-env)
            "not a function")
  (test/exn (interp (parse `{+ 1 {lambda {[x : num]} x}}) mt-env)
            "not a number")
  (test/exn (interp (parse `{let {[bad : (num -> num) {lambda {[x : num]} {+ x y}}]}
                              {let {[y : num 5]}
                                {bad 2}}})
                    mt-env)
            "free variable"))
(module+ test
  (test (parse `2)
        (numE 2))
  (test (parse `x)
        (idE 'x))
  (test (parse `{+ 2 1})
        (plusE (numE 2) (numE 1)))
  (test (parse `{* 3 4})
        (multE (numE 3) (numE 4)))
  (test (parse `{+ {* 3 4} 8})
        (plusE (multE (numE 3) (numE 4))
               (numE 8)))
  (test (parse `{let {[x : num {+ 1 2}]}
                  y})
        (appE (lamE (list 'x) (list (numT)) (idE 'y))
              (list (plusE (numE 1) (numE 2)))))
  (test (parse `{lambda {[x : num]} 9})
        (lamE (list'x) (list (numT)) (numE 9)))
  (test (parse `{double 9})
        (appE (idE 'double) (list(numE 9))))
  (test/exn (parse `{{'()}})
            "invalid input")

  (test (parse-type `num)
        (numT))
  (test (parse-type `bool)
        (boolT))
  (test (parse-type `(num -> bool))
        (arrowT (list (numT)) (boolT)))
  (test/exn (parse-type `1)
            "invalid input"))
(module+ test
  (print-only-errors #t))

;; part 1 test cases----------------------------------------
(test (interp (parse `{if true 4 5})
              mt-env)
      (numV 4))
(test (interp (parse `{if false 4 5})
              mt-env)
      (numV 5))
(test (interp (parse `{if {= 13 {if {= 1 {+ -1 2}}
                                    12
                                    13}}
                          4
                          5})
              mt-env)
      (numV 5))
(test (typecheck (parse `{= 13 {if {= 1 {+ -1 2}}
                                   12
                                   13}})
                 mt-env)
      (boolT))
(test (typecheck (parse `{if {= 1 {+ -1 2}}
                             {lambda {[x : num]} {+ x 1}}
                             {lambda {[y : num]} y}})
                 mt-env)

      (arrowT (list (numT)) (numT)))
(test/exn (typecheck (parse `{+ 1 {if true true false}})
                     mt-env)
          "no type")
(test/exn (interp (parse `{if 1 2 3}) mt-env)
          "not a boolean")
(test/exn (typecheck (parse `{+ 1 {if true false 2}})
                     mt-env)
          "no type")
;; part 2 test cases----------------------------------------
(test (interp (parse `{pair 10 8})
              mt-env)
;; Your constructor might be different than pairV:
      (pairV (numV 10) (numV 8)))
(test (interp (parse `{fst {pair 10 8}})
              mt-env)
      (numV 10))
(test (interp (parse `{snd {pair 10 8}})
              mt-env)
      (numV 8))
(test (interp (parse `{let {[p : (num * num) {pair 10 8}]}
                        {fst p}})
              mt-env)
      (numV 10))
(test (typecheck (parse `{pair 10 8})
                 mt-env)
;; Your constructor might be different than crossT:
      (crossT (numT) (numT)))
(test (typecheck (parse `{fst {pair 10 8}})
                 mt-env)
      (numT))
(test (typecheck (parse `{+ 1 {snd {pair 10 8}}})
                 mt-env)
      (numT))
(test (typecheck (parse `{lambda {[x : (num * bool)]}
                           {fst x}})
                 mt-env)
;; Your constructor might be different than crossT:
      (arrowT (list (crossT (numT) (boolT))) (numT)))
(test (typecheck (parse `{{lambda {[x : (num * bool)]}
                            {fst x}}
                          {pair 1 false}})
                 mt-env)
      (numT))
(test (typecheck (parse `{{lambda {[x : (num * bool)]}
                            {snd x}}
                          {pair 1 false}})
                 mt-env)
      (boolT))
(test/exn (typecheck (parse `{fst 10})
                     mt-env)
          "no type")
(test/exn (typecheck (parse `{snd 10})
                     mt-env)
          "no type")

(test/exn (typecheck (parse `{+ 1 {fst {pair false 8}}})
                     mt-env)
          "no type")
(test/exn (typecheck (parse `{lambda {[x : (num * bool)]}
                               {if {fst x}
                                   1
                                   2}})
                     mt-env)
          "no type")
(test/exn (interp (parse `{fst {fst 1}})
              mt-env)
      "not a pair")
(test/exn (interp (parse `{snd {snd 1}})
              mt-env)
      "not a pair")
;; part 3 test cases----------------------------------------
(test (interp (parse `{{lambda {}
                         10}})
              mt-env)
      (numV 10))
(test (interp (parse `{{lambda {[x : num] [y : num]} {+ x y}}
                       10
                       20})
              mt-env)
      (numV 30))
(test (typecheck (parse `{{lambda {[x : num] [y : bool]} y}
                          10
                          false})
                 mt-env)
      (boolT))
(test/exn (typecheck (parse `{{lambda {[x : num] [y : bool]} y}
                              false
                              10})
                     mt-env)
          "no type")
(test/exn (typecheck (parse `(* 2 true))
                     mt-env)
          "no type")

(test/exn (typecheck (parse `{+ {lambda {[x : bool]} x} 2})
                     mt-env)
          "no type")

(test/exn (typecheck-nums (parse `(+ 1 true)) (parse `(+ 1 2)) mt-env)
          "no type")
(test/exn (typecheck (parse `(= false 10)) mt-env) "num")
(test/exn (typecheck (parse `(= 10 false)) mt-env) "num")
