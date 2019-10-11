#lang plai-typed

#|
 | Funções simples
 | Um único argumento, que será representado por um symbol
 |
 |#

; Novo tipo, com funções.
; Precisamos de duas novas entradas: 
;     - identificador, para argumentos
;     - aplicação da função
(define-type ExprC
  [numC (n : number)]
  [idC  (s : symbol)]  ; identificador
  [appC (fun : symbol) (arg : ExprC)] ; aplicação, com o nome da função e o valor do argumento
  [plusC (l : ExprC) (r : ExprC)]
  [multC (l : ExprC) (r : ExprC)]
  [divC (l : ExprC) (r : ExprC)]
  [ifC   (condição : ExprC) (sim : ExprC) (não : ExprC)]
  )

; definição de função com 1 argumento
(define-type FunDefC
  [fdC (name : symbol) (arg : symbol) (body : ExprC)]
  )


; inclui funções
(define-type ExprS
  [numS    (n : number)]
  [idS     (s : symbol)] 
  [appS    (fun : symbol) (arg : ExprS)] 
  [plusS   (l : ExprS) (r : ExprS)]
  [bminusS (l : ExprS) (r : ExprS)]
  [uminusS (e : ExprS)]
  [multS   (l : ExprS) (r : ExprS)]
  [divS    (l : ExprS) (r : ExprS)]
  [ifS     (c : ExprS) (s : ExprS) (n : ExprS)]
  )

; agora é preciso arrumar desugar interpretador e parser.

(define (desugar [as : ExprS]) : ExprC  
  (type-case ExprS as
    [numS    (n) (numC n)]
    [idS     (s) (idC s)] ; este é fácil
    [appS    (fun arg) (appC fun (desugar arg))] ; fun é um symbol, não precisa de desugar 
    [plusS   (l r) (plusC (desugar l) (desugar r))] 
    [multS   (l r) (multC (desugar l) (desugar r))]
    [divS    (l r) (divC (desugar l) (desugar r))]
    [bminusS (l r) (plusC (desugar l) (multC (numC -1) (desugar r)))]
    [uminusS (e)   (multC (numC -1) (desugar e))]
    [ifS     (c s n) (ifC (desugar c) (desugar s) (desugar n))]
    ))

#|
 | O interpretador precisa de uma lista adicional de definições para os símbolos
 |#

(define-type Binding
      [bind (name : symbol) (val : number)])

; A lista de associações é o environment
(define-type-alias Env (listof Binding))
(define mt-env empty)        ; ente pronunciar "mt" em inglês e compare com "empty"
(define extend-env cons)     ; sorte, cons faz exatamente o que queremos para estender o env


; lookup
(define (lookup [for : symbol] [env : Env]) : number
       (cond
            [(empty? env) (error 'lookup "name not found")] ; livre (não definida)
            [else (cond
                  [(symbol=? for (bind-name (first env)))   ; achou!
                                 (bind-val (first env))]
                  [else (lookup for (rest env))])]))        ; vê no resto

; Agora o interpretador!


; interp: todas as chamadas recursivas devem levar em conta o environment
(define (interp [a : ExprC] [env : Env] [fds : (listof FunDefC)]) : number
  (type-case ExprC a
    [numC (n) n]
    [idC (n) (lookup n env)]

    [appC (f a) 
          (local ([define fd (get-fundef f fds)]) 
            (interp (fdC-body fd)
                    (extend-env 
                        (bind (fdC-arg fd) (interp a env fds))
                        mt-env)              ; novo environment feito do zero!
                    fds))]
    [plusC (l r) (+ (interp l env fds) (interp r env fds))]
    [multC (l r) (* (interp l env fds) (interp r env fds))]
    [divC  (l r) (/ (interp l env fds) (interp r env fds))]
    [ifC (c s n) (if (zero? (interp c env fds)) (interp n env fds) (interp s env fds))]
    ))

; get-fundef
(define (get-fundef [n : symbol] [fds : (listof FunDefC)]) : FunDefC
  (cond
    [(empty? fds) (error 'get-fundef "referência para função não definida")]
    [(cons? fds) (cond
                   [(equal? n (fdC-name (first fds))) (first fds)] ; achou!
                   [else (get-fundef n (rest fds))] ; procura no resto
                   )]))


; o parser precisa tratar de chamadas
(define (parse [s : s-expression]) : ExprS
  (cond
    [(s-exp-number? s) (numS (s-exp->number s))]
    [(s-exp-list? s)
     (let ([sl (s-exp->list s)])
       (case (s-exp->symbol (first sl))
         [(+) (plusS (parse (second sl)) (parse (third sl)))]
         [(*) (multS (parse (second sl)) (parse (third sl)))]
         [(/) (divS  (parse (second sl)) (parse (third sl)))]
         [(-) (bminusS (parse (second sl)) (parse (third sl)))]
         [(~) (uminusS (parse (second sl)))]
         [(call) (appS (s-exp->symbol (second sl)) (parse (third sl)))]
         [(if) (ifS (parse (second sl)) (parse (third sl)) (parse (fourth sl)))]
         [else (error 'parse "invalid list input")]))]
    [else (error 'parse "invalid input")]))

#|
 | Nesta linguagem, as funções são pré-definidas
 | Vejamos dobro, quadrado e fatorial
 |#
(define biblioteca (list 
                    [fdC 'dobro 'x (plusC (idC 'x) (idC 'x))]
                    [fdC 'quadrado 'y (multC (idC 'y) (idC 'y))]
                    [fdC 'fatorial 'n (ifC  (idC 'n) 
						 (multC (appC 'fatorial (plusC (idC 'n) (numC -1))) 
								(idC 'n))
						 (numC 1))]
                    [fdC 'narciso  'narciso (multC (idC 'narciso) (numC 1000))]
                    ))

(interp (desugar (parse (read))) mt-env biblioteca)