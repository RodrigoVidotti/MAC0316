#lang plai-typed

(define-type ExprC
  [numC  (n : number)]
  [varC  (s : symbol)] ; não é mais identificador
  [plusC (l : ExprC) (r : ExprC)]
  [multC (l : ExprC) (r : ExprC)]
  [divC  (l : ExprC) (r : ExprC)]
  [lamC (arg : symbol) (body : ExprC)] ; nomes não são mais necessários
  [appC (fun : ExprC) (arg : ExprC)] ; a aplicação recebe uma função
  [ifC   (condição : ExprC) (sim : ExprC) (não : ExprC)]
  [seqC    (b1 : ExprC) (b2 : ExprC)]
  [setC    (var : symbol) (arg : ExprC)]
  )

; inclui funções e variáveis
(define-type ExprS
  [numS    (n : number)]
  [varS     (s : symbol)] 
  [lamS    (arg : symbol) (body : ExprS)] ; muda de acordo
  [appS    (fun : ExprS) (arg : ExprS)] 
  [plusS   (l : ExprS) (r : ExprS)]
  [bminusS (l : ExprS) (r : ExprS)]
  [uminusS (e : ExprS)]
  [multS   (l : ExprS) (r : ExprS)]
  [divS    (l : ExprS) (r : ExprS)]
  [ifS     (c : ExprS) (s : ExprS) (n : ExprS)]
  [seqS    (b1 : ExprS) (b2 : ExprS)]
  [setS    (var : symbol) (arg : ExprS)]
  )

; agora é preciso arrumar desugar interpretador e parser.

(define (desugar [as : ExprS]) : ExprC  
  (type-case ExprS as
    [numS    (n) (numC n)]
    [varS     (s) (varC s)]
    [lamS    (a b)  (lamC a (desugar b))] ; idem
    [appS    (fun arg) (appC (desugar fun) (desugar arg))]
    [plusS   (l r) (plusC (desugar l) (desugar r))] 
    [multS   (l r) (multC (desugar l) (desugar r))]
    [divS    (l r) (divC (desugar l) (desugar r))]
    [bminusS (l r) (plusC (desugar l) (multC (numC -1) (desugar r)))]
    [uminusS (e)   (multC (numC -1) (desugar e))]
    [ifS     (c s n) (ifC (desugar c) (desugar s) (desugar n))]
    [seqS    (b1 b2)            (seqC (desugar b1) (desugar b2))]
    [setS    (var expr)         (setC  var (desugar expr))]
    ))

; Precisamos de Storage e Locations
(define-type-alias Location number)

#|
 | Closure não tem mais o nome, mas precisa do environment
 |#
(define-type Value
  [numV  (n : number)]
  [closV (arg : symbol) (body : ExprC) (env : Env)])


; símbolos devem se associar ao location
(define-type Binding
      [bind (name : symbol) (val : Location)])

; A lista de associações é o environment
(define-type-alias Env (listof Binding))
(define mt-env empty)        ; ente pronunciar "mt" em inglês e compare com "empty"
(define extend-env cons)     ; sorte, cons faz exatamente o que queremos para estender o env

;Armazenamento
(define-type Storage
      [cell (location : Location) (val : Value)])
(define-type-alias Store (listof Storage))

(define mt-store empty)
(define override-store cons)

; lookup também muda o tipo de retorno
(define (lookup [for : symbol] [env : Env]) : Location
       (cond
            [(empty? env) (error 'lookup (string-append (symbol->string for) " não foi encontrado"))] ; livre (não definida)
            [else (cond
                  [(symbol=? for (bind-name (first env)))   ; achou!
                                 (bind-val (first env))]
                  [else (lookup for (rest env))])]))        ; vê no resto

; fetch é o lookup do store
(define (fetch [l : Location] [sto : Store]) : Value
       (cond
            [(empty? sto) (error 'fetch "posição não encontrada")]
            [else (cond
                  [(= l   (cell-location (first sto)))   ; achou!
                                 (cell-val (first sto))]
                  [else (fetch l (rest sto))])]))        ; vê no resto


;; retorna a próxima localização disponível
(define new-loc
   (let ( [ n (box 0)])
        (lambda () 
           (begin
              (set-box! n (+ 1 (unbox n)))
              (unbox n)))))

; novos operadores
(define (num+ [l : Value] [r : Value]) : Value
    (cond
        [(and (numV? l) (numV? r))
             (numV (+ (numV-n l) (numV-n r)))]
        [else
             (error 'num+ "Um dos argumentos não é número")]))

(define (num* [l : Value] [r : Value]) : Value
    (cond
        [(and (numV? l) (numV? r))
             (numV (* (numV-n l) (numV-n r)))]
        [else
             (error 'num* "Um dos argumentos não é número")]))

(define (num/ [l : Value] [r : Value]) : Value
    (cond
        [(and (numV? l) (numV? r))
             (numV (/ (numV-n l) (numV-n r)))]
        [else
             (error 'num/ "Um dos argumentos não é número")]))

(define-type Result
      [v*s (v : Value) (s : Store)])

; Agora o interpretador!
(define (interp [a : ExprC] [env : Env] [sto : Store]) : Result
  (type-case ExprC a
    [numC (n) (v*s (numV n) sto)]
    [varC (n)  (v*s (fetch (lookup n env) sto) sto)]  ; busca em cascata, env e em seguida no sto 
    [lamC (a b) (v*s (closV a b env) sto)] ; definição de função captura o environment
    [seqC (b1 b2) (type-case Result (interp b1 env sto)
                    [v*s (v-b1 s-b1) ; resultado e store retornado por b1
                          (interp b2 env s-b1)])]

    [appC (f a)
      (type-case Result (interp f env sto) ; acha a função
         [v*s (v-f s-f)
              (type-case Result (interp a env s-f) ; argumento com sto modificado pela função
                 [v*s (v-a s-a)
                      (let ([onde (new-loc)]) ; aloca posição para o valor do argumento
                           (interp (closV-body v-f) ; corpo
                                   (extend-env (bind (closV-arg v-f) onde) ; com novo argumento
                                       (closV-env v-f))
                                   (override-store (cell onde v-a) s-a))) ; com novo valor
                  ])])]

    [plusC (l r) 
           (type-case Result (interp l env sto)
               [v*s (v-l s-l)
                    (type-case Result (interp r env s-l)
                      [v*s (v-r s-r)
                           (v*s (num+ v-l v-r) s-r)])])]
    [multC (l r) 
           (type-case Result (interp l env sto)
               [v*s (v-l s-l)
                    (type-case Result (interp r env s-l)
                      [v*s (v-r s-r)
                           (v*s (num* v-l v-r) s-r)])])]

    [divC (l r) 
           (type-case Result (interp l env sto)
               [v*s (v-l s-l)
                    (type-case Result (interp r env s-l)
                      [v*s (v-r s-r)
                           (v*s (num/ v-l v-r) s-r)])])]

    [ifC (c s n) (if (zero? (numV-n (v*s-v (interp c env sto)))) (interp n env sto) (interp s env sto))]
    

    [setC (var val) (type-case Result (interp val env sto)
                     [v*s (v-val s-val)
                          (let ([onde (lookup var env)]) ; acha a variável
                            (v*s v-val
                                 (override-store ; atualiza
                                  (cell onde v-val) s-val)))])]
    ))

; o parser precisa tratar de chamadas
(define (parse [s : s-expression]) : ExprS
  (cond
    [(s-exp-number? s) (numS (s-exp->number s))]
    [(s-exp-symbol? s) (varS (s-exp->symbol s))] ; pode ser um símbolo livre nas definições de função
    [(s-exp-list? s)
     (let ([sl (s-exp->list s)])
       (case (s-exp->symbol (first sl))
         [(+) (plusS (parse (second sl)) (parse (third sl)))]
         [(*) (multS (parse (second sl)) (parse (third sl)))]
         [(/) (divS  (parse (second sl)) (parse (third sl)))]
         [(-) (bminusS (parse (second sl)) (parse (third sl)))]
         [(~) (uminusS (parse (second sl)))]
         [(func) (lamS (s-exp->symbol (second sl)) (parse (third sl)))] ; definição
         [(call) (appS (parse (second sl)) (parse (third sl)))]
         [(if) (ifS (parse (second sl)) (parse (third sl)) (parse (fourth sl)))]
         [(seq) (seqS (parse (second sl)) (parse (third sl)))]
         [(:=) (setS (s-exp->symbol (second sl)) (parse (third sl)))]
         [else (error 'parse "invalid list input")]))]
    [else (error 'parse "invalid input")]))

; Facilitador
(define (interpS [s : s-expression]) (interp (desugar (parse s)) mt-env mt-store))

(numV-n (v*s-v (interpS (read))))