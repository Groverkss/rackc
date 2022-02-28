#lang racket
(require racket/set racket/stream)
(require racket/fixnum)
(require "interp-Lint.rkt")
(require "interp-Lvar.rkt")
(require "interp-Cvar.rkt")
(require "interp.rkt")
(require "utilities.rkt")
(provide (all-defined-out))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Lint examples
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The following compiler pass is just a silly one that doesn't change
;; anything important, but is nevertheless an example of a pass. It
;; flips the arguments of +. -Jeremy
(define (flip-exp e)
  (match e
    [(Var x) e]
    [(Prim 'read '()) (Prim 'read '())]
    [(Prim '- (list e1)) (Prim '- (list (flip-exp e1)))]
    [(Prim '+ (list e1 e2)) (Prim '+ (list (flip-exp e2) (flip-exp e1)))]))

(define (flip-Lint e)
  (match e
    [(Program info e) (Program info (flip-exp e))]))


;; Next we have the partial evaluation pass described in the book.
(define (pe-neg r)
  (match r
    [(Int n) (Int (fx- 0 n))]
    [else (Prim '- (list r))]))

(define (pe-add r1 r2)
  (match* (r1 r2)
    [((Int n1) (Int n2)) (Int (fx+ n1 n2))]
    [(_ _) (Prim '+ (list r1 r2))]))

(define (pe-exp e)
  (match e
    [(Int n) (Int n)]
    [(Prim 'read '()) (Prim 'read '())]
    [(Prim '- (list e1)) (pe-neg (pe-exp e1))]
    [(Prim '+ (list e1 e2)) (pe-add (pe-exp e1) (pe-exp e2))]))

(define (pe-Lint p)
  (match p
    [(Program info e) (Program info (pe-exp e))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; HW1 Passes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (uniquify-exp env)
  (lambda (e)
    (match e
      [(Var x) (Var (dict-ref env x))]
      [(Int n) (Int n)]
      [(Let x e body)
       (let ([new-env (dict-set env x (gensym x))])
         (Let
          (dict-ref new-env x)
          ((uniquify-exp env) e)
          ((uniquify-exp new-env) body)))]
      [(Prim op es)
       (Prim op (for/list ([e es]) ((uniquify-exp env) e)))])))

;; uniquify : R1 -> R1
(define (uniquify p)
  (match p
    [(Program info e) (Program info ((uniquify-exp '()) e))]))

; Given a list of expressions, build atoms from the and collect their
; environment. Returns (list (list atoms), environment).
(define (collect-env es)
  (match es
    ['() (list '() '())]
    [(list ast rst ...)
     (match (rco-atm ast)
       [(list atm env)
        (match (collect-env rst)
          [(list atm-ret env-ret)
           (list
            (cons atm atm-ret)
            (append env env-ret))])])]))

(define (create-tmp-var)
  (gensym "tmp"))

; Given a AST, convert it into an atom.
; Returns (list atom environment)
(define (rco-atm ast)
  (match ast
    [(Int n) (list (Int n) '())]
    [(Var n) (list (Var n) '())]
    ; Convert body to an atom and return that. Push this Let into the
    ; environment. This Let must come before the inside environment to
    ; preserve order of executation of statements.
    [(Let x e body)
     (match (rco-atm body)
       [(list atm env)
        (list atm (append (list (list x (rco-exp e))) env))])]
    ; Convert each es to an atom and collect their environment into env
    ; using collect-env. Now, create a new tmp variable, and assign it to
    ; be result of Prim. tmp-var = (Prim op atm-list). The new Prim created
    ; must come at end of environment to preserve order of execuation of
    ; statements.
    [(Prim op es)
     (match (collect-env es)
       [(list atm-list env)
        (let ([tmp-var (create-tmp-var)])
          (list
           (Var tmp-var)
           (append env (list (list tmp-var (Prim op atm-list))))))])]))

(define (create-let-from-env env body)
  (match env
    ['() body]
    [(list (list var e) more ...)
     (Let var e (create-let-from-env more body))]))

; Given a AST, remove complex expressions from it.
; Returns an AST
(define (rco-exp ast)
  (match ast
    [(Int n) (Int n)]
    [(Var x) (Var x)]
    [(Let x e body) (Let x (rco-exp e) (rco-exp body))]
    [(Prim op es)
     (match (collect-env es)
       [(list atm-list env)
        (create-let-from-env env (Prim op atm-list))])]))

; remove-complex-opera* : R1 -> R1
(define (remove-complex-opera* p)
  (match p
    [(Program info e) (Program info (rco-exp e))]))

(define (explicate-tail e)
  (match e
    [(Var x) (Return (Var x))]
    [(Int n) (Return (Int n))]
    [(Let x rhs body) (explicate-assign rhs x (explicate-tail body))]
    [(Prim op es) (Return (Prim op es))]
    [else (error "explicate-tail unhandled case" e)]))

(define (explicate-assign e x cont)
  (match e
    [(Var y) (Seq (Assign (Var x) (Var y)) cont)]
    [(Int n) (Seq (Assign (Var x) (Int n)) cont)]
    [(Let y rhs body)
     (explicate-assign rhs y (explicate-assign body x cont))]
    [(Prim op es) (Seq (Assign (Var x) (Prim op es)) cont)]
    [else (error "explicate-assign unhandled case" e)]))

;; explicate-control : R1 -> C0
(define (explicate-control p)
  (match p
    [(Program info body)
     (CProgram info (list (cons 'start (explicate-tail body))))]))

(define (select-atm atm)
  (match atm
    [(Var x) (Var x)]
    [(Int n) (Imm n)]
    [(Reg r) (Reg r)]))

(define (get-op-name prim)
  (match prim
    [(Prim '+ (list e1 e2)) 'addq]
    [(Prim '- (list e1 e2)) 'subq]
    [(Prim '- (list e1)) 'negq]))

(define (select-assign x e)
  (match e
    ; movq e, x
    [atm #:when (atm? atm)
         (list (Instr 'movq (list (select-atm atm) x)))]
    ; op x because e1 = x
    [(Prim op (list e1)) #:when (equal? e1 x)
                         (list (Instr (get-op-name e) (list (select-atm x))))]
    ; movq e1, x ; op x
    [(Prim op (list e1))
     (list
      (Instr 'movq (list (select-atm e1) x))
      (Instr (get-op-name e) (list (select-atm x))))]
    ; ; op e2 x because e1 = x
    [(Prim op (list e1 e2)) #:when (equal? e1 x)
                            (list (Instr
                                   (get-op-name e)
                                   (list (select-atm e2) x)))]
    ; movq e1, x ; op e2, x
    [(Prim op (list e1 e2))
     (list
      (Instr 'movq (list (select-atm e1) x))
      (Instr (get-op-name e) (list (select-atm e2) x)))]
    [(Prim 'read '())
      (list
        (Callq 'read_int 0)
        (Instr 'movq (list (Reg 'rax) x)))]
    [else (error "select-assign unhandled case")]))

(define (select-stmt stmt)
  (match stmt
    [(Return e) (select-assign (Reg 'rax) e)]
    [(Assign x e) (select-assign x e)]
    [else (error "select-stmt unhandled case")]))

(define (select-tail t)
  (match t
    [(Return x) (append (select-stmt t) (list (Jmp 'conclusion)))]
    [(Seq assign tail) (append (select-stmt assign) (select-tail tail))]
    [else (error "select-tail unhandled case")]))

;; select-instructions : C0 -> pseudo-x86
(define (select-instructions p)
  (match p
    [(CProgram info (list (cons 'start t)))
     (X86Program info (list (cons 'start (Block '() (select-tail t)))))]))

(define (get-next-stack-loc env)
  (* (- 8) (+ (length env) 1)))

; Returns new-arg, new-env
(define (assign-arg arg env)
  (match arg
    [(Var x)
     #:when (dict-has-key? env x)
     (values (dict-ref env x) env)]
    [(Var x)
     (let ([curr (get-next-stack-loc env)])
       (values
        (Deref 'rbp curr)
        (dict-set env x (Deref 'rbp curr))))]
    [else (values arg env)]))

; Returns new-args, new-env
(define (assign-arg-list args env)
  (match args
    ['() (values '() env)]
    [(list arg more ...)
     (define-values
       (tmp-arg tmp-env)
       (assign-arg arg env))
     (define-values
       (new-args new-env)
       (assign-arg-list more tmp-env))
     (values (cons tmp-arg new-args) new-env)]))

(define (assign-var-mapping instrs env)
  (match instrs
    ['() '()]
    [(list (Instr name args) more ... )
     (define-values
       (new-args new-env)
       (assign-arg-list args env))
     (cons
      (Instr name new-args)
      (assign-var-mapping more new-env))]
    [else (cons
           (car instrs)
           (assign-var-mapping (cdr instrs) env))]))

;; assign-homes : pseudo-x86 -> pseudo-x86
(define (assign-homes p)
  (match p
    [(X86Program info (list (cons 'start (Block blkinfo instrs))))
     (let ([new-instrs (assign-var-mapping instrs '())])
       (X86Program info (list (cons 'start (Block blkinfo new-instrs)))))]))

(define (patch-instrs-list instrs)
  (match instrs
    ['() '()]
    [(list (Instr op (list a b)) more ...)
     #:when (and (Deref? a) (Deref? b))
     (append
      (list (Instr 'movq (list a (Reg 'rax))))
      (list (Instr op (list (Reg 'rax) b)))
      (patch-instrs-list more))]
    [(list instr more ...)
     (cons
      instr
      (patch-instrs-list more))]))

;; patch-instructions : psuedo-x86 -> x86
(define (patch-instructions p)
  (match p
    [(X86Program info (list (cons 'start (Block blkinfo instrs))))
     (let ([new-instrs (patch-instrs-list instrs)])
       (X86Program info (list (cons 'start (Block blkinfo new-instrs)))))]))

; TODO: Fix stack frame size (Assumed to be 16?).
(define (generate-prelude)
  (list (cons 'main (Block '()
                     (list (Instr 'pushq (list (Reg 'rbp)))
                           (Instr 'movq (list (Reg 'rsp) (Reg 'rbp)))
                           (Instr 'subq (list (Imm 16000) (Reg 'rsp)))
                           (Jmp 'start))))))

; TODO: Fix stack frame size (Assumed to be 16?).
(define (generate-conclusion)
  (list (cons
          'conclusion
          (Block '()
                 (list (Instr 'addq (list (Imm 16000) (Reg 'rsp)))
                       (Instr 'popq (list (Reg 'rbp)))
                       (Retq))))))

;; prelude-and-conclusion : x86 -> x86
(define (prelude-and-conclusion p)
  (match p
    [(X86Program info blocks)
     (X86Program info (append (generate-prelude)
                              blocks
                              (generate-conclusion)))]))

;; Define the compiler passes to be used by interp-tests and the grader
;; Note that your compiler file (the file that defines the passes)
;; must be named "compiler.rkt"
(define compiler-passes
  `(("uniquify" ,uniquify ,interp-Lvar)
     ("remove complex opera*" ,remove-complex-opera* ,interp-Lvar)
     ("explicate control" ,explicate-control ,interp-Cvar)
     ("instruction selection" ,select-instructions ,interp-x86-0)
     ("assign homes" ,assign-homes ,interp-x86-0)
     ("patch instructions" ,patch-instructions ,interp-x86-0)
     ("prelude-and-conclusion" ,prelude-and-conclusion ,interp-x86-0)
     ))
