#lang racket
(require racket/set racket/stream)
(require racket/fixnum)
(require data/queue)
(require graph)
(require "interp-Lwhile.rkt")
(require "interp-Cwhile.rkt")
(require "interp.rkt")
(require "priority_queue.rkt")
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

;; shrink : R1 -> R1
;; Remove `and` and `or` from the language.
(define (shrink-expr exp)
  (match exp
    ; (and e1 e2) => (if e1 e2 #f)
    [(Prim 'and (list arg1 arg2))
     (If (shrink-expr arg1) (shrink-expr arg2) (Bool #f))]
    ; (or e1 e2) => (if e1 #t e2)
    [(Prim 'or (list arg1 arg2))
     (If (shrink-expr arg1) (Bool #t)(shrink-expr arg2) )]
    [(Prim op args)
     (Prim op (for/list ([arg args]) (shrink-expr arg)))]
    [(Let x e t)
     (Let x (shrink-expr e) (shrink-expr t))]
    [(If e1 e2 e3)
     (If (shrink-expr e1) (shrink-expr e2) (shrink-expr e3))]
    [(SetBang x e) (SetBang x (shrink-expr e))]
    [(Begin es e) (Begin (for/list ([esp es]) (shrink-expr esp)) (shrink-expr e))]
    [(WhileLoop cnd e) (WhileLoop (shrink-expr cnd) (shrink-expr e))]
    [_ exp]))

(define (shrink p)
  (match p
    [(Program info e)
     (Program info (shrink-expr e))]))

(define (uniquify-exp env)
  (lambda (e)
    (match e
      [(Var x) (Var (dict-ref env x))]
      [(Let x e body)
       (let ([new-env (dict-set env x (gensym x))])
         (Let
          (dict-ref new-env x)
          ((uniquify-exp env) e)
          ((uniquify-exp new-env) body)))]
      [(Prim op es)
       (Prim op (for/list ([e es]) ((uniquify-exp env) e)))]
      [(If e1 e2 e3)
       (If ((uniquify-exp env) e1) ((uniquify-exp env) e2) ((uniquify-exp env) e3))]
      [(SetBang x e)
       (SetBang (dict-ref env x) ((uniquify-exp env) e))]
      [(Begin es e)
       (Begin (for/list ([esp es]) ((uniquify-exp env) esp)) ((uniquify-exp env) e))]
      [(WhileLoop cnd e)
       (WhileLoop ((uniquify-exp env) cnd) ((uniquify-exp env) e))]
      [_ e])))

;; uniquify : R1 -> R1
(define (uniquify p)
  (match p
    [(Program info e) (Program info ((uniquify-exp '()) e))]))

(define (collect-set! e)
  (match e
    [(Var x) (set)]
    [(Int x) (set)]
    [(Bool x) (set)]
    [(Void) (set)]
    [(Prim op args) 
      (foldr (lambda (x v) (set-union v (collect-set! x))) (set) args)]
    [(Let x e t) (set-union (collect-set! e) (collect-set! t))]
    [(If e1 e2 e3)
     (set-union (collect-set! e1) (collect-set! e2) (collect-set! e3))]
    [(SetBang x e) (set-union (set x) (collect-set! e))]
    [(Begin es e) 
      (foldr (lambda (x v) (set-union v (collect-set! x))) (collect-set! e) es)]
    [(WhileLoop cnd e) (set-union (collect-set! cnd) (collect-set! e))]))

(define (uncover-get!-exp set!-vars exp)
  (match exp
    [(Var x) 
      (if (set-member? set!-vars x)
          (GetBang x)
          (Var x))]
    [(Int x) exp]
    [(Bool x) exp]
    [(Void) exp]
    [(Prim op args)
     (Prim op
           (for/list ([arg args])
             (uncover-get!-exp set!-vars arg)))]
    [(Let x e t)
     (Let x
          (uncover-get!-exp set!-vars e)
          (uncover-get!-exp set!-vars t))]
    [(If e1 e2 e3)
     (If (uncover-get!-exp set!-vars e1)
         (uncover-get!-exp set!-vars e2)
         (uncover-get!-exp set!-vars e3))]
    [(SetBang x e)
     (SetBang x (uncover-get!-exp set!-vars e))]
    [(Begin es e)
     (Begin
      (for/list ([esp es])
        (uncover-get!-exp set!-vars esp))
      (uncover-get!-exp set!-vars e))]
    [(WhileLoop cnd e)
     (WhileLoop
      (uncover-get!-exp set!-vars cnd)
      (uncover-get!-exp set!-vars e))]))

;; uncover-get! : Replace uses of mutable variables with (get! var)
(define (uncover-get! p)
  (match p
    [(Program info e)
     (Program info  (uncover-get!-exp (collect-set! e) e))]))

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

(define (create-as-tmp-exp ast)
  (let ([tmp-var (create-tmp-var)])
    (list
     (Var tmp-var)
     (list (list tmp-var (rco-exp ast))))))

; Given a AST, convert it into an atom.
; Returns (list atom environment)
(define (rco-atm ast)
  (match ast
    [(Int n) (list (Int n) '())]
    [(Bool n) (list (Bool n) '())]
    [(Var n) (list (Var n) '())]
    [(Void) (list (Void) '())]
    ; Convert body to an atom and return that. Push this Let into the
    ; environment. This Let must come before the inside environment to
    ; preserve order of executation of statements.
    [(Let x e body)
     (match (rco-atm body)
       [(list atm env)
        (list atm (append (list (list x (rco-exp e))) env))])]
    ; Assign this exp variable and return enviroment as that assignment.
    [(Prim op es) (create-as-tmp-exp ast)]
    [(If e1 e2 e3) (create-as-tmp-exp ast)]
    [(GetBang x) (create-as-tmp-exp ast)]
    [(SetBang var exp) (create-as-tmp-exp ast)]
    [(Begin exp-lst exp) (create-as-tmp-exp ast)]
    [(WhileLoop cnd exp) (create-as-tmp-exp ast)]))

(define (create-let-from-env env body)
  (match env
    ['() body]
    [(list (list var e) more ...)
     (Let var e (create-let-from-env more body))]))

; Given a AST, remove complex expressions from it.
; Returns an AST
(define (rco-exp ast)
  (match ast
    [(Var x) ast]
    [(Int x) ast]
    [(Bool x) ast]
    [(Void) ast]
    [(Let x e body) (Let x (rco-exp e) (rco-exp body))]
    [(Prim op es)
     (match (collect-env es)
       [(list atm-list env)
        (create-let-from-env env (Prim op atm-list))])]
    [(If e1 e2 e3) (If (rco-exp e1) (rco-exp e2) (rco-exp e3))]
    [(GetBang var) (Var var)]
    [(SetBang var exp) (SetBang var (rco-exp exp))]
    [(Begin exp-lst exp)
     (Begin
      (for/list ([exp-atm exp-lst])
        (rco-exp exp-atm))
      (rco-exp exp))]
    [(WhileLoop cnd exp) (WhileLoop (rco-exp cnd) (rco-exp exp))]))

; remove-complex-opera* : R1 -> R1
(define (remove-complex-opera* p)
  (match p
    [(Program info e) (Program info (rco-exp e))]))

(define basic-blocks '())

(define (create-block tail)
  (match tail
    [(Goto label) (Goto label)]
    [_ (let ([label (gensym 'block)])
         (set! basic-blocks (cons (cons label tail) basic-blocks))
         (Goto label))]))

; Check if a Prim op is a cmp.
(define (is-prim-cmp op)
  (or (equal? op 'eq?)
      (equal? op '<)
      (equal? op '<=)
      (equal? op '>)
      (equal? op '>=)))

(define (explicate-effect e cont)
  (match e
    [(Var x) cont]
    [(Int n) cont]
    [(Bool n) cont]
    [(Void) cont]
    [(Let y rhs body)
     (explicate-assign rhs y (explicate-effect body cont))]
    [(Prim 'read es) (Seq e cont)]
    [(Prim op es) cont]
    [(If cnd thn els)
     (let ([cont-goto (create-block cont)])
       (explicate-pred cnd
                       (explicate-effect thn cont-goto)
                       (explicate-effect els cont-goto)))]
    [(WhileLoop cnd body)
     (let* ([loop-label (gensym 'loop)]
            [cont-goto (create-block cont)]
            [loop-block
             (explicate-pred cnd
                             (create-block (explicate-effect body (Goto loop-label)))
                             (create-block cont-goto))])
       (set! basic-blocks
              (cons (cons loop-label loop-block) basic-blocks))
       (Goto loop-label))]
    [(Begin es body) 
      (let ([cont-body (explicate-effect body cont)])
        (foldr explicate-effect cont-body es))]
    [(SetBang var rhs) (explicate-assign rhs var cont)]
    [else (error "explicate-effect unhandled case" e)]))

; `thn`, `els` are assumed to be tails i.e. they are
; already passed through explicate-tail.
(define (explicate-pred cnd thn els)
  (match cnd
    [(Var x)
     (IfStmt (Prim 'eq? (list cnd (Bool #t)))
             (create-block thn)
             (create-block els))]
    [(Let x rhs body)
     (explicate-assign rhs x (explicate-pred body thn els))]
    [(Prim 'not (list e))
     (explicate-pred e els thn)]
    [(Prim op es) #:when (is-prim-cmp op)
                  (IfStmt (Prim op es)
                          (create-block thn)
                          (create-block els))]
    [(Bool b) (if b thn els)]
    ; Push cnd^ up and use thn^ and els^ as conditions for branches.
    [(If cnd^ thn^ els^)
     ; Create blocks for `thn` and `els` to prevent duplicates.
     (let ([thn-goto (create-block thn)]
           [els-goto (create-block els)])
       (explicate-pred cnd^
                       (explicate-pred thn^ thn-goto els-goto)
                       (explicate-pred els^ thn-goto els-goto)))]
    [else (error "explicate-pred unhandled case" cnd)]))

(define (explicate-tail e)
  (match e
    [(Var x) (Return (Var x))]
    [(Int n) (Return (Int n))]
    [(Bool n) (Return (Bool n))]
    [(Void) (Return (Void))]
    [(Let x rhs body) (explicate-assign rhs x (explicate-tail body))]
    [(Prim op es) (Return (Prim op es))]
    [(If cnd thn els)
     (explicate-pred cnd (explicate-tail thn) (explicate-tail els))]
    [(WhileLoop cnd body)
     (let* ([loop-label (gensym 'loop)]
            [loop-block
             (explicate-pred cnd
                             (create-block (explicate-effect body (Goto loop-label)))
                             (create-block (Return (Void))))])
       (set! basic-blocks
              (cons (cons loop-label loop-block) basic-blocks))
       (Goto loop-label))]
    [(Begin es body) 
      (let ([tail-body (explicate-tail body)])
        (foldr explicate-effect tail-body es))]
    [(SetBang var rhs) (explicate-assign rhs var (Return (Void)))]
    [else (error "explicate-tail unhandled case" e)]))

(define (explicate-assign e x cont)
  (match e
    [(Var y) (Seq (Assign (Var x) (Var y)) cont)]
    [(Int n) (Seq (Assign (Var x) (Int n)) cont)]
    [(Bool n) (Seq (Assign (Var x) (Bool n)) cont)]
    [(Void) (Seq (Assign (Var x) (Void)) cont)]
    [(Let y rhs body)
     (explicate-assign rhs y (explicate-assign body x cont))]
    [(Prim op es) (Seq (Assign (Var x) (Prim op es)) cont)]
    [(If cnd thn els)
     (let ([cont-goto (create-block cont)])
       (explicate-pred cnd
                       (explicate-assign thn x cont-goto)
                       (explicate-assign els x cont-goto)))]
    [(WhileLoop cnd body)
     (let* ([loop-label (gensym 'loop)]
            [cont-goto (create-block cont)]
            [loop-block
             (explicate-pred cnd
                             (create-block (explicate-effect body (Goto loop-label)))
                             (create-block (explicate-assign (Void) x cont-goto)))])
       (set! basic-blocks
              (cons (cons loop-label loop-block) basic-blocks))
       (Goto loop-label))]
    [(Begin es body) 
      (let ([cont-body (explicate-assign body x cont)])
        (foldr explicate-effect cont-body es))]
    [(SetBang var rhs) (explicate-assign rhs var (Seq (Assign (Var x) (Void)) cont))]
    [else (error "explicate-assign unhandled case" e)]))

;; explicate-control : R1 -> C0
(define (explicate-control p)
  (match p
    [(Program info body)
     (CProgram info (cons (cons 'start (explicate-tail body)) basic-blocks))]))

(define (select-atm atm)
  (match atm
    [(Var x) (Var x)]
    [(Int n) (Imm n)]
    [(Reg r) (Reg r)]
    [(Void) (Imm 0)]
    [(ByteReg r) (ByteReg r)]
    ; #t => 1
    ; #f => 0
    [(Bool b) (if b (Imm 1) (Imm 0))]))

(define (get-op-name prim)
  (match prim
    [(Prim '+ (list e1 e2)) 'addq]
    [(Prim '- (list e1 e2)) 'subq]
    [(Prim '- (list e1)) 'negq]))

(define (get-cmp-set-op op)
  (match op
    ['eq? 'sete]
    ['< 'setl]
    ['<= 'setle]
    ['> 'setg]
    ['>= 'setge]))

(define (select-assign x e)
  (match e
    ; movq e, x
    [atm #:when (atm? atm)
         (list (Instr 'movq (list (select-atm atm) x)))]

    ; not x because e1 = x
    [(Prim 'not (list e1)) #:when (equal? e1 x)
                           (list (Instr 'xorq (list (Imm 1) (select-atm x))))]
    ; movq e1, x ; not x
    [(Prim 'not (list e1))
     (list
      (Instr 'movq (list (select-atm e1) x))
      (Instr 'xorq (list (Imm 1) (select-atm x))))]

    [(Prim 'read '())
     (list
      (Callq 'read_int 0)
      (Instr 'movq (list (Reg 'rax) x)))]

    ; (cmp e1 e2) => cmpq e2 e1, setcc %al, movzbq %al x
    [(Prim op (list e1 e2)) #:when (is-prim-cmp op)
                            (list
                             (Instr 'cmpq (list (select-atm e2) (select-atm e1)))
                             (Instr (get-cmp-set-op op) (list (ByteReg 'al)))
                             (Instr 'movzbq (list (Reg 'al) (select-atm x))))]

    ; op x because e1 = x
    [(Prim op (list e1)) #:when (equal? e1 x)
                         (list (Instr (get-op-name e) (list (select-atm x))))]
    ; movq e1, x ; op x
    [(Prim op (list e1))
     (list
      (Instr 'movq (list (select-atm e1) x))
      (Instr (get-op-name e) (list (select-atm x))))]

    ; op e2 x because e1 = x
    [(Prim op (list e1 e2)) #:when (equal? e1 x)
                            (list (Instr
                                   (get-op-name e)
                                   (list (select-atm e2) x)))]
    ; movq e1, x ; op e2, x
    [(Prim op (list e1 e2))
     (list
      (Instr 'movq (list (select-atm e1) x))
      (Instr (get-op-name e) (list (select-atm e2) x)))]

    [else (error "select-assign unhandled case" e)]))

(define (select-stmt stmt)
  (match stmt
    [(Return e) (select-assign (Reg 'rax) e)]
    [(Assign x e) (select-assign x e)]
    [(Prim 'read '())
     (list (Callq 'read_int 0))]
    [else (error "select-stmt unhandled case" stmt)]))

(define (get-cmp-cnd op)
  (match op
    ['eq? 'e]
    ['< 'l]
    ['<= 'le]
    ['> 'g]
    ['>= 'ge]))

; if (cmp e1 e2) thn els =>
; cmpq e2 e1
; je (label thn)
; jmp (label els)
(define (select-if cnd thn els)
  (let ([label1 (match thn [(Goto label) label])]
        [label2 (match els [(Goto label) label])])
    (match cnd
      [(Prim op (list e1 e2))
       #:when (is-prim-cmp op)
       (list (Instr 'cmpq (list (select-atm e2) (select-atm e1)))
             (JmpIf (get-cmp-cnd op) label1)
             (Jmp label2))])))

(define (select-tail t)
  (match t
    [(Return x) (append (select-stmt t) (list (Jmp 'conclusion)))]
    [(Seq assign tail) (append (select-stmt assign) (select-tail tail))]
    [(Goto label) (list (Jmp label))]
    [(IfStmt cnd thn els) (select-if cnd thn els)]
    [else (error "select-tail unhandled case" t)]))

;; select-instructions : C0 -> pseudo-x86
(define (select-instructions p)
  (match p
    [(CProgram info tails)
     (X86Program info (for/list ([tail tails])
                        (match tail
                          [(cons label t)
                           (cons label (Block '() (select-tail t)))])))]))

(define (convert-to-regs names) (for/list ([reg names]) (Reg reg)))
(define caller-saved-list (convert-to-regs (list 'rax 'rcx 'rdx 'rsi 'rdi 'r8 'r9 'r10 'r11)))
(define callee-saved-list (convert-to-regs (list 'rsp 'rbp 'rbx 'r12 'r13 'r14 'r15)))
(define labels->live (make-hash))
(define labels->blocks (make-hash))

;; Get all write locations from an instruction.
;; TODO: Ask Bharat if all instructions do a write or is it only
;;       movq.
(define (get-write-locations instr)
  (match instr
    ; `cmpq` instructions do not write.
    [(Instr 'cmpq _) (set)]
    [(Instr _ `(,arg1 ,arg2)) (set arg2)]
    [(Instr _ `(,arg1)) (set arg1)]
    [(Callq _ _) (list->set caller-saved-list)]
    [(Jmp _) (set)]
    [(JmpIf _ _) (set)]
    [_ (error "Unimplemented instruction" instr)]))

;; Get all read locations from an instruction.
(define (get-read-locations instr)
  (set-subtract
   (match instr
     [(Instr _ (list args ...)) (list->set (filter (lambda (x) (not (Imm? x))) args))]
     [(Callq _ airty) (list->set (take callee-saved-list airty))]
     [(Jmp _) (set)]
     [(JmpIf _ _) (set)]
     [_ (error "Unimplemented instruction?" instr)])
   (get-write-locations instr)))

;; L_after(k) = (L_after(k + 1) - W(k + 1)) + R(k + 1)
(define (compute-L_afterk L_afterk+1 instrk+1)
  (set-union (set-subtract L_afterk+1
                           (get-write-locations instrk+1))
             (get-read-locations instrk+1)))

(define (liveness-analysis-instrs instrs live->after)
  (match instrs
    ; jmpcc, jmp -> use labels->live
    [(list (JmpIf _ target1) (Jmp target2))
     (list live->after live->after)]
    ; jmp -> use labels->live
    [(list (Jmp target))
     (list live->after)]
    ; Base case.
    [(list _ instrk+1 _ ...)
     (let* ([anal (liveness-analysis-instrs (rest instrs) live->after)]
            [L_afterk+1 (car anal)])
       (cons (compute-L_afterk L_afterk+1 instrk+1) anal))]))

;; Given a block, do a pass of backward dataflow analysis on it.
(define (liveness-analysis-block block label live->after)
  (match block
    [(Block blkinfo instrs)
     (let* ([liveness (liveness-analysis-instrs instrs live->after)]
            [label-liveness (compute-L_afterk
                             (car liveness)
                             (car instrs))])
       ; Set liveness information for this label.
       (dict-set! labels->live label label-liveness)
       ; Attach livness to this block and return it.
       (Block
        (dict-set blkinfo 'liveness liveness)
        instrs))]))

;; Given a list of (cons label block), build a CFG.
(define (build-liveness-cfg label-block-lst)
  (let ([cfg (directed-graph '())])
    ; Traverse through all blocks.
    (for ([label-block label-block-lst])
      (match label-block
        [(cons label block)
         (match block
           [(Block blkinfo instrs)
            ; Traverse through all instructions of a block.
            (for ([instr instrs])
              (match instr
                ; For each instruction add edge in CFG for a jump.
                [(JmpIf _ target) (add-directed-edge! cfg label target)]
                [(Jmp target) (add-directed-edge! cfg label target)]
                [_ null]))])]))
    cfg))

(define (analyze_dataflow G transfer bottom join)
  (define mapping (make-hash))
  (for ([v (in-vertices G)])
    (dict-set! mapping v bottom))
  (define worklist (make-queue))
  (for ([v (in-vertices G)])
    (enqueue! worklist v))
  (define trans-G (transpose G))
  (while (not (queue-empty? worklist))
         (define node (dequeue! worklist))
         (define input (for/fold ([state bottom])
                                 ([pred (in-neighbors trans-G node)])
                         (join state (dict-ref mapping pred))))
         (define output (transfer node input))
         (cond [(not (equal? output (dict-ref mapping node)))
                (dict-set! mapping node output)
                (for ([v (in-neighbors G node)])
                  (enqueue! worklist v))]))
  mapping)

(define (liveness-transfer label live->after)
  (if (eq? label 'conclusion)
      (set)
      (let* ([blk (dict-ref labels->blocks label)]
             [update-blk (liveness-analysis-block blk label live->after)])
        (dict-set! labels->blocks label update-blk)
        (dict-ref labels->live label))))

;; Given a list of (cons label block), do liveness analysis.
(define (liveness-analysis-blocks label-block-lst)
  (let* ([cfg (transpose (build-liveness-cfg label-block-lst))])
    ; Set label -> block mapping in labels->blocks
    (for ([label-block label-block-lst])
      (match label-block
        [(cons label block) (dict-set! labels->blocks label block)]))
    ; Run dataflow analysis
    (analyze_dataflow cfg liveness-transfer (set) set-union)
    ; Return blocks with liveness set
    (hash->list labels->blocks)))

;; Uncover live pass : Analysis to find live variables.
(define (uncover-live p)
  (match p
    [(X86Program info label-block-lst)
     (X86Program info (liveness-analysis-blocks label-block-lst))]))

; Add conflicts between set1 and set2
(define (add-interference set1 set2 graph)
  (for* ([s1 set1]
         [s2 set2])
    (add-edge! graph s1 s2))
  graph)

(define (add-vertices-interference! instr graph)
  (match instr
    [(Instr op args)
     (for ([arg args])
       (if (not (Imm? arg))
           (add-vertex! graph arg)
           void))]
    [_ void]))

(define (build-interference-instr L_afterk instr graph)
  (add-vertices-interference! instr graph)
  (match instr
    ; Add edge between (d, v) \forall v \in L_after(k) | v != s or v != d
    [(Instr op (list s d))
     #:when (or (eq? op 'movq) (eq? op 'movzbq))
     (add-vertex! graph d)
     ; Add edges
     (let* ([filtered_L (set-subtract L_afterk (set s d))])
       (add-interference filtered_L (set d) graph))]
    ; For any other instruction, (d, v) \forall d \in write(instr) v \in L_afterk | v != d
    [_
     (let* ([write-locs (get-write-locations instr)]
            [filtered_L (set-subtract L_afterk write-locs)])
       (add-interference filtered_L write-locs graph))]))

(define (build-interference-instrs liveness instrs graph)
  (match instrs
    ['() graph]
    [(list instr rest-instrs ...)
     (build-interference-instrs
      (cdr liveness)
      rest-instrs
      (build-interference-instr (car liveness) instr graph))]))

(define (build-interference-blocks blocks graph)
  (match blocks
    ['() graph]
    [(list (cons _ (Block blkinfo instrs)) rest-blocks ...)
     (build-interference-blocks
      rest-blocks
      (build-interference-instrs
       (dict-ref blkinfo 'liveness) instrs graph))]))

;; Build Interference Graph : Analysis to find register conflicts.
(define (build-interference p)
  (match p
    [(X86Program info blocks)
     (X86Program
      (dict-set
       info
       'conflicts
       (build-interference-blocks blocks (undirected-graph '())))
      blocks)]))

; TODO: Update priority of graph neighbours.
(define (color-graph-update-queue graph queue v) queue)

; Given a set of colors, find the MEX starting from 0.
(define (get-mex-color s)
  (define (get-mex-color-start s t)
    (if
     (set-member? s t)
     (get-mex-color-start s (+ t 1))
     t))
  (get-mex-color-start s 0))

; Returns -1 for unassigned neighbors. This doesn't affect
; algorithm since we take MEX from 0.
(define (get-neighbor-colors graph current-mapping v)
  (for/set ([n (in-neighbors graph v)])
    (if (dict-has-key? current-mapping n)
        (dict-ref current-mapping n)
        -1)))

(define (color-graph-update-mapping graph current-mapping v)
  (dict-set
   current-mapping
   v
   (get-mex-color (get-neighbor-colors graph current-mapping v))))

(define (color-graph-recurse graph queue current-mapping)
  (if
   (= (pqueue-count queue) 0)
   current-mapping
   (let ([v (vector-ref (pqueue-pop! queue) 0)])
     (color-graph-recurse
      graph
      (color-graph-update-queue graph queue v)
      (color-graph-update-mapping graph current-mapping v)))))

; Vertices are stored in priority queue as (vertex, priority)
(define (color-graph-<= v1 v2)
  (>= (vector-ref v1 1) (vector-ref v2 1)))

(define unassignable-register-mapping
  (list (cons (ByteReg 'al) -10)
        (cons (Reg 'rax) -1)
        (cons (Reg 'r11) -2)
        (cons (Reg 'r15) -3)
        (cons (Reg 'rbp) -4)
        (cons (Reg 'rbx)  0)
        (cons (Reg 'rcx)  1)
        (cons (Reg 'rdx)  2)
        (cons (Reg 'rsi)  3)
        (cons (Reg 'rdi)  4)
        (cons (Reg 'r8)   5)
        (cons (Reg 'r9)   6)
        (cons (Reg 'r10)  7)
        (cons (Reg 'r12)  8)
        (cons (Reg 'r13)  9)
        (cons (Reg 'r14) 10)))

(define (get-vertex-priority graph v)
  (sequence-length (in-neighbors graph v)))

(define (color-graph graph)
  (let* ([queue (make-pqueue color-graph-<=)])
    (for ([v (in-vertices graph)])
      (match v
        [(Reg _) void]
        [_ (pqueue-push! queue (vector v (get-vertex-priority graph v)))]))
    (color-graph-recurse graph queue unassignable-register-mapping)))

(define (get-next-stack-loc env)
  (* (- 8) (+ (length env) 1)))

(define color-to-register-mapping
  (list (cons  0  (Reg 'rbx))
        (cons  1  (Reg 'rcx))
        (cons  2  (Reg 'rdx))
        (cons  3  (Reg 'rsi))
        (cons  4  (Reg 'rdi))
        (cons  5  (Reg 'r8))
        (cons  6  (Reg 'r9))
        (cons  7  (Reg 'r10))
        (cons  8  (Reg 'r12))
        (cons  9  (Reg 'r13))
        (cons  10 (Reg 'r14))))

(define (get-stack-loc color)
  (* -8 (+ (- color (length color-to-register-mapping)) 1)))

(define (get-mapping-from-color color)
  (if (dict-has-key? color-to-register-mapping color)
      (dict-ref color-to-register-mapping color)
      (Deref 'rbp (get-stack-loc color))))

(define (allocate-registers-arg arg allocation)
  (if (Var? arg)
      (get-mapping-from-color (dict-ref allocation arg))
      arg))

; Assumes `Instrs` is the only instruction that can have arguements is `Instr`.
(define (allocate-registers-instrs instrs allocation)
  (for/list ([instr instrs])
    (match instr
      [(Instr name args)
       (Instr name (for/list ([arg args])
                     (allocate-registers-arg arg allocation)))]
      [_ instr])))

(define (allocate-registers-blocks blocks allocation)
  (for/list ([block blocks])
    (match block
      [(cons label (Block blkinfo instrs))
       (cons label (Block blkinfo (allocate-registers-instrs instrs allocation)))])))

;; Allocate registers : Take out interference graph from info of program and do register allocation.
(define (allocate-registers p)
  (match p
    [(X86Program info blocks)
     (let*
         ([allocation (color-graph (dict-ref info 'conflicts))])
       (X86Program info (allocate-registers-blocks blocks allocation)))]))

(define (patch-instrs-list instrs)
  (match instrs
    ['() '()]
    [(list (Instr 'movq (list a b)) more ...)
     #:when (equal? a b)
     (patch-instrs-list more)]
    [(list (Instr op (list a b)) more ...)
     #:when (and (Deref? a) (Deref? b))
     (append
      (list (Instr 'movq (list a (Reg 'rax))))
      (list (Instr op (list (Reg 'rax) b)))
      (patch-instrs-list more))]
    [(list (Instr 'cmpq (list a (Imm b))) more ...)
     (append
      (list (Instr 'movq (list (Imm b) (Reg 'rax))))
      (list (Instr 'cmpq (list a (Reg 'rax))))
      (patch-instrs-list more))]
    [(list (Instr 'movzbq (list a (Deref b num))) more ...)
     (append
      (list (Instr 'movzbq (list a (Reg 'rax))))
      (list (Instr 'movq (list (Reg 'rax) (Deref b num))))
      (patch-instrs-list more))]
    [(list instr more ...)
     (cons
      instr
      (patch-instrs-list more))]))

(define (patch-instructions-blocks label-block-lst)
  (for/list ([label-block label-block-lst])
    (match label-block
      [(cons label block)
       (match block
         [(Block blkinfo instrs)
          (cons label (Block blkinfo (patch-instrs-list instrs)))])])))

;; patch-instructions : psuedo-x86 -> x86
(define (patch-instructions p)
  (match p
    [(X86Program info label-block-lst)
     (X86Program info (patch-instructions-blocks label-block-lst))]))

; TODO: Fix stack frame size (Assumed to be 16?).
;       Also take care of callq instructions stack thingy mentioned
;       in chapter 3
(define (generate-prelude)
  (list (cons 'main (Block '()
                           (list (Instr 'pushq (list (Reg 'rbp)))
                                 (Instr 'movq (list (Reg 'rsp) (Reg 'rbp)))
                                 (Instr 'subq (list (Imm 64000) (Reg 'rsp)))
                                 (Jmp 'start))))))

; TODO: Fix stack frame size (Assumed to be 16?).
;       Also take care of callq instructions stack thingy mentioned
;       in chapter 3
(define (generate-conclusion)
  (list (cons
         'conclusion
         (Block '()
                (list (Instr 'addq (list (Imm 64000) (Reg 'rsp)))
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
  `(("shrink" ,shrink, interp-Lwhile)
    ("uniquify" ,uniquify ,interp-Lwhile)
    ("uncover get!" ,uncover-get!, interp-Lwhile)
    ("remove complex opera*" ,remove-complex-opera* ,interp-Lwhile)
    ("explicate control" ,explicate-control ,interp-Cwhile)
    ("instruction selection" ,select-instructions ,#f)
    ("uncover live" ,uncover-live ,#f)
    ("build interference graph" ,build-interference ,#f)
    ("allocate registers" ,allocate-registers ,#f)
    ("patch instructions" ,patch-instructions ,#f)
    ("prelude-and-conclusion" ,prelude-and-conclusion ,#f)
    ))
