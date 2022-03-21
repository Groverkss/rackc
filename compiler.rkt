#lang racket
(require racket/set racket/stream)
(require racket/fixnum)
(require graph)
(require "interp-Lint.rkt")
(require "interp-Lvar.rkt")
(require "interp-Cvar.rkt")
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

(define (convert-to-regs names) (for/list ([reg names]) (Reg reg)))
(define caller-saved-list (convert-to-regs (list 'rax 'rcx 'rdx 'rsi 'rdi 'r8 'r9 'r10 'r11)))
(define callee-saved-list (convert-to-regs (list 'rsp 'rbp 'rbx 'r12 'r13 'r14 'r15)))

;; Get all write locations from an instruction.
;; TODO: Ask Bharat if all instructions do a write or is it only
;;       movq.
(define (get-write-locations instr)
  (match instr
    [(Instr _ `(,arg1 ,arg2)) (set arg2)]
    [(Instr _ `(,arg1)) (set arg1)]
    [(Callq _ _) (list->set caller-saved-list)]
    [(Jmp _) (set)]
    [_ "Unimplemented instruction"]))

;; Get all read locations from an instruction.
(define (get-read-locations instr)
  (set-subtract
   (match instr
     [(Instr _ (list args ...)) (list->set (filter (lambda (x) (not (Imm? x))) args))]
     [(Callq _ airty) (list->set (take callee-saved-list airty))]
     [(Jmp _) (set)]
     [_ "Unimplemented instruction?"])
   (get-write-locations instr)))

(define (liveness-analysis-instrs instrs)
  (match instrs
    ; L_after is empty for last instruction.
    [(list instr) (list (set))]
    ; L_after(k) = (L_after(k + 1) - W(k + 1)) + R(k + 1)
    [(list _ instrk+1 _ ...)
     (let* ([anal (liveness-analysis-instrs (rest instrs))]
            [L_afterk+1 (car anal)])
       (cons
        (set-union (set-subtract
                    L_afterk+1
                    (get-write-locations instrk+1))
                   (get-read-locations instrk+1))
        anal))]))

;; Given a block, do a pass of backward dataflow analysis on it.
(define (liveness-analysis block)
  (match block
    [(Block blkinfo instrs)
     (Block
      (dict-set blkinfo 'liveness (liveness-analysis-instrs instrs))
      instrs)]))

;; Uncover live pass : Analysis to find live variables.
(define (uncover-live p)
  (match p
    [(X86Program info (list (cons 'start block)))
     (X86Program info (list (cons 'start (liveness-analysis block))))]))

; Add conflicts between set1 and set2
(define (add-interference set1 set2 graph)
  (for* ([s1 set1]
         [s2 set2])
    (add-edge! graph s1 s2))
  graph)

(define (build-interference-instr L_afterk instr graph)
  (match instr
    ; Add edge between (d, v) \forall v \in L_after(k) | v != s or v != d
    [(Instr 'movq (list s d))
     (add-vertex! graph d)
     ; Add vertex s and d, just in case no edge is added.
     (if (not (Imm? s))
         (add-vertex! graph s)
         void)
     (if (not (Imm? d))
         (add-vertex! graph d)
         void)
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
  (list (cons (Reg 'rax) -1)
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
      [(list (Instr op (list a b)) more ...)
       #:when (equal? a b)
       (patch-instrs-list more)]
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
      ("uncover live" ,uncover-live ,interp-x86-0)
      ("build interference graph" ,build-interference ,interp-x86-0)
      ("allocate registers" ,allocate-registers ,interp-x86-0)
      ("patch instructions" ,patch-instructions ,interp-x86-0)
      ("prelude-and-conclusion" ,prelude-and-conclusion ,interp-x86-0)
      ))