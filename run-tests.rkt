#! /usr/bin/env racket
#lang racket

(require "utilities.rkt")
(require "interp-Lif.rkt")
(require "interp.rkt")
(require "type-check-Lif.rkt")
(require "compiler.rkt")
; (debug-level 2)
(AST-output-syntax 'concrete-syntax)

;; all the files in the tests/ directory with extension ".rkt".
(define all-tests
  (map (lambda (p) (car (string-split (path->string p) ".")))
       (filter (lambda (p)
                 (string=? (cadr (string-split (path->string p) ".")) "rkt"))
               (directory-list (build-path (current-directory) "tests")))))

(define (tests-for r)
  (map (lambda (p)
         (caddr (string-split p "_")))
       (filter
        (lambda (p)
          (string=? r (car (string-split p "_"))))
        all-tests)))

(interp-tests "cond" type-check-Lif compiler-passes interp-Lif "cond_test" (tests-for "cond"))

;; Uncomment the following when all the passes are complete to
;; test the final x86 code.
(compiler-tests "cond" type-check-Lif compiler-passes "cond_test" (tests-for "cond"))
