#! /usr/bin/env racket
#lang racket

(require "utilities.rkt")
(require "interp-Lvar.rkt")
(require "interp-Cvar.rkt")
(require "interp.rkt")
(require "compiler.rkt")

(define file (command-line #:args (filename) filename))
(define ast (read-program file))

(define (opt passes ast)
  (pretty-print ast)
  (match passes
    ['() ast]
    [(list (list name fun interp) more ...)
     (println (string-append "Applying " name))
     (opt more (fun ast))]))

(define final (opt compiler-passes ast))
