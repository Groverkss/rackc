#! /usr/bin/env racket
#lang racket

(require "utilities.rkt")
(require "compiler.rkt")

(define file (command-line #:args (filename) filename))

(AST-output-syntax 'concrete-syntax)

(define opt (compile-file #f compiler-passes))

(opt file)
