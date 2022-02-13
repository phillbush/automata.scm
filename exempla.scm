#!/usr/bin/env scheme-r7rs
(import (scheme small))
(load "automata.scm")

; Let's define some strings.
; Our alphabet are the numbers 0 and 1.
(define strings
  (list
    "01010"
    "11010"
    "10111"
    "10010"))

; This DFA recognizes the language of strings beginning with 1 and
; ending with 0.  All our states are numbers.
(define start-1-end-0
  (make-dfa
    0
    (lambda (state) (eq? state 2))
    (lambda (symbol state)
      (cond ((and (eq? state 0) (eq? symbol #\0)) 3)
            ((and (eq? state 0) (eq? symbol #\1)) 1)
            ((and (eq? state 1) (eq? symbol #\0)) 2)
            ((and (eq? state 1) (eq? symbol #\1)) 1)
            ((and (eq? state 2) (eq? symbol #\0)) 2)
            ((and (eq? state 2) (eq? symbol #\1)) 1)
            ((and (eq? state 3) (eq? symbol #\0)) 3)
            ((and (eq? state 3) (eq? symbol #\1)) 3)))))

; This DFA recognizes the language of string with at least three 1's
(define has-three-1s
  (make-dfa
    0
    (lambda (state) (eq? state 3))
    (lambda (symbol state)
      (cond ((and (eq? state 0) (eq? symbol #\0)) 0)
            ((and (eq? state 0) (eq? symbol #\1)) 1)
            ((and (eq? state 1) (eq? symbol #\0)) 1)
            ((and (eq? state 1) (eq? symbol #\1)) 2)
            ((and (eq? state 2) (eq? symbol #\0)) 2)
            ((and (eq? state 2) (eq? symbol #\1)) 3)
            ((and (eq? state 3) (eq? symbol #\0)) 3)
            ((and (eq? state 3) (eq? symbol #\1)) 3)))))

; This DFA recognizes the language of strings which does not have the
; substring "110" in it.
(define has-no-110
  (make-dfa
    0
    (lambda (state)
      (or (eq? state 0)
          (eq? state 1)
          (eq? state 2)))
    (lambda (symbol state)
      (cond ((and (eq? state 0) (eq? symbol #\0)) 0)
            ((and (eq? state 0) (eq? symbol #\1)) 1)
            ((and (eq? state 1) (eq? symbol #\0)) 0)
            ((and (eq? state 1) (eq? symbol #\1)) 2)
            ((and (eq? state 2) (eq? symbol #\0)) 3)
            ((and (eq? state 2) (eq? symbol #\1)) 2)
            ((and (eq? state 3) (eq? symbol #\0)) 3)
            ((and (eq? state 3) (eq? symbol #\1)) 3)))))

; This NFA recognizes the language of strings whose third-to-last symbol is 1
(define third-to-last-is-1
  (make-nfa
    0
    (lambda (state) (eq? state 3))
    (lambda (symbol state)
      (cond ((and (eq? state 0) (eq? symbol #\0)) (make-set 0))
            ((and (eq? state 0) (eq? symbol #\1)) (make-set 0 1))
            ((and (eq? state 1) (eq? symbol #\0)) (make-set 2))
            ((and (eq? state 1) (eq? symbol #\1)) (make-set 2))
            ((and (eq? state 2) (eq? symbol #\0)) (make-set 3))
            ((and (eq? state 2) (eq? symbol #\1)) (make-set 3))
            (else (make-set))))))

; This NFA recognizes the language of strings of an even number of
; zeroes, or exactly two ones.
(define even-0-or-two-1
  (make-nfa
    0
    (lambda (state) (or (eq? state 1) (eq? state 5)))
    (lambda (symbol state)
      (cond ((and (eq? state 0) (null? symbol)) (make-set 1 3))
            ((and (eq? state 1) (eq? symbol #\0)) (make-set 2))
            ((and (eq? state 1) (eq? symbol #\1)) (make-set 1))
            ((and (eq? state 2) (eq? symbol #\0)) (make-set 1))
            ((and (eq? state 2) (eq? symbol #\1)) (make-set 2))
            ((and (eq? state 3) (eq? symbol #\1)) (make-set 4))
            ((and (eq? state 3) (eq? symbol #\0)) (make-set 3))
            ((and (eq? state 4) (eq? symbol #\1)) (make-set 5))
            ((and (eq? state 4) (eq? symbol #\0)) (make-set 4))
            ((and (eq? state 5) (eq? symbol #\0)) (make-set 5))
            (else (make-set))))))

; This NFA recognizes the language of strings in which every 0 is
; followed by at least one 1.
(define every-0-followed-by-1
  (make-nfa
    0
    (lambda (state) (or (eq? state 0) (eq? state 2)))
    (lambda (symbol state)
      (cond ((and (eq? state 0) (eq? symbol #\0)) (make-set 1))
            ((and (eq? state 0) (eq? symbol #\1)) (make-set 2))
            ((and (eq? state 1) (eq? symbol #\1)) (make-set 2))
            ((and (eq? state 2) (eq? symbol #\0)) (make-set 1))
            ((and (eq? state 2) (eq? symbol #\1)) (make-set 2))
            (else (make-set))))))

; a test function
(define (test dfa-name dfa)
  (define (iter rest)
    (if (null? rest)
        (newline)
        (begin
          (display dfa-name)
          (display " on ")
          (display (car rest))
          (display ":\t")
          (display (if (automaton-run dfa (car rest)) "accept" "reject"))
          (newline)
          (iter (cdr rest)))))
  (iter strings))

(define (main args)
  (test "start-1-end-0" start-1-end-0)
  (test "has-three-1s" has-three-1s)
  (test "has-no-110" has-no-110)
  (test "third-to-last-is-1" third-to-last-is-1)
  (test "even-0-or-two-1" even-0-or-two-1)
  (test "every-0-followed-by-1" every-0-followed-by-1)

  (exit #t))
