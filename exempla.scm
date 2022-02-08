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
    "00000"
    "0000"))

; This DFA recognizes the language of strings beginning with 1 and
; ending with 0.  All our states are symbols like 'qx.
(define start-1-end-0
  (make-dfa
    'q0
    (lambda (state) (eq? state 'q2))
    (lambda (symbol state)
      (cond ((and (eq? state 'q0) (eq? symbol #\0)) 'q3)
            ((and (eq? state 'q0) (eq? symbol #\1)) 'q1)
            ((and (eq? state 'q1) (eq? symbol #\0)) 'q2)
            ((and (eq? state 'q1) (eq? symbol #\1)) 'q1)
            ((and (eq? state 'q2) (eq? symbol #\0)) 'q2)
            ((and (eq? state 'q2) (eq? symbol #\1)) 'q1)
            ((and (eq? state 'q3) (eq? symbol #\0)) 'q3)
            ((and (eq? state 'q3) (eq? symbol #\1)) 'q3)
            (else #f)))))

; This DFA recognizes the language of string with at least three 1's
(define has-three-1s
  (make-dfa
    'q0
    (lambda (state) (eq? state 'q3))
    (lambda (symbol state)
      (cond ((and (eq? state 'q0) (eq? symbol #\0)) 'q0)
            ((and (eq? state 'q0) (eq? symbol #\1)) 'q1)
            ((and (eq? state 'q1) (eq? symbol #\0)) 'q1)
            ((and (eq? state 'q1) (eq? symbol #\1)) 'q2)
            ((and (eq? state 'q2) (eq? symbol #\0)) 'q2)
            ((and (eq? state 'q2) (eq? symbol #\1)) 'q3)
            ((and (eq? state 'q3) (eq? symbol #\0)) 'q3)
            ((and (eq? state 'q3) (eq? symbol #\1)) 'q3)
            (else #f)))))

; This DFA recognizes the language of strings which does not have the
; substring "110" in it.
(define has-no-110
  (make-dfa
    'q0
    (lambda (state)
      (or (eq? state 'q0)
          (eq? state 'q1)
          (eq? state 'q2)))
    (lambda (symbol state)
      (cond ((and (eq? state 'q0) (eq? symbol #\0)) 'q0)
            ((and (eq? state 'q0) (eq? symbol #\1)) 'q1)
            ((and (eq? state 'q1) (eq? symbol #\0)) 'q0)
            ((and (eq? state 'q1) (eq? symbol #\1)) 'q2)
            ((and (eq? state 'q2) (eq? symbol #\0)) 'q3)
            ((and (eq? state 'q2) (eq? symbol #\1)) 'q2)
            ((and (eq? state 'q3) (eq? symbol #\0)) 'q3)
            ((and (eq? state 'q3) (eq? symbol #\1)) 'q3)
            (else #f)))))

; This NFA recognizes the language of strings whose third-to-last symbol is 1
(define third-to-last-is-1
  (make-nfa
    'q0
    (lambda (state) (eq? state 'q3))
    (lambda (symbol state)
      (cond ((and (eq? state 'q0) (eq? symbol #\0)) (list 'q0))
            ((and (eq? state 'q0) (eq? symbol #\1)) (list 'q0 'q1))
            ((and (eq? state 'q1) (eq? symbol #\0)) (list 'q2))
            ((and (eq? state 'q1) (eq? symbol #\1)) (list 'q2))
            ((and (eq? state 'q2) (eq? symbol #\0)) (list 'q3))
            ((and (eq? state 'q2) (eq? symbol #\1)) (list 'q3))
            (else #f)))))

; This NFA recognizes the language of strings of k zeroes, where k is a
; multiple of 2 or 3
(define k-zeroes
  (make-nfa
    'q0
    (lambda (state) (or (eq? state 'q1) (eq? state 'q3)))
    (lambda (symbol state)
      (cond ((and (eq? state 'q0) (null? symbol)) (list 'q1 'q3))
            ((and (eq? state 'q1) (eq? symbol #\0)) (list 'q2))
            ((and (eq? state 'q2) (eq? symbol #\0)) (list 'q1))
            ((and (eq? state 'q3) (eq? symbol #\0)) (list 'q4))
            ((and (eq? state 'q4) (eq? symbol #\0)) (list 'q5))
            ((and (eq? state 'q5) (eq? symbol #\0)) (list 'q3))
            (else #f)))))

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
  (test "k-zeroes" k-zeroes)

  (exit #t))
