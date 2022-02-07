#!/usr/bin/env scheme-r7rs
(import (scheme small))
(load "automata")

; Let's define some strings.
; Our alphabet are the numbers 0 and 1.
(define strings
  (list
    '(0 1 0 1 0)
    '(1 1 0 1 0)
    '(1 0 0 1 1)))

; This DFA recognizes the language of strings beginning with 1 and
; ending with 0.  All our states are symbols like 'qx.
(define start-1-end-0
  (make-dfa
    'qs
    (lambda (state) (eq? state 'q0))
    (lambda (state symbol)
      (cond ((and (eq? state 'qs) (eq? symbol 0)) 'qe)
            ((and (eq? state 'qs) (eq? symbol 1)) 'q1)
            ((and (eq? state 'q1) (eq? symbol 0)) 'q0)
            ((and (eq? state 'q1) (eq? symbol 1)) 'q1)
            ((and (eq? state 'q0) (eq? symbol 0)) 'q0)
            ((and (eq? state 'q0) (eq? symbol 1)) 'q1)
            ((and (eq? state 'qe) (eq? symbol 0)) 'qe)
            ((and (eq? state 'qe) (eq? symbol 1)) 'qe)))))

; This DFA recognizes the language of string with at least three 1's
(define has-three-1s
  (make-dfa
    'q0
    (lambda (state) (eq? state 'q3))
    (lambda (state symbol)
      (cond ((and (eq? state 'q0) (eq? symbol 0)) 'q0)
            ((and (eq? state 'q0) (eq? symbol 1)) 'q1)
            ((and (eq? state 'q1) (eq? symbol 0)) 'q1)
            ((and (eq? state 'q1) (eq? symbol 1)) 'q2)
            ((and (eq? state 'q2) (eq? symbol 0)) 'q2)
            ((and (eq? state 'q2) (eq? symbol 1)) 'q3)
            ((and (eq? state 'q3) (eq? symbol 0)) 'q3)
            ((and (eq? state 'q3) (eq? symbol 1)) 'q3)))))

; This DFA recognizes the language of strings which does not have the
; substring "110" in it.
(define has-no-110
  (make-dfa
    'q0
    (lambda (state)
      (or (eq? state 'q0)
          (eq? state 'q1)
          (eq? state 'q2)))
    (lambda (state symbol)
      (cond ((and (eq? state 'q0) (eq? symbol 0)) 'q0)
            ((and (eq? state 'q0) (eq? symbol 1)) 'q1)
            ((and (eq? state 'q1) (eq? symbol 0)) 'q0)
            ((and (eq? state 'q1) (eq? symbol 1)) 'q2)
            ((and (eq? state 'q2) (eq? symbol 0)) 'q3)
            ((and (eq? state 'q2) (eq? symbol 1)) 'q2)
            ((and (eq? state 'q3) (eq? symbol 0)) 'q3)
            ((and (eq? state 'q3) (eq? symbol 1)) 'q3)))))

; a test function
(define (test dfa-name dfa)
  (define (iter rest)
    (if (null? rest)
        (newline)
        (begin
          (display dfa-name)
          (display " on ")
          (display (car rest))
          (display ":	")
          (display (dfa-run dfa (car rest)))
          (newline)
          (iter (cdr rest)))))
  (iter strings))

(define (main args)
  (test "start-1-end-0" start-1-end-0)
  (test "has-three-1s" has-three-1s)
  (test "has-no-110" has-no-110)
  (exit #t))
