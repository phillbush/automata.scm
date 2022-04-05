#!/usr/bin/env scheme-r7rs
(import (scheme small))
(load "automata.scm")

; First, we define some strings.
; Our alphabet are the characters 0 and 1.
(define strings
  (list
    "010101"
    "010000"
    "110100"
    "101101"
    "100001"
    "000111"))

; Then, we define some automata to recognize those strings.
; We label the automata with distinct names.
(define automata
  (list

    ; This NFA is created from a regular expression which recognizes the
    ; language of strings beginning with 1 and ending with 0.
    (cons "start-1-end-0"
      (regexp->nfa
        (make-cat #\1 (make-cat (make-star (make-union #\0 #\1)) #\0))))

    ; This NFA is created from a regular expression which recognizes the
    ; language of strings whose third-to-last symbol is 1
    (cons "third-to-last-is-1"
      (regexp->nfa
        (make-cat (make-star (make-union #\0 #\1))
                  (make-cat #\1
                           (make-cat (make-union #\0 #\1)
                                     (make-union #\0 #\1))))))

    ; This DFA recognizes the language of string with at least three 1's
    (cons "has-three-1s"
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
    (cons "has-no-110"
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

    ; This NFA recognizes the language of strings of an even number of
    ; zeroes, or exactly two ones.
    (cons "even-0-or-two-1"
      (make-nfa
        0
        (lambda (state) (or (eq? state 1) (eq? state 5)))
        (lambda (symbol state)
          (cond ((and (eq? state 0) (empty-string? symbol))  (make-set 1 3))
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

    ; This NFA recognizes the language of strings which ends in 1.
    (cons "ends-in-1"
      (make-nfa
        0
        (lambda (state) (eq? state 1))
        (lambda (symbol state)
          (cond ((and (eq? state 0) (eq? symbol #\0)) (make-set 0))
                ((and (eq? state 0) (eq? symbol #\1)) (make-set 0 1))
                (else (make-set))))))

    ; This PDA recognizes the language of n zeroes followed by n ones,
    ; where n is any natural number.  Note that the stack is initialized
    ; with a special symbol signaling the end of stack.  We manipulate
    ; the stack directly with cons, car, and cdr.
    (cons "n-zeros-n-ones"
      (make-pda
        0
        (lambda (state) (or (eq? state 0) (eq? state 2)))
        (lambda (symbol pd-pair)
          (let ((state (pd-state pd-pair))
                (stack (pd-stack pd-pair)))
            (cond 
                  ((and (eq? state 0) (eq? symbol #\0))
                   (make-set (cons 0 (cons #\0 stack))))
                  ((and (eq? state 0) (eq? symbol #\1) (eq? (car stack) #\0))
                   (make-set (cons 1 (cdr stack))))
                  ((and (eq? state 1) (eq? symbol #\1) (eq? (car stack) #\0))
                   (make-set (cons 1 (cdr stack))))
                  ((and (eq? state 1) (empty-string? symbol) (end-symbol? (car stack)))
                   (make-set (cons 2 (cdr stack))))
                  (else
                    (make-set)))))))

    ; This PDA recognizes the language of strings of the form 0^i+1^j+0^k
    ; (i zeros, j ones and k zeros), in which i = j or i = k.
    (cons "i-zeros-j-ones-k-zeros"
      (make-pda
        0
        (lambda (state) (or (eq? state 2) (eq? state 5)))
        (lambda (symbol pd-pair)
          (let ((state (pd-state pd-pair))
                (stack (pd-stack pd-pair)))
            (cond ((and (eq? state 0) (eq? symbol #\0))
                   (make-set (make-pd-pair 0 (cons #\0 stack))))
                  ((and (eq? state 0) (empty-string? symbol))
                   (make-set (make-pd-pair 1 stack)
                             (make-pd-pair 3 stack)))
                  ((and (eq? state 1) (eq? symbol #\1) (eq? (car stack) #\0))
                   (make-set (make-pd-pair 1 (cdr stack))))
                  ((and (eq? state 1) (empty-string? symbol) (end-symbol? (car stack)))
                   (make-set (make-pd-pair 2 (cdr stack))))
                  ((and (eq? state 2) (eq? symbol #\0))
                   (make-set (make-pd-pair 2 stack)))
                  ((and (eq? state 3) (eq? symbol #\1))
                   (make-set (make-pd-pair 3 stack)))
                  ((and (eq? state 3) (empty-string? symbol))
                   (make-set (make-pd-pair 4 stack)))
                  ((and (eq? state 4) (eq? symbol #\0) (eq? (car stack) #\0))
                   (make-set (make-pd-pair 4 (cdr stack))))
                  ((and (eq? state 4) (empty-string? symbol) (end-symbol? (car stack)))
                   (make-set (make-pd-pair 5 (cdr stack))))
                  (else
                    (make-set)))))))

    ; This PDA is created from a context-free grammar which recognizes
    ; the language of strings composed of n zeroes followed by n ones.
    ; That is the same language as the one recognized by a PDA before.
    (cons "n-zeros-n-ones"
      (cfg->pda
        (make-cfg #\S
          (make-rule #\S "0S1")
          (make-rule #\S ""))))

    ; This PDA is created from a context-free grammar which recognizes
    ; the languages of even-sized strings in which the right half is the
    ; reverse of the left one.
    (cons "reverse"
      (cfg->pda
        (make-cfg #\S
          (make-rule #\S "0S0")
          (make-rule #\S "1S1")
          (make-rule #\S ""))))

    ; This PDA is created from a context-free grammar which recognizes
    ; the languages of balanced parentheses (consider 0 as left
    ; parenthesis and 1 as right parenthesis).
    (cons "balanced-parentheses"
      (cfg->pda
        (make-cfg #\S
          (make-rule #\S "SS")
          (make-rule #\S "01")
          (make-rule #\S "0S1"))))

    ))

; This procedure prints the result of the application of an automaton on a string.
(define (print-test automaton string)
  (display (car automaton))
  (display " on ")
  (display string)
  (display ":\t")
  (display (if (automaton-run (cdr automaton) string) "accept" "reject"))
  (newline))

; Run defined automata on defined strings.
(define (main args)
  (for-each
    (lambda (automaton)
      (for-each
        (lambda (string)
          (print-test automaton string))
        strings)
      (newline))
    automata)
  (exit #t))
