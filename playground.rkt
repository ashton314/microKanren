#lang racket/base

(require "kanren.rkt")

(define (father p s)
  (conde ((== p 'paul) (== s 'jason))
         ((== p 'john) (== s 'henry))
         ((== p 'jason) (== s 'tom))
         ((== p 'peter) (== s 'brian))
         ((== p 'tom) (== s 'peter))))

(define (grandfather g s)
  (fresh (p) (conj+ (father g p) (father p s))))

;; Run to find out who's who's grandfather:
;; (run* (rel p s) (conj+ (grandfather p s) (== (cons p s) rel)))
;; => '((paul . tom) (jason . peter) (tom . brian))
