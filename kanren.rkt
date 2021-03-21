#lang racket/base
(provide (all-defined-out))

;; Taken from the μKanren paper by Hemann and Friedman
;; Resources:
;;  - http://minikanren.org/
;;  - http://webyrd.net/scheme-2013/papers/HemannMuKanren2013.pdf

;;;-------------------------
;;; µKanren Core

;; Variables
(define (var c) (vector c))
(define (var? c) (vector? c))
(define (var=? x₁ x₂) (= (vector-ref x₁ 0) (vector-ref x₂ 0)))

;; Search for a variable's value in substitution
(define (walk u s)
  (let ([pr (and (var? u) (assp (λ (v) (var=? u v)) s))])
    (if pr (walk (cdr pr) s) u)))

;; Extend a variable
(define (ext-s x v s) `((,x . ,v) . ,s))

;; equiv constructors (the symbol ≡ can be typed `\equiv` when TeX
;; input is turned on in Emacs)
(define (≡ u v)
  (λ (s/c)
    (let ([s (unify u v (car s/c))])
      (if s (unit `(,s . ,(cdr s/c))) mzero))))

;; An alias for ease of typing
(define == ≡)

;; I think these constructs function essentially as the `unit` in the
;; list monad. See also bind and mplus.
(define (unit s/c) (cons s/c mzero))
(define mzero '())

;; Unify: the first of our basic constructors
(define (unify u v s)
  (let ([u (walk u s)]
        [v (walk v s)])
    (cond
      [(and (var? u) (var? v) (var=? u v)) s]
      [(var? u) (ext-s u v s)]
      [(var? v) (ext-s v u s)]
      [(and (pair? u) (pair? v))
       (let ([s (unify (car u) (car v) s)])
         (and s (unify (cdr u) (cdr v) s)))]
      [else (and (eqv? u v) s)])))

;; call/fresh: the second basic constructor
(define (call/fresh f)
  (λ (s/c)
    (let ([c (cdr s/c)])
      ((f (var c)) `(,(car s/c) . ,(+ c 1))))))

;; The remaining two basic goal constructors
(define (disj g₁ g₂) (λ (s/c) (mplus (g₁ s/c) (g₂ s/c))))
(define (conj g₁ g₂) (λ (s/c) (bind (g₁ s/c) g₂)))

;; mplus: merge two streams
(define (mplus $₁ $₂)
  (cond
    [(null? $₁) $₂]
    ;; Return a thunk to implement lazy "immature" streams.
    ;; Note how we swap the order of $₁ and $₂ to interleave the
    ;; streams.
    [(procedure? $₁) (λ () (mplus $₂ ($₁)))]
    [else (cons (car $₁) (mplus (cdr $₁) $₂))]))

(define (bind $ g)
  (cond
    [(null? $) mzero]
    [(procedure? $) (λ () (bind ($) g))]
    [else (mplus (g (car $)) (bind (cdr $) g))]))

;; Needed because Racket doesn't implement R6RS's assp function
;; natively afaik
(define (assp ? lst)
  (if (null? lst)
      #f
      (if (? (caar lst)) (car lst) (assp ? (cdr lst)))))

;;;-------------------------
;;; User-level Functionality

;; "Snooze": automate the inverse-η-delay
(define-syntax Zzz
  (syntax-rules ()
    [(_ g) (λ (s/c) (λ () (g s/c)))]))

;; Variadic conj and disj
(define-syntax conj+
  (syntax-rules ()
    [(_ g) (Zzz g)]
    [(_ g0 g ...) (conj (Zzz g0) (conj+ g ...))]))

(define-syntax disj+
  (syntax-rules ()
    [(_ g) (Zzz g)]
    [(_ g0 g ...) (disj (Zzz g0) (disj+ g ...))]))

(define-syntax conde
  (syntax-rules ()
    [(_ (g0 g ...) ...)
     (disj+ (conj+ g0 g ...) ...)]))

(define-syntax fresh
  (syntax-rules ()
    [(_ () g0 g ...)
     (conj+ g0 g ...)]
    [(_ (x0 x ...) g0 g ...)
     (call/fresh (λ (x0) (fresh (x ...) g0 g ...)))]))

;; Stream->List functionality
(define (pull $) (if (procedure? $) (pull ($)) $))

(define (take-all $)
  (let ([$ (pull $)])
    (if (null? $) '() (cons (car $) (take-all (cdr $))))))

(define (take n $)
  (if (zero? n) '()
      (let ([$ (pull $)])
        (cond
          [(null? $) '()]
          [else (cons (car $) (take (- n 1) (cdr $)))]))))

;; Reification utilities
(define (mK-reify s/c*)
  (map reify-state/1st-var s/c*))

(define (reify-state/1st-var s/c)
  (let ([v (walk* (var 0) (car s/c))])
    (walk* v (reify-s v '()))))

(define (reify-s v s)
  (let ([v (walk v s)])
    (cond
      [(var? v)
       (let ([n (reify-name (length s))])
         (cons `(,v . ,n) s))]
      [(pair? v) (reify-s (cdr v) (reify-s (car v) s))]
      [else s])))

(define (reify-name n)
  (string->symbol (string-append "_" "." (number->string n))))

(define (walk* v s)
  (let ([v (walk v s)])
    (cond
      [(var? v) v]
      [(pair? v) (cons (walk* (car v) s) (walk* (cdr v) s))]
      [else v])))

;; Host interface
(define empty-state '(() . 0))
(define (call/empty-state g) (g empty-state))

(define-syntax run
  (syntax-rules ()
    [(_ n (x ...) g0 g ...)
     (mK-reify (take n (call/empty-state
                        (fresh (x ...) g0 g ...))))]))

(define-syntax run*
  (syntax-rules ()
    [(_ (x ...) g0 g ...)
     (mK-reify (take-all (call/empty-state
                          (fresh (x ...) g0 g ...))))]))
