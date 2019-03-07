#lang rosette

; a b
; a b b a
; a b b a b a a b

; 1 + 2 = 3
; 1 + 2 + 2 + 1 = 6
; 1 + 2 + 2 + 1 + 2 + 1 + 1 + 2 = 12

(define gen-1 (vector 1 2))             ; total = 3
(define gen-2 (vector 1 2 2 1))         ; total = 6
(define gen-3 (vector 1 2 2 1 2 1 1 2)) ; total = 12

(define alphabet '(1 2))

(define one-successor `(1 2))
(define two-successor `(2 1))

(define-symbolic one-successor-length integer?)
(define-symbolic two-successor-length integer?)

; assert lengths within bounds of second generation
(assert (> one-successor-length 0))
(assert (< one-successor-length 4))
(assert (> two-successor-length 0))
(assert (< two-successor-length 4))

(define gen-two-length 0)
(set! gen-two-length (+ gen-two-length one-successor-length))
(set! gen-two-length (+ gen-two-length two-successor-length))
(define gen-three-length 0)
(set! gen-three-length (+ gen-three-length one-successor-length))
(set! gen-three-length (+ gen-three-length two-successor-length))
(set! gen-three-length (+ gen-three-length two-successor-length))
(set! gen-three-length (+ gen-three-length one-successor-length))

; assert lengths of rules add up to lengths of generation successors
(assert (= gen-two-length 4))
(assert (= gen-three-length 8))

(define final-gen-successor-posns `(
	(0)
	(+ 0 one-successor-length)
	(+ 0 (+ one-successor-length two-successor-length))
	(+ 0 (+ one-successor-length (+ two-successor-length two-successor-length)))))

(define soln (solve (assert true)))
(define soln-one-successor-length (evaluate one-successor-length soln))
(define soln-two-successor-length (evaluate two-successor-length soln))

(println soln-one-successor-length)
(println soln-two-successor-length)