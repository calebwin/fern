#lang rosette

; L-system generations
(define generations '(
	(0 1 2)
	(0 1 1 0 0 1 2)
	(0 1 1 0 0 1 0 0 0 1 0 1 1 0 0 1 2)))

; alphabet of L-system
; must be the set of the range from 0 to the length of the alphabet
(define alphabet (remove-duplicates (last generations)))

; set bitwidth to n where (2^n)/2 >= (length of alphabet)
(current-bitwidth (round (exact-ceiling (log (* 2 (+ (length (last generations)) 1)) 2))))

; rules of L-system
; each rule maps a predecessor (the index of the rule) to a successor (whose length is at the index)
(define successor-lengths (map (lambda (symbol) (begin (define-symbolic* length integer?) length)) alphabet))

; (1) assert lengths are within correct bounds

(define max-rule-length (length (list-ref generations 1)))
(for ([successor-length successor-lengths])
	(assert (> successor-length 0)) 
	(assert (< successor-length max-rule-length)))

; (2) assert lengths add up to total length of each generation

(for ([i (in-naturals 0)] [generation generations])
	(when (< i (- (length generations) 1))
		(define next-gen-length (length (list-ref generations (+ i 1))))
		(define symbolic-next-gen-length 0)
		; TODO figure out why the first index of generation is "list"
		(for ([j (in-naturals 0)] [symbol generation])
			(set! symbolic-next-gen-length (+ symbolic-next-gen-length (list-ref successor-lengths symbol))))
		(assert (= symbolic-next-gen-length next-gen-length))))

; (3) same symbols yield same lengths

; look at the generation before the last
; find the positions of each symbols' successors in the next generation (the last one)
(define last-gen-successor-positions (list 0))
(define gen-before-last (list-ref generations (- (length generations) 2)))
(define gen-last (list-ref generations (- (length generations) 1)))
(define cumulative-last-gen-successor-position 0)
; TODO figure out why the first index of gen-before-last is "list"
(for ([index (in-naturals 0)] [symbol gen-before-last] #:when (< index (- (length gen-before-last) 1)))
	(set! cumulative-last-gen-successor-position (+ cumulative-last-gen-successor-position (list-ref successor-lengths symbol)))
	(set! last-gen-successor-positions (append last-gen-successor-positions (list cumulative-last-gen-successor-position))))

; get the symbolic successors of each symbol in generation before last
; TODO get expressions for symbolic successors with fewer constraints
(define last-gen-successors (list))
(for ([index (in-naturals 0)] [symbol gen-before-last])
	(set! last-gen-successors (append last-gen-successors (list (take (drop gen-last (list-ref last-gen-successor-positions index)) (list-ref successor-lengths symbol))))))

(println last-gen-successors)

; assert that each symbolic successor is equal for ones that represent the same alphabet
(for ([alphabet-symbol alphabet])
	(define gen-before-last-indices (list))
	(for ([index (in-naturals 0)] [symbol gen-before-last])
		(when (= symbol alphabet-symbol)
			(set! gen-before-last-indices (append gen-before-last-indices (list index)))))
	(for ([index (in-naturals 0)] [gen-before-last-index gen-before-last-indices] #:when (< index (- (length gen-before-last-indices) 1)))
		(assert (andmap = (list-ref last-gen-successors gen-before-last-index) (list-ref last-gen-successors (list-ref gen-before-last-indices (+ index 1)))))))

; solve for lengths
(define solution (solve (assert true)))

; print lengths
(println (evaluate solution successor-lengths))