; ideas
; -- symbolically execute L-systems to prune them early
; -- use some sort of metric to determine whether a symbolic L-system is satisfactory w/o execution
; -- compare frequencies of each alphabet in first generation of symbolic output and given string
; -- compare frequencies of each alphabet in first generation of symbolic output and given string across percent ranges of strings
; -- 

; define all possible successors
; -- subsequences of given string
; -- valid branch cut from tree

; define possible successors for each symbol
; -- must be one of all possible successors
; -- must have length such that constraint for final length is sat for symbolic lengths of other symbols

; optimizations
; -- prune search space of candidate L-systems
; -- vary maximum length of candidate successors for different symbols
; -- vary maximum edit distance
; -- assert length of generation

#lang rosette

(require rosette/lib/angelic)

; ----------------
; HELPER FUNCTIONS
; ----------------

; flattens a multi-dimensional list
(define (flatten-symbolic lst)
  (cond ((null? lst) '())
        ((pair? lst)
         (append (flatten-symbolic (car lst)) (flatten-symbolic (cdr lst))))
        (else (list lst))))

; returns differences in frequency percentages of 2 sequences for each symbol in an alphabet
(define (frequency-differences a b alphabet)
	(define a-frequencies (map
		(lambda (symbol-in-alphabet)
			(* (length b) (foldl
				(lambda (symbol curr-frequency)
					(cond
						[(equal? symbol symbol-in-alphabet) (+ curr-frequency 1)]
						[else curr-frequency]))
				0
				a)))
		alphabet))
	(define b-frequencies (map
		(lambda (symbol-in-alphabet)
			(* (length a) (foldl
				(lambda (symbol curr-frequency)
					(cond
						[(equal? symbol symbol-in-alphabet) (+ curr-frequency 1)]
						[else curr-frequency]))
				0
				b)))
		alphabet))
	(map
		(lambda (expected-frequency actual-frequency)
			(abs (- expected-frequency actual-frequency)))
		a-frequencies b-frequencies)
	)

; get subsequences of given sequence
(define (subsequences sequence max-length)
	(define set-combinations (list))
	(for/list ([combination-length (in-range (floor max-length))])
		(for/list ([start-index (in-range (+ 1 (- (length sequence) combination-length)))])
			(define new-combination (take (list-tail sequence start-index) combination-length))
			(set! set-combinations (append set-combinations (list new-combination)))))
	(remove-duplicates set-combinations))

; apply given L-system to given generation
; return results
(define (interpret-once generation system)
	(append-map
		(lambda (symbol) (list-ref system symbol))
		generation))
(define (interpret-repeat generation iterations system)
	(define result generation)
	(for/list ([i (in-range iterations)])
		(set! result (interpret-once result system)))
	result)
(define (interpret generation iterations system)
	(define res generation)
	(for-each
		(lambda (i)
			(define new-res '())
			(for-each
				(lambda (symbol)
					(set! new-res (append new-res (list-ref system symbol))))
				res)
			(set! res new-res))
		(take (list 1 2 3 4 5) iterations))
	)
(define (approx-interpret generation iterations system)
	(define result generation)
	(set! result (append-map (lambda (symbol) (list-ref system symbol)) result))
	(set! result (append-map (lambda (symbols) (append-map (lambda (symbol) (list-ref system symbol)) symbols)) result))
	result
	)

; determines an approximate distance between 2 sequences
; intended to emulate edit distance
(define (lcs a b solver)
	(solver-push solver)
	(define-symbolic* a-take integer?)
	(define-symbolic* a-drop integer?)
	(define-symbolic* b-take integer?)
	(define-symbolic* b-drop integer?)
	(solver-assert solver (list (equal? (take (list-tail a a-drop) a-take) (take (list-tail b b-drop) b-take))))
	(solver-maximize solver (list (length (take (list-tail a a-drop) a-take))))
	(define sol (solver-check solver))
	(define solved-a-take (evaluate a-take sol))
	(define solved-a-drop (evaluate a-drop sol))
	(define distance (length (take (list-tail a solved-a-drop) solved-a-take)))
	(solver-pop solver)
	distance
	)

; determines an approximate distance between 2 sequences
; intended to emulate edit distance
(define (hamming a b)
	(cond
		[(< (length a) (length b)) (begin
			(define distance 0)
			(define index 0)
			(for-each
				(lambda (symbol)
					(set! distance (+ distance (cond [(equal? symbol (list-ref b index)) 0] [else 1])))
					(set! index (+ index 1)))
				a)
			distance
			)]
		[else (begin
			(define distance 0)
			(define index 0)
			(for-each
				(lambda (symbol)
					(set! distance (+ distance (cond [(equal? symbol (list-ref a index)) 0] [else 1])))
					(set! index (+ index 1)))
				b)
			distance
			)])
	)

; determines if give string represents branch
(define (branch? possible-successor)
	(define bracket-depth 0)
	(define is-valid #t)
	(for-each
		(lambda (symbol)
			(set! bracket-depth (+ bracket-depth (cond
				[(equal? symbol 0) 1]
				[(equal? symbol 1) -1]
				[else 0])))
			(set! is-valid (cond
				[(! is-valid) is-valid]
				[else (>= bracket-depth 0)])))
		possible-successor)
	(and
		(>= (length possible-successor) 1)
		(equal? (count (lambda (symbol) (equal? symbol 0)) possible-successor) (count (lambda (symbol) (equal? symbol 1)) possible-successor))
		(! (equal? (take possible-successor 1) 1))
		(! (&& (equal? (length possible-successor) 1) (equal? (take possible-successor 1) 2)))
		(! (&& (equal? (length possible-successor) 1) (equal? (take possible-successor 1) 3)))
		(! (equal? (take-right possible-successor 1) 0))
		(! (equal? (take-right possible-successor 1) 2))
		(! (equal? (take-right possible-successor 1) 3))
		is-valid
		)
	)

; encode and decode
(define (encode raw)
	(map
		(lambda (symbol) (cond
			[(char=? symbol #\[) 0]
			[(char=? symbol #\]) 1]
			[(char=? symbol #\+) 2]
			[(char=? symbol #\-) 3]
			[(char=? symbol #\F) 4]
			[(char=? symbol #\X) 5]
			))
		(string->list raw)))
(define (decode code)
	(list->string (map
		(lambda (symbol) (cond
			[(equal? symbol 0) #\[]
			[(equal? symbol 1) #\]]
			[(equal? symbol 2) #\+]
			[(equal? symbol 3) #\-]
			[(equal? symbol 4) #\F]
			[(equal? symbol 5) #\X]
			))
		code)))

; -----
; INPUT
; -----

; (define target-generation (encode "[-FX]+FX"))
; (define target-generation (encode "FF+[+F-F-F]-[-F+F+F]"))
; (define target-generation (list 4 0 2 5 1 4 0 3 5 1 2 5))
(define target-generation (list 4 4 0 2 4 0 2 5 1 4 0 3 5 1 2 5 1 4 4 0 3 4 0 2 5 1 4 0 3 5 1 2 5 1 2 4 0 2 5 1 4 0 3 5 1 2 5))
(define initial-generation (encode "X")) ; initial stem that synthesized program will be applied to to generate trees/strings
(define num-applications 2) ; # of times synthesized program will be applied to generate trees/strings

; -------------------------
; PRUNE POSSIBLE SUCCESSORS
; -------------------------

; solver
(define solver (current-solver))

; get alphabet
; get average successor length
; get subsequences of target tree/string
(define alphabet (sort (remove-duplicates target-generation) <))
(define symbolic-out-of-total (/ (count (lambda (symbol) (<= symbol 3)) target-generation) (length target-generation)))
(define overall-ratio-length-increase (/ (length target-generation) (length initial-generation)))
(define ratio-length-increase (expt (length target-generation) (/ 1 num-applications)))
(define average-symbolic-successor-length (+ (/ (- ratio-length-increase 1) symbolic-out-of-total) 1))
(define max-successor-length (* average-symbolic-successor-length (- (length alphabet) 4)))
(define target-subsequences (subsequences target-generation max-successor-length))

; define successors of each symbol
; initially set possible successors of each symbol to be subsequences of target
(define successors (map
	(lambda (symbol) target-subsequences)
	alphabet))

; filter out strings that don't represent a branch cut from given tree
; filter out by length
(set! successors (map
	(lambda (successor-set)
		(filter
			branch?
			successor-set))
	successors))

; filter out simple changes
(set! successors (map
	(lambda (successor-set)
		(filter
			branch?
			successor-set))
	successors))

; define symbolic successors
(define symbolic-successors (map
	(lambda (symbol) (cond
		[(<= symbol 3) (list symbol)]
		[else (begin
			; define a symbolic successor  for this symbol
			; as index of element in list of successors for this symbol
			(define-symbolic* symbolic-successor-index integer?)
			(list-ref (list-ref successors symbol) symbolic-successor-index))]))
	alphabet))

; -------------------
; CONSTRAINTS FOR N=1
; -------------------

; ; define symbolic generation
; (define symbolic-generation (approx-interpret initial-generation num-applications symbolic-successors))

; ; define symbolic distance from target generation
; (define symbolic-hamming (hamming target-generation symbolic-generation))
; ; (define symbolic-lcs (lcs target-generation symbolic-generation solver))
; (define symbolic-distance 0)
; ; (define symbolic-distance (+ symbolic-hamming (abs (- (length target-generation) (length symbolic-generation)))))
; ; (define symbolic-distance (- (length target-generation) (length symbolic-generation)))
; ; (define symbolic-distance (+ (- (length target-generation) symbolic-lcs) (- (length symbolic-generation) symbolic-lcs)))

; ; constrain symbolic successors
; (assert (<= symbolic-distance (ceiling (/ (length target-generation) 2))))

; ---------------------
; CONSTRAINTS FOR N > 1
; ---------------------

; gets generation after first application of symbolic L-system
(define symbolic-first-application-generation (interpret-once initial-generation symbolic-successors))

; gets differences in frequency between generation after first application and given target generation
; a frequency is a number which when divided by a * b where a and b are lengths of 2 generations being compared gives the normalized percentage frequency
(define symbolic-frequency-differences (frequency-differences symbolic-first-application-generation target-generation alphabet))

; constrain differences in frequency
(for-each
	(lambda (frequency-difference)
		(assert (<= frequency-difference (* 0.1 (length symbolic-first-application-generation) (length target-generation))))
		)
	symbolic-frequency-differences)

; -------------------------
; SOLVE AND DISPLAY RESULTS
; -------------------------

; solve for solution successors
(define sol (solve (begin void)))
(define sol-successors (evaluate symbolic-successors sol))

; print results
(display "initial generation:   ")
(displayln (decode initial-generation))
(display "target generation:    ")
(displayln (decode target-generation))
(display "synthesized L-system: ")
(displayln (map decode sol-successors))
(display "example generation:   ")
(displayln (decode (interpret-repeat initial-generation num-applications sol-successors)))
