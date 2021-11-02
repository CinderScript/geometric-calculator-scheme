
(newline)

; Takes a procedure argument when called
; procedure --> (procedure index element accumulator)
(define (foreach a-list procedure #!optional accumulator-initialization)
    (cond ((equal? accumulator-initialization #!default) (set! accumulator-initialization 0) )   )
    (define (list-crawler inner-list index len accumulator)
        (if 
            (not (= index len))    ;stop after last index, or on break
            (begin
                (set! accumulator (procedure index (car inner-list) accumulator))
                (list-crawler (cdr inner-list) (+ index 1) len accumulator)
            )
            accumulator             ;when finished return the accumulator
        )
    )
    ;run the crawler
    (list-crawler a-list 0 (length a-list) accumulator-initialization)   
)

(define (foreach-2d list-2d procedure )
    (define (crawler ))
)

(define (get a-list index)
    (define (getter i e accumulator)
        (if 
            ; if we get to the correct index
            ; return the element
            ; else return the previous accumulator
            (equal? i index) 
            e
            accumulator
        )
    )
    (foreach a-list getter 0)
)

(define (print i e accumulator)
    (display "Index: ")
    (display i)
    (display ",  Element: ")
    (display e)
    (display )
    (newline)
)

(define (product i e accumulator)
    (* accumulator e)
)
(define (sum i e accumulator)
    (+ accumulator e)
)

(define array `( (2 4 6 8 10)
                 (20 40 60 80 100)
                 (1 3 5 7 9)
                 (10 30 50 70 90)
                 (1 2 3 4 5 6 7 8 9 0) )
)

(newline)
(newline)
(display (car array))
(newline)
(newline)


(exit)