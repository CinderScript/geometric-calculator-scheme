; DEFINITIONS

; POINT UTILITIES
(define (make-point x y) 
        (cons x y))

(define (get-x point) 
        (car point))

(define (get-y point)
        (cdr point))

;L INE UTILITIES
(define (get-slope p1 p2)
    (if (= (get-x p1) (get-x p2) ) 
        `(vertical)
        (/ (- (get-y p1) (get-y p2) ) (- (get-x p1) (get-x p2)))
    ))

(define (is-line p1 p2 p3)
        (if (equal? (get-slope p1 p2) (get-slope p2 p3))
            #t    
            #f))

(define (distance p1 p2)
    (let ( (x-diff (- (get-x p2) (get-x p1)) )
           (y-diff (- (get-y p2) (get-y p1)) ))
        (sqrt (+ (expt x-diff 2) (expt y-diff 2)))
	))

; TRIANGLE UTILITIES
(define (triangle-perimeter p1 p2 p3)
    (let ( (dist-1 (distance p1 p2)) 
           (dist-2 (distance p1 p3))
           (dist-3 (distance p2 p3)) ) 
           (+ dist-1 dist-2 dist-3)
	))

(define (triangle-area p1 p2 p3)
    (display "FIXME"))

(define (process-triangle p1 p2 p3)
    (display "FIXME"))

; SQUARE UTILITIES
(define (is-square p1 p2 p3 p4)
    (let ( (dist-1 (distance p1 p3)) 
           (dist-2 (distance p2 p4)) ) 
           (if (equal? dist-1 dist-2)
		   #t
		   #f)
	))

(define (square-perimeter p1 p2 p3 p4)
    (let ( (dist-1 (distance p1 p2)) 
           (dist-2 (distance p2 p3))
           (dist-3 (distance p3 p4))
		   (dist-4 (distance p4 p1)) ) 
           (+ dist-1 dist-2 dist-3 dist-4)
	))

(define (square-area p1 p2 p3 p4)
    (let ( (dist-1 (distance p1 p2)) 
           (dist-2 (distance p2 p3)) ) 
           (* dist-1 dist-2)
	))




(newline)
(newline)
