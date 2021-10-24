; DEFINITIONS

; POINT Functions
(define (make-point x y) 
        (cons x y))

(define (get-x point) 
        (car point))

(define (get-y point)
        (cdr point))

;LINE Functions
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

; TRIANGLE Functions
(define (triangle-perimeter p1 p2 p3)
    (let ( (dist-1 (distance p1 p2)) 
           (dist-2 (distance p1 p3))
           (dist-3 (distance p2 p3)) ) 
           (+ dist-1 dist-2 dist-3)
	))

; Returns the area of a triangle given 3 points
; uses Heron's formula:
; s = perimeter / 2
; root(s*(s - side1)*(s - side2)*(s - side3))
(define (triangle-area p1 p2 p3)
    (let ( (dist-1 (distance p1 p2)) 
           (dist-2 (distance p2 p3))
		   (dist-3 (distance p1 p3)) 
		   (s (/ (triangle-perimeter p1 p2 p3) 2)) ) 
           (sqrt (* s (- s dist-1) (- s dist-2) (- s dist-3) ))
	))


; SQUARE Functions
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

; PROCESS Functions
; Returns the dimensions of a square if the four points make a square
(define (process-square p1 p2 p3 p4)
    (if (is-square p1 p2 p3 p4)
        (begin
			    (let ( 	(side (distance p1 p2)) 
						(perimeter (square-perimeter p1 p2 p3 p4))
						(area (square-area p1 p2 p3 p4)) ) 
						    (display "Side = ")
							(display side)
							(newline)
							(display "Perimeter = ")
							(display perimeter)
							(newline)
							(display "Area = ")
							(display area)
							(list side perimeter area) ;return list of each value
		))
		(begin
        	(display "Not a Square")
			(list 0 0 0)
		)
    ))

; Returns the dimensions of a triangle if the three points make a triangle
(define (process-triangle p1 p2 p3)
    (if (not (is-line p1 p2 p3))	;if not a line, it is a triangle
        (begin
			    (let ( 	(dist-1 (distance p1 p2)) 
						(dist-2 (distance p2 p3))
						(dist-3 (distance p1 p3)) 
						(perimeter (triangle-perimeter p1 p2 p3))
						(area (triangle-area p1 p2 p3)) ) 
						    (display "Side 1 = ")
							(display dist-1)
							(newline)
							(display "Side 2 = ")
							(display dist-2)
							(newline)
							(display "Side 3 = ")
							(display dist-3)
							(newline)
							(display "Perimeter = ")
							(display perimeter)
							(newline)
							(display "Area = ")
							(display area)
							(list dist-1 dist-2 dist-3 perimeter area) ;return list of each value
		))
		(begin
        	(display "Not a Triangle")
			(list 0 0)
		)
    ))
