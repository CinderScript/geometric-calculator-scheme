

; make-point
; Returns a value pair where the first value
; is an x-coordinate and the 2nd value is the y-coordinate
; Parameters: x: x-coordinate
;			  y: y-coordinate
; Returns:	  coordinate value pair
(define (make-point x y) 
        (cons x y))

; get-x
; Returns the x-coordinate of the given point
; Parameters: p1 = point 1 
; Returns:	  x-coordinate value
(define (get-x point) 
        (car point))

; get-y
; Returns the y-coordinate of the given point
; Parameters: p1 = point 1 
; Returns:	  y-coordinate value
(define (get-y point)
        (cdr point))

; get-slope
; Returns the slope of a line given the lines start end end point.
; If the line is vertical, then '(vertical) is returned.
; Parameters: p1 = point 1 
; 			  p2 = point 2 
; Returns:	  slope value
(define (get-slope p1 p2)
    (if (= (get-x p1) (get-x p2) ) 
        `(vertical)
        (/ (- (get-y p1) (get-y p2) ) (- (get-x p1) (get-x p2)))
    ))

; is-line
; Returns #t if the given three points make a line by
; checking if the slope between two sets of points is equal.
; Parameters: p1 = point 1 
; 			  p2 = point 2 
; Returns:	  #t if line, else #f
(define (is-line p1 p2 p3)
        (if (equal? (get-slope p1 p2) (get-slope p2 p3))
            #t    
            #f))

; distance
; Returns the distance between two points
; Parameters: p1 = point 1 
; 			  p2 = point 2 
; Returns:	  distance
(define (distance p1 p2)
    (let ( (x-diff (- (get-x p2) (get-x p1)) )
           (y-diff (- (get-y p2) (get-y p1)) ))
        (sqrt (+ (expt x-diff 2) (expt y-diff 2)))
	))

; triangle-perimeter
; returns the perimeter of a triangle given by thre points.  Assumes 
; the three points make a triangle (no check is done)
; Parameters: p1 = point 1 
; 			  p2 = point 2 
; 			  p3 = point 3
; Returns:	  perimeter distance
(define (triangle-perimeter p1 p2 p3)
    (let ( (dist-1 (distance p1 p2)) 
           (dist-2 (distance p1 p3))
           (dist-3 (distance p2 p3)) ) 
           (+ dist-1 dist-2 dist-3)
	))


; triangle-area
; returns the area of a triangle given by thre points.  Assumes 
; the three points make a triangle (no check is done)
; Parameters: p1 = point 1 
; 			  p2 = point 2 
; 			  p3 = point 3
; Returns:	  area (using Heron's formula)
(define (triangle-area p1 p2 p3)
	; Heron's formula:
	; s = perimeter / 2
	; return root(s*(s - side1)*(s - side2)*(s - side3))
    (let ( (dist-1 (distance p1 p2)) 
           (dist-2 (distance p2 p3))
		   (dist-3 (distance p1 p3)) 
		   (s (/ (triangle-perimeter p1 p2 p3) 2)) ) 
           (sqrt (* s (- s dist-1) (- s dist-2) (- s dist-3) ))
	))


; is-square
; returns #t if the four points given make a square, else returns #f
; Parameters: p1 = point 1 
; 			  p2 = point 2 
; 			  p3 = point 3
; 			  p4 = point 4
; Returns:	  #t or #f
(define (is-square p1 p2 p3 p4)
    (let ( (dist-1 (distance p1 p3)) 
           (dist-2 (distance p2 p4)) ) 
           (if (equal? dist-1 dist-2)
		   #t
		   #f)
	))

; square-perimeter
; returns the perimeter of a square given by four points.  Assumes 
; the four points make a square (no check is done)
; Parameters: p1 = point 1 
; 			  p2 = point 2 
; 			  p3 = point 3
; 			  p4 = point 4
; Returns:	  perimeter length
(define (square-perimeter p1 p2 p3 p4)
    (let ( (dist-1 (distance p1 p2)) 
           (dist-2 (distance p2 p3))
           (dist-3 (distance p3 p4))
		   (dist-4 (distance p4 p1)) ) 
           (+ dist-1 dist-2 dist-3 dist-4)
	))

; square-area
; returns the area of a square given by four points.  Assumes 
; the four points make a square (no check is done)
; Parameters: p1 = point 1 
; 			  p2 = point 2 
; 			  p3 = point 3
; 			  p4 = point 4
; Returns:	  area as length * width
(define (square-area p1 p2 p3 p4)
    (let ( (dist-1 (distance p1 p2)) 
           (dist-2 (distance p2 p3)) ) 
           (* dist-1 dist-2)
	))

; process-square
; If the four points make a triangle, process-square prints out the 
; properties of this square. Else, prints "Not a Square"
; Parameters: p1 = point 1 
; 			  p2 = point 2 
; 			  p3 = point 3
; 			  p4 = point 4
; Returns:	  (side-1 side-2 side-3 perimeter area) if triangle exists
; 			  (0 0 0 0 0) if triangle does not exist
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

; process-triangle
; If the three points make a triangle, process-triangle prints out the 
; properties of this triangle. Else, prints "Not a Triangle"
; Parameters: p1 = point 1 
; 			  p2 = point 2 
; 			  p3 = point 3
; Returns:	  (side-1 side-2 side-3 perimeter area) if triangle exists
; 			  (0 0 0 0 0) if triangle does not exist
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
			(list 0 0 0 0 0)
		)
    ))

	