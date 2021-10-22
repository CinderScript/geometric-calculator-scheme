(display "TEST 1: ----- make-point			(2 . 3)")
(newline)
(display (make-point 2 3))
(newline)

(display "TEST 2: ----- is-line				#t")
(newline)
(display (is-line (make-point 1 3) (make-point 2 6) (make-point 3 9)))
(newline)

(display "TEST 3: ----- is-line				#f")
(newline)
(display (is-line (make-point 2 2) (make-point 5 2) (make-point 3.5 -2)))
(newline)

(display "TEST 4: ----- distance			3.1622")
(newline)
(display (distance (make-point 1 3) (make-point 2 6)))
(newline)

(display "TEST 5: ----- perimeter			11.5440")
(newline)
(display (triangle-perimeter (make-point 2 2) (make-point 5 2) (make-point 3.5 -2)))
(newline)

(display "TEST 6: ----- area				5.9999")
(newline)
(display (triangle-area (make-point 2 2) (make-point 5 2) (make-point 3.5 -2)))
(newline)

(display "TEST 7: ----- process-triangle	5.9999")
(newline)
(process-triangle (make-point 2 2) (make-point 5 2) (make-point 3.5 -2))

(display "TEST 8: ----- is-square       	#t")
(newline)
(display (is-square (make-point 3 1) (make-point 2 4) (make-point 5 5) (make-point 6 2)))
(newline)

(display "TEST 9: ----- is-square       	#f")
(newline)
(display (is-square (make-point 2 4) (make-point 5 7) (make-point 8 4) (make-point 5 0)))
(newline)

(display "TEST 10: ----- square-perimeter  	12.64911")
(newline)
(display (square-perimeter (make-point 3 1) (make-point 2 4) (make-point 5 5) (make-point 6 2)))
(newline)

(display "TEST 11: ----- square-area    	10")
(newline)
(display (square-area (make-point 3 1) (make-point 2 4) (make-point 5 5) (make-point 6 2)))
(newline)

(display "TEST 12: ----- process-square	    side=3.16228")
(newline)
(display (process-square (make-point 3 1) (make-point 2 4) (make-point 5 5) (make-point 6 2)))
(newline)

(display "TEST 13: ----- Infinite Slope		#t")
(newline)
(display (is-line (make-point 2 2) (make-point 2 3) (make-point 2 -2)))
(newline)


(newline)
(newline)

(exit)
