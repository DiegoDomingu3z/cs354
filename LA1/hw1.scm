; Diego Dominguez
; CS 354 Programming Languages 
; Jim Buffenbarger
; September 16, 2025


; args: source (any type), count (number)
; source is what will be copied and count is the number of times it will copy
(define (copy source count)
  (cond ((number? source) (make-list 1 source))   ; If its is a number, it makes a list containing just one copy of that number
        ((null? source)   (make-list 1 source))   ; if source is empty it will just make empty list
        ((string? source) (make-list 1 source))   ; If source is a string, it makes a list containing that string one time
        ((boolean? source)(make-list 1 source))   ; if source is bool it will just make list with it one time
        (else (make-list count source))))         ; if its a list then it will make a list of that list the num of count times 


; recursive for super duper
; args: item (any), count (number), list
(define (cons-n item count list)
  (if (<= count 0) ; if count is 0 or less than just return the same list
      list
      (cons item (cons-n item (- count 1) list))))



;; super-duper source (any) count (number)
(define (super-duper source count)
  (cond
    ((null? source) '()) ; if empty list just return it

    ;; if its a number, string, boolean or symbol just return it once
    ((or (number? source) (string? source) (boolean? source) (symbol? source))
     source)

    ;; if list then go through each element recursively
    ((pair? source)
     (let rec ((lst source))
       (if (null? lst)
           '()
           (let* ((first (car lst))
                  (rest-expanded (rec (cdr lst))))
             (if (pair? first)                          ; sublist
                 (cons-n (super-duper first count) count rest-expanded)
                 (cons-n first count rest-expanded))))))
    (else source)))



; -------------------------
; Test cases that implemented
; -------------------------
(newline)
(display "-------------------------\n")
(display "Test cases implemented:\n")
(display "-------------------------\n")
(display "Strings:\n")
(display (super-duper "Boise_State" 2)) ; → "Boise_State"
(newline)
(display (super-duper '(x "Boise_State" y) 2)) ; → (x "Boise_State" "Boise_State" y y)
(newline)  
(newline)
(display (super-duper '(x ("Boise_State") y) 2)) ; → ((x x ("Boise_State" "Boise_State") ("Boise_State" "Boise_State") y y)
(newline)
(newline)
(display "Booleans:\n")
(display (super-duper #t 5))     ; -> #t
(newline)
(display (super-duper #f 3))     ; -> #f
(newline)(newline)

; single element lists
(display (super-duper '(#t) 2))  ; -> (#t #t)
(newline)
(display (super-duper '(#f) 3))  ; -> (#f #f #f)
(newline)(newline)

; two element lists
(display (super-duper '(#t #f) 1)) ; → (#t #f)
(newline)
(display (super-duper '(#t #f) 2)) ; → (#t #t #f #f)
(newline)
(newline)

(newline)
(newline)





; -------------------------
; Test cases that was given to us in the assignment
; -------------------------
(display "-------------------------\n")
(display "Test cases that was given to us in the assignment\n")
(display "-------------------------\n")
(display "Numbers:\n")
(display (super-duper 123 1)) ; → 123
(newline)
(display (super-duper 123 2)) ; → 123
(newline)
(newline)

(display "Empty lists:\n")
(display (super-duper '() 1)) ; → ()
(newline)
(display (super-duper '() 2)) ; → ()
(newline)
(newline)

(display "Single element lists:\n")
(display (super-duper '(x) 1)) ; → (x)
(newline)
(display (super-duper '(x) 2)) ; → (x x)
(newline)
(newline)

(display "Two element lists:\n")
(display (super-duper '(x y) 1)) ; → (x y)
(newline)
(display (super-duper '(x y) 2)) ; → (x x y y)
(newline)
(newline)

(display "Nested lists:\n")
(display (super-duper '((a b) y) 3)) ; → ((a a a b b b) (a a a b b b) (a a a b b b) y y y)
(newline)
(newline)

(exit)
