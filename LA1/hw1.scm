; Diego Dominguez
; CS 354 Programming Languages 
; Jim Buffenbarger
; September 16, 2025



; Definition: Repeats item based on count and adds the rest of list after it
; Params: item (any value), count (number, dictates how many times to repeat), tail (list, the rest after the repeats)
; Returns: a new list with item repeated by the count number of times, then the tail list at the end
; Recursion:calls itself when count is grater than 0, decreasing count by 1 each time
(define (consN item count tail)
  (if (<= count 0) tail
      (cons item (consN item (- count 1) tail))))



; Definition: Takes a list and repeasts each top level element count times from left to right. Does not look
; inside of the list.
; Params: lst (list, can by empty or have sublists), count (integer >= 0)
; Returns: a new list where each element of LST appears count times in order
; Recursion: duper calls itself on cdr of the list
(define (duper lst count)
  (cond ((null? lst) '())
        (else (consN (car lst) count
                     (duper (cdr lst) count)))))



; definition: Goes though a list, checks if each element is a pair (list), if so then it calls super-duper to fully exapnd it,
; if not then it just leaves it unchanged
; Params: lst (list that will be scanned thorugh), count (integer >= 0)
; Returns: A new list where every sublist inside lst is expanded by super duper, and atoms are left as is
; Recursion: calls itself on the cdr of the list
(define (expand-sublists lst count)
  (if (null? lst) '()
      (let ((first (car lst)))
        (cons (if (pair? first)
                  (super-duper first count)  
                  first)                  
              (expand-sublists (cdr lst) count)))))



; Definition: If source is an atom, it just returns as is, if its a list it first expands all sublists, then calls duper to repeac each element count times.
; if its just empty then we just return ()
; Params: source (any value) count (integer >= 0)
; Returns: atoms are unchanged, a new list where each eleemnt appears by the count number of times, and all sublists are fully expanded,
; and empty lists are just returned as ()
; Recursion: calles expand sublists and duper, which both call themselves recursively
(define (super-duper source count)
  (cond
    ((null? source) '())

    ; top level atoms are unchanged
    ((or (number? source) (string? source) (boolean? source) (symbol? source))
     source)

    ; if list, then call duper
    ((pair? source)
     (duper (expand-sublists source count) count))

    (else source)))





(newline)
(display "----------------------------------------------------------------------------------------------------\n")
(display "Test cases implemented:\n")
(display "----------------------------------------------------------------------------------------------------\n")
(display "STRINGS:\n")
(display "OUTPUT: ")
(display (super-duper "Boise_State" 2))
(display "   EXPECTED: Boise_State")
(newline)
(newline)

(display "OUTPUT: ")
(display (super-duper '(x "Boise_State" y) 2))
(display "   EXPECTED: (x x Boise_State Boise_State y y)\n")
(newline)
(newline)

(display "OUTPUT: ")
(display (super-duper '(x ("Boise_State") y) 2))
(display "   EXPECTED: (x x (Boise_State Boise_State) (Boise_State Boise_State) y y)\n")
(newline)
(newline)

(display "BOOLEANS:\n")
(display "OUTPUT: ")
(display (super-duper #t 5))
(display "   EXPECTED: #t")
(newline)
(newline)
(display "OUTPUT: ")
(display (super-duper #f 3))
(display "   EXPECTED: #f")
(newline)
(newline)

; single element lists
(display "OUTPUT: ")
(display (super-duper '(#t) 2))
(display "   EXPECTED: (#t #t)")
(newline)
(newline)

(display "OUTPUT: ")
(display (super-duper '(#f) 3))
(display "   EXPECTED: (#f #f #f)")
(newline)
(newline)

; two element lists
(display "OUTPUT: ")
(display (super-duper '(#t #f) 1))
(display "   EXPECTED: (#t #f)")
(newline)
(newline)
(display "OUTPUT: ")
(display (super-duper '(#t #f) 2))
(display "   EXPECTED: (#t #t #f #f)")
(newline)
(newline)
; count is 0
(display "COUNT IS 0:\n")
(display "OUTPUT: ")
(display (super-duper '(x y) 0))
(display "   EXPECTED: ()")
(newline)
(newline)
(display "OUTPUT: ")
(display (super-duper '() 0))
(display "   EXPECTED: ()")
(newline)
(newline)
(display "OUTPUT: ")
(display (super-duper 123 0))
(display "   EXPECTED: 123")
(newline)
(newline)
(display "DEEP NESTED LIST:\n")
(display "OUTPUT: ") 
(display (super-duper '((a (b c)) d) 2)) 
(display "   EXPECTED: ((a (b b c c) (b b c c)) (a (b b c c) (b b c c)) d d)")
(newline)
(newline)
(display "MIXED DATA TYPES:\n")
(display "OUTPUT: ")
(display (super-duper '(x "sup" #t (y z) 69) 2))
(display "   EXPECTED: (x x sup sup #t #t (y y z z) (y y z z) 69 69)")
(newline)
(newline)





(display "----------------------------------------------------------------------------------------------------\n")
(display "Test cases that was given to us in the assignment\n")
(display "----------------------------------------------------------------------------------------------------\n")
(display "Numbers:\n")
(display (super-duper 123 1)) ; 123
(newline)
(display (super-duper 123 2)) ;  123
(newline)
(newline)

(display "Empty lists:\n")
(display (super-duper '() 1)) ;  ()
(newline)
(display (super-duper '() 2)) ; ()
(newline)
(newline)

(display "Single element lists:\n")
(display (super-duper '(x) 1)) ;  (x)
(newline)
(display (super-duper '(x) 2)) ; (x x)
(newline)
(newline)

(display "Two element lists:\n")
(display (super-duper '(x y) 1)) ; (x y)
(newline)
(display (super-duper '(x y) 2)) ;  (x x y y)
(newline)
(newline)

(display "Nested lists:\n")
(display (super-duper '((a b) y) 3)) ;  ((a a a b b b) (a a a b b b) (a a a b b b) y y y)
(newline)
(newline)

(exit)
