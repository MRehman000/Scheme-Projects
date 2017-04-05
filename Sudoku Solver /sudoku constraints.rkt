;Sudoku
;Muhammad Rehman and Suman Islam

;Easy Difficulty, Solvable
(define board '(5 3 - - 7 - - - -
                6 - - 1 9 5 - - -
                - 9 8 - - - - 6 -
                8 - - - 6 - - - 3
                4 - - 8 - 3 - - 1
                7 - - - 2 - - - 6
                - 6 - - - - 2 8 -
                - - - 4 1 9 - - 5
                - - - - 8 - - 7 9))

;Medium difficulty, solvable
(define medboard    '(1 - - - - 7 - 4 -
                      - - - - - 5 9 - 7
                      - - - - - 9 - 2 8
                      - - 9 - - - - - 5
                      7 - - - 3 - - - 9
                      5 - - - - - 3 - -
                      4 1 - 6 - - - - -
                      9 - 8 2 - - - - -
                      - 5 - 9 - - - - 2))

;Hard Difficulty, not solvable by our function
(define hardboard   '(8 - - - - - - - -
                      - - 3 6 - - - - -
                      - 7 - - 9 - 2 - -
                      - 5 - - - 7 - - -
                      - - - - 4 5 7 - -
                      - - - 1 - - - 3 -
                      - - 1 - - - - 6 8
                      - - 8 5 - - - 1 -
                      - 9 - - - - 4 - -))

(define los '(1 2 3 4 5 6 7 8 9 0 -))
               
(define (displayln lst)
  (begin
    (display lst)
    (display "\n")))

(define (board? board)
    (and (equal? (length board) 81) (covered? board los) (lat? board)))


;-----------------------------------------------
;Basic helper functions

(define (member? a lst)
  (cond
    ((null? lst) #f)
    ((equal? a (car lst))
     #t)
    (else
     (member? a (cdr lst)))))

(define (covered? lexp los)
  (cond
    ((null? lexp) #t)
    ((member? (car lexp) los)
     (and #t (covered? (cdr lexp) los)))
    (else
     #f
     )))       

(define (lat? lst)
  (cond
    ((null? lst) #t)
    ((atom? (car lst))
     (lat? (cdr lst)))
    (else
     #f)))

(define (atom? x)
  (and (not (null? x)) (not (pair? x))))

(define (flatten x)
  (cond ((null? x) '())
        ((pair? x) (append (flatten (car x)) (flatten (cdr x))))
        (else (list x))))


;---------------------------------------------------------------
;Functions that will return board, row, column, and boxes
;---------------------------------------------------------------

;Pre: A list of length 81
;Post: A list of length 9, which is the x-th row of the board
(define (row x board)
  (define (row-help x i)
    (cond
      ((equal? -1 i) (quote()))
      (else
       (list (row-help x (- i 1)) (list-ref board (+ i x))))))
  (flatten (row-help (* x 9) 8)))


;Pre: A list of length 81
;Post: A list of length 9, which is the x-th column of the board
(define (col x board)
  (define (column-help x i)
    (cond
      ((> 0 i) (quote()))
      (else
       (cons (column-help x (- i 9)) (list-ref board (+ i x))))))
  (flatten (column-help x 72)))



;Pre: A list of length 81
;Post: A list of length 9, which is the x-th box of the board
(define (box x board)
  (cond
    ((or (equal? x 0) (equal? x 1) (equal? x 2))
     (get-box-values (* x 3) board))
    ((equal? x 3)
     (get-box-values 27 board))
    ((equal? x 4)
     (get-box-values 30 board))
    ((equal? x 5)
     (get-box-values 33 board))
    ((equal? x 6)
     (get-box-values 54 board))
    ((equal? x 7)
     (get-box-values 57 board))
    ((equal? x 8)
     (get-box-values 60 board))
    (else
     (display "invalid box number"))))

;Pre: x value to refer to a quadrant inside the board
;Post: Return a list of 9 elements of the box
(define (get-box-values x board)
  (list (list-ref board x)
        (list-ref board (+ x 1))
        (list-ref board (+ x 2))
        (list-ref board (+ x 9))
        (list-ref board (+ x 10))
        (list-ref board (+ x 11))
        (list-ref board (+ x 18))
        (list-ref board (+ x 19))
        (list-ref board (+ x 20))))


;Pre: A list of 81 elements
;Post: A nicely formatted sudoku board
(define (displayboard board)
  (define (display-help x)
    (cond
      ((equal? -1 x) (display ""))
      (else
       (begin
         (display-help (- x 1))
         (displayln (row x board))))))
  (display-help 8))

;-----------------------------------------------
;End functions to display row,col,box, and board
;-----------------------------------------------

;-----------------------------------------------
;Functions to confirm if the list does not have
;repeating numbers
;-----------------------------------------------

;Pre: A list of length 9
;Post: True if the row does not have repeating numbers
;      False if there are repeating numbers
(define (check-row? row)
  (cond
    ((null? row) #t)
    ((equal? (car row) '-)
     (check-row? (cdr row)))
    ((not (member? (car row) (cdr row)))
     (check-row? (cdr row)))
    (else
     #f)))

(define check-col? check-row?)

(define check-box? check-row?)


;---------------------------------------------------
;Functions to help us get the position of an element
;---------------------------------------------------


(define (index-to-row index)
  (floor (/ index 9)))

(define (index-to-col index)
  (modulo index 9))

(define (pos index)
  (list (index-to-row index) (index-to-col index)))

(define (pos-to-index x y)
  (+ (* x 9) y))

(define (coords-to-box x y)
  (+ (* 3 (quotient x 3)) (quotient y 3)))



;list-set! function will set the kth index in a list
;equal to value. NOTE: (Found online), not my work
(define (list-set! list k val)
    (if (zero? k)
        (set-car! list val)
        (list-set! (cdr list) (- k 1) val)))


;Pre: coords is a list of length 2, board is a sudoku board
;Post: Return the value of the element at the coordinates
(define (get-value coords board)
  (list-ref board (pos-to-index (car coords)
                          (cdr coords))
            ))


;This function will find all the missing positions on the board
;It will return a list containing all of the coordinates of the missing
;positions on the board.
;Pre: A sudoku board, which is a list of length 81
;Post: A list containing coordinates of all the empty cells in the board
(define (empty-cell board)
  (let loop ((i 0)
             (count 81)
             (missing '()))
    (cond
      ((zero? count) (reverse missing))
      ((number? (list-ref board i))
       (loop (+ i 1) (- count 1) missing))
      (else
       (loop (+ i 1) (- count 1) (cons (pos i) missing))))))


;Pre: 2 lists of any size
;Post: A list with all the elements of l2, subtracted from l1
; Example (list-difference '(a b c d e f) '(a b f))  ==> (c d e)
(define (list-difference l1 l2)
  (cond ((null? l1)
         '())
        ((not (member (car l1) l2))
         (cons (car l1) (list-difference (cdr l1) l2)))
        (else
         (list-difference (cdr l1) l2))))


(define nums '(1 2 3 4 5 6 7 8 9 -))

;=====================================================
;This is the function used for our constraints
;The constraints for the numbers are that the number...
;   1. Must not be repeated inside a row
;   2. Must not be repeated inside a column
;   3. Must not be repeated inside a box
;We use list-difference to get these numbers by removing
;all present numbers from the row, col, and box. 

;Pre: An x coordinate, y coordinate, and a sudoku board
;Post: Get all possible numbers that can fit into this cell
(define (possible-nums x y board)
  (let* ((index (pos-to-index x y))
        (num (list-ref board index)))
  (if (number? (list-ref board index))
      (list num)
      (begin
          (list-difference
           (list-difference
           (list-difference nums (row x board)) (col y board)) (box (coords-to-box x y) board))))))



;==========================================================================
;This is our main function.
;This function attempts to solve the sudoku board
;It does this by looping through the empty cells.
;It finds if an empty cell can only accept one value,
;If the cell can only accept one and only one value, we assign
;that value to the board and continue on to the next missing cell.
;If we see that multiple values fit there, this does not fit our constraint,
;and therefore we skip it until it is ready to be filled in.

;Pre: A sudoku board
;Post: A solved sudoku board, or #f if board is not solvable
(define (solve-board board)
  (let loop ((empty-cells (empty-cell board)))
    (if (null? empty-cells)
        (begin
          (search board))
        (begin
          (let* ((missing-num (car empty-cells))
                 (x (car missing-num))
                 (y (car (cdr missing-num)))
                 (possible (possible-nums x y board)))
            (if (null? possible)
                  #f
                (if (null? (cdr possible))
                    (begin
                      (list-set! board (pos-to-index x y) (car possible))
                      (loop (empty-cell board)))
                    (loop (cdr empty-cells)))))))))
  
;============================================================================
;This is a search function for more complicated puzzles
;This function is called when there are no constraints on any
;of the empty cells in the board. This means that there is more than
;one possible value for each cell. This function tries to guess what that
;value will be. The function takes one of the possibilites and tries to solve
;the board with that possibility. If that does not work, it goes back and
;tries to solve with the next possibility.
;NOTE: This function is not fully working!
(define (search board)
  (cond
    ((not (member? '- board)) ;meaning we don't have any empty spaces
      (displayboard board))
    (else
     (let loop ((empty-cells (empty-cell board)))
    (if (null? empty-cells)
           #f
        (begin 
          (let* ((missing-num (car empty-cells))
                 (x (car missing-num))
                 (y (car (cdr missing-num)))
                 (possible (possible-nums x y board)))
                (begin
                  (let newloop ((values possible)
                                (tmpboard board))
                    (if (null? values)
                        (loop (cdr empty-cells))
                            (begin
                              (list-set! tmpboard (pos-to-index x y) (car values))
                             (displayboard tmpboard)
                             (newline)
                              (if (solve-board tmpboard) (displayboard tmpboard)
                                  (begin
                                    (list-set! tmpboard (pos-to-index x y) '-)
                                    (newloop (cdr values) tmpboard)))
                                  ))
                              ))))
                  )))))
                 
   
(displayln "Easy Board")
(solve-board board)
#| Desired Output
(5 3 4 6 7 8 9 1 2)
(6 7 2 1 9 5 3 4 8)
(1 9 8 3 4 2 5 6 7)
(8 5 9 7 6 1 4 2 3)
(4 2 6 8 5 3 7 9 1)
(7 1 3 9 2 4 8 5 6)
(9 6 1 5 3 7 2 8 4)
(2 8 7 4 1 9 6 3 5)
(3 4 5 2 8 6 1 7 9)
|#

(displayln "Medium Board")
(solve-board medboard)
#|Desired Output
(1 9 3 8 2 7 5 4 6)
(8 2 4 1 6 5 9 3 7)
(6 7 5 3 4 9 1 2 8)
(2 3 9 4 8 1 6 7 5)
(7 4 1 5 3 6 2 8 9)
(5 8 6 7 9 2 3 1 4)
(4 1 2 6 5 8 7 9 3)
(9 6 8 2 7 3 4 5 1)
(3 5 7 9 1 4 8 6 2)
|#

;(displayln "Hard Board")
;(solve-board hardboard)
#| Output

#f

|#
;Output is #f because our program fails to solve it.
;If you would like, you can run (solve-board hardboard)
;I left in the debugging framwork so you can see how it is working. 

;==============================================================================
;The hardboard is an example of a board that cannot be solved by the program
;The reason being is that their is no constraint on some of the empty cells.
;Our program searches through each cell but fails to find a possible constraint
;Now how do we solve this problem? We can solve through a search function,
;We have that implemented, however it is not fully functional.
;Many hours were spent debugging and trying to fix this function, but we had no
;luck fixing it.
;Since this project is focused on the propogation of constraints, we've decided
;to leave it how it is, to dedicate more time towards the completion of the
;research.
;===============================================================================

    


