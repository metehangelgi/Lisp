;; load sudoku code
(load "sudoku.scm")

;;; Cells

; Cell = Pair<tag-symbol, List<int>>

(define (make-cell)
  ; makes an instance of a cell, with values from 1 to *size*, with tag
  ; type: -> Cell
  (cons 'cell (generate-interval 1 *size*)))

; accessors

; get possible values for the cell
(define (cell-values cell) (cdr cell))


; mutators

; set possible values for the cell
(define (set-cell-values! cell values)
  (set-cdr! cell values))


;; to start building the grid, create a row of cells of desired size

;;;;;;;;;;;;;;;;;;;
;;; Rows

; Row = List<Cell>

(define (make-row n)
  ; makes a row of n cells
  ; type: int -> Row
  (if (= n 0)
      '()
      (cons (make-cell) (make-row (- n 1)))))

;; to continue building the grid, create desired number of rows of desired size

(define (make-rows n)
  ; make a list of n rows
  ; type: int -> List<Row>
  (if (= n 0)
      '()
      (cons (make-row *size*) (make-rows (- n 1)))))


;;;;;;;;;;;;;;;;;;;;
;;; Problem 1
;;;;;;;;;;;;;;;;;;;;
;;;; Columns
;
;; Column = List<Cell>
;
;;; given a grid represented as a list of rows, need to collect
;;; elements of each row into a column 
;
;;; YOU WILL NEED TO FILL IN make-column (Problem 1)
;

(define (make-column n rows)
  ; collect the nth element of each row into a column
  ; n specifies which element to collect, starting from 0
  ; type: int,List<Row> -> Column
  (define (get-n lst n)
    (if (= n 0)
        (car lst)
        (get-n (cdr lst) (- n 1))))
  (map (lambda (row) (get-n row n)) rows))
;
(define (make-columns rows)
  ; given a list of rows, make a list of columns from it
  ; each column should share cells with each row
  ; type: List<Row> -> List<Column>
  (define (helper n rows)
    (if (= n *size*)
        '()
        (cons (make-column n rows) (helper (+ n 1) rows))))
  (helper 0 rows))

;;;;;;;;;;;;;;;;;;;;
;;; Problem 2 
;;;;;;;;;;;;;;;;;;;;
;;;; Regions
;
;; Region = List<Cell>
;
(define (make-regions rows)
  ; given a list of rows, make a list of regions from it.
  ; type: List<Row> -> List<Region>
  (if (null? rows)
      '()
      (append (make-some-regions (first-n rows (region-size)))
              (make-regions (but-first-n rows (region-size))))))
;
;;; YOU WILL NEED TO FILL IN make-some-regions for Problem 2
;
(define (make-some-regions some-rows)
  ; takes a few rows of the grid (a number of rows equal to (region-size))
  ; and returns their regions.
  ; type: List<Row> -> List<Region>
  
  (define (rows-to-regions some-rows)
    (if (null? (car some-rows)) '()
        (cons (map (lambda (row) (first-n row (region-size))) some-rows)
              (rows-to-regions (map (lambda (row) (but-first-n row (region-size))) some-rows)))))
  
  (define (flatten x)
    (cond ((null? x) '())
          ((eq? (car x) 'cell) (list x))
          (else (append (flatten (car x))
                        (flatten (cdr x))))))
  
  (map (lambda (region) (flatten region)) (rows-to-regions some-rows)))
 
;;; create test rows, columns, and grid
(define *size* 9)
(define test-rows (make-rows *size*))
(define test-columns (make-columns test-rows))
(define test-regions (make-regions test-rows))

;;;;;;;;;;;;;;;;;;;
;;; Grid

; Grid = List: tag, List<Row>, List<Column>, List<Region>

(define (make-grid)
  (let ((rows (make-rows *size*)))
    (list 'grid rows (make-columns rows) (make-regions rows))))

;; here are some accessors for a game grid

(define (first lst)
  (car lst))

(define (second lst)
  (cadr lst))

(define (third lst)
  (caddr lst))

(define (fourth lst)
  (cadddr lst))

(define (get-rows grid)
  (second grid))

(define (get-columns grid)
  (third grid))

(define (get-regions grid)
  (fourth grid))

(define (get-cell grid r c)
  ; gets the cell at row r, column c in the grid.
  ; (counting from 0).
  ; type: Grid,int,int -> Cell
  (let* ((rows (get-rows grid))
         (row (list-ref rows r))
         (cell (list-ref row c)))
    cell))

;; mutator for game grid
(define (set-value! grid r c val)
  (set-cell-values! (get-cell grid r c) (list val)))

;; special mutator for testing game grid construction
(define (set-value-row! rows r c val)
  (let ((row (list-ref rows r)))
    (let ((cell (list-ref row c)))
      (set-cell-values! cell (list val)))))

;; display function for game grid
(define (display-grid grid)
  ;; display the state of the grid.
  ;; if there is a unique answer, it prints it, otherwise it prints a dot
  ;; type: Grid -> void
  (define (print-dashed-line) 
    (display (make-string (+ (* 2 *size*) (* 2 (region-size)) 1) #\-)) 
    (newline))
  
  (define (printable-value cell) 
    (let ((values (cell-values cell)))
      (if (= (length values) 1) (first values) ".")))
  
  (define (print-values cells)
    (for-each (lambda (cell) (printf "~a " (printable-value cell))) cells))
  
  (define (print-row row)
    (display "| ")
    (if (null? row)
        (newline)
        (begin (print-values (first-n row (region-size)))
               (print-row (but-first-n row (region-size))))))
  
  (define (print-rows rows)
    (print-dashed-line)
    (if (not (null? rows))
        (begin (for-each print-row (first-n rows (region-size)))
               (print-rows (but-first-n rows (region-size))))))
  
  ;; main body of display-grid: print the rows of the grid
  (newline)
  (print-rows (get-rows grid)))

;;;;;;;;;;;;;;;;;;;
;;; Initial values

; Initial values are the values already filled into the grid at the start of the game.

; InitialValues = List< row,col,val >
;    each element in this list is a triple <r,c,val>
;    where r and c are row and column indexes (counting from 0)
;      and val is the number found in that cell

; initial values for a 4x4 grid
(define *initial-9x9-values* 
  '((0 0 3)       (0 2 2)
                  (1 1 1)
                  (2 0 4)
                  (3 1 3)
                  (3 3 2)))

; initial values for a 9x9 grid
(define *initial-9x9-values* 
  '(        (0 1 6)       (0 3 1)       (0 5 4)      (0 7 5)
                          (1 2 8)(1 3 3)       (1 5 5)(1 6 6)
                          (2 0 2)                                              (2 8 1)
                          (3 0 8)             (3 3 4)       (3 5 7)            (3 8 6)
                          (4 2 6)                     (4 6 3)
                          (5 0 7)             (5 3 9)       (5 5 1)            (5 8 4)
                          (6 0 5)                                              (6 8 2)
                          (7 2 7)(7 3 2)       (7 5 6)(7 6 9)
                          (8 1 4)       (8 3 5)       (8 5 8)      (8 7 7)))


(define (set-initial-values! grid initial-values)
  ; sets the initial values into a grid
  ; assumes grid has just been created
  ; type: Grid, InitialValues -> void
  (for-each (lambda (entry) (set-value! grid (first entry) (second entry) (third entry)))
            initial-values))

;;;;;;;;;;;;;;;;;;;
;;; void was used in assignment code, but not defined in R5RS
;;; it alway returns null
(define (void)
  (if (= 1 0)
      'hi
      '()))

;;;;;;;;;;;;;;;;
;;; Problem 3
;;;;;;;;;;;;;;;;
; Problem 3 enables the player to backtrack during the game. Code is incorporated into the complete 
; game code at the end of the assignment.
  
;;;:::::::::::::::::
;;; Problem 4
;;;;;;;;;;;;;;;;;;;;
;
(define (get-unique-values cells)
  ;; given a list of cells
  ;; return the cell values, if any, that are 
  ;; unique to their cells
  (if (null? cells)
      '()
      (let ((next-cell (car cells)))
        (if (= (length (cell-values next-cell)) 1)
            (cons (car (cell-values next-cell)) (get-unique-values (cdr cells)))
            (get-unique-values (cdr cells))))))

(define (in? val lst)
  ;; return #t if val is an element of lst
  (cond ((null? lst) #f)
        ((= val (car lst)) #t)
        (else (in? val (cdr lst)))))

(define (not-in lst1 lst2)
  ;; return the elements of lst1 that are not in lst2
  (define (not-in-iter lst1 lst2 not-in)
    (cond ((null? lst1) not-in)
          ((not (in? (car lst1) lst2)) (not-in-iter (cdr lst1) lst2 (cons (car lst1) not-in)))
          (else (not-in-iter (cdr lst1) lst2 not-in))))
  (reverse (not-in-iter lst1 lst2 '())))

(define (remove-dupes cells)
  ; given a list of cells:
  ; returns list with values removed from each cell that 
  ; are uniquely determined in another cell in the list
  (let ((unique-values (get-unique-values cells)))
      (for-each (lambda (cell)
                  (if (= (length (cell-values cell)) 1)
                      (set-cell-values! cell (cell-values cell))
                      (set-cell-values! cell (not-in (cell-values cell) unique-values)))) cells)))

(define (simplify-grid grid)
  (for-each (lambda (row) (remove-dupes row)) (get-rows grid))
  (for-each (lambda (column) (remove-dupes column)) (get-columns grid))
  (for-each (lambda (region) (remove-dupes region)) (get-regions grid)))

;;;;;;;;;;;;;;;;;;;;;;;
;;; Problem 5 
;;;;;;;;;;;;;;;;;;;;;;;
(define (cell-length cell)
  (length (cell-values cell)))

(define (cell-lengths row)
  (map (lambda (cell) (cell-length cell)) row))

(define (cell-lengths-rows rows)
  (map (lambda (row) (cell-lengths row)) rows))

(define (iterate-simplify-grid grid)
  (let ((cell-lengths-rows-old (cell-lengths-rows (get-rows grid)))
        (cell-lengths-columns-old (cell-lengths-rows (get-columns grid)))
        (cell-lengths-regions-old (cell-lengths-rows (get-regions grid))))
      (for-each (lambda (region) (remove-dupes region)) (get-regions grid))
      (for-each (lambda (row) (remove-dupes row)) (get-rows grid))
      (for-each (lambda (column) (remove-dupes column)) (get-columns grid))
  (if (not (and (equal? cell-lengths-rows-old (cell-lengths-rows (get-rows grid)))
                (equal? cell-lengths-columns-old (cell-lengths-rows (get-columns grid)))
                (equal? cell-lengths-regions-old (cell-lengths-rows (get-regions grid)))))
      (iterate-simplify-grid grid))))

;;;;;;;;;;;;;;;;;;;;;;;
;;; Problem 6
;;;;;;;;;;;;;;;;;;;;;;;  


(define (get-unique-values-columns columns)
  ;; input: list of columns
  ;; output: list of lists of unique values that appear in each column, 
  ;; length of output list is equal to number of columns with at least one unique value
  ;; unique values may be repeated among the sub-lists
  (define (helper columns)
   (map (lambda (column) (get-unique-values column)) columns))
  (define (accumulate uniques non-empty)
    (cond ((null? uniques) non-empty)
          ((null? (car uniques)) (accumulate (cdr uniques) non-empty))
          (else (accumulate (cdr uniques) (cons (car uniques) non-empty)))))
  (reverse (accumulate (helper columns) '())))


; (display "Test get-unique-values-columns:") (newline)
; (get-unique-values-columns get-unique-values-columns-test)

(define (count-times-unique uniques-per-column value)
  ;; input: value (integer)
  ;; output: number of columns for which value is uniquely determined
  (define (helper uniques-per-column value n)
    (cond ((null? uniques-per-column) n)
          ((in? value (car uniques-per-column))
           (helper (cdr uniques-per-column) value (+ n 1)))
          (else (helper (cdr uniques-per-column) value n))))
  (helper uniques-per-column value 0))

(define (get-n-1-uniques columns values)
  ;; input: columns
  ;; output: values that are uniquely determine in n-1 columns, where n = region size
  ;; values will be repeated in the output list
  (define (remove-nulls lst no-nulls)
    ;; input: list lst
    ;; output: lst with null values removed
    (cond ((null? lst) no-nulls)
          ((null? (car lst)) (remove-nulls (cdr lst) no-nulls))
          ((remove-nulls (cdr lst) (cons (car lst) no-nulls)))))
    (remove-nulls (map (lambda (value) (if (= (count-times-unique (get-unique-values-columns columns) value) (- (region-size) 1)) value '())) values) '()))

(define (n-1-others columns)
  ;; input: columns
  ;; output: list of lists that contain the values in each column that 
  ;; are uniquely determined in n-1 columns, but not in the current column
  (define (possibles column-uniques uniques-n-1 candidates)
    ;; input: column-uniques: uniques values for column
    ;;        uniques-n-1: values that are unique in n-1 columns
    ;;        candidates: initially null
    ;; output: candidates: list of values in uniques-n-1 but not in column-uniques
    (cond ((null? uniques-n-1) candidates)
          ((not (in? (car uniques-n-1) column-uniques)) (possibles column-uniques (cdr uniques-n-1) (cons (car uniques-n-1) candidates)))
          (else (possibles column-uniques (cdr uniques-n-1) candidates))))
  ;; apply possibles to each column
  (map (lambda (column) (possibles (get-unique-values column) (get-n-1-uniques columns (generate-interval 1 *size*)) '())) columns))

(define (column-help columns)
  (if (null? columns)
      '()
      (append (n-1-others (first-n columns (region-size)))
              (column-help (but-first-n columns (region-size))))))

;;;;;;;;;;;;;;;;;;;;;
;; Problem 6: End
;;;;;;;;;;;;;;;;;;;;;
 

;;;;;;;;;;;;;;;;;;;;
;; Problem 7
;;;;;;;;;;;;;;;;;;;;

(define (in lst1 lst2)
  ;; return the elements of lst1 that are in lst2
  (define (in-iter lst1 lst2 in)
    (cond ((null? lst1) in)
          ((in? (car lst1) lst2) (in-iter (cdr lst1) lst2 (cons (car lst1) in)))
          (else (in-iter (cdr lst1) lst2 in))))
  (reverse (in-iter lst1 lst2 '())))

(define (vals-in col vals)
  ;; inputs: column, a list of cells
  ;;         vals, a list of values
  ;; output: a list of sublists that correspond to the cells of column
  ;;         each sublist contains the cell-values that are contained in vals
  (map (lambda (cell) (in (cell-values cell) vals)) col))

(define (promising-cells columns values)
  (define (helper columns values good-starts)
    (if (null? columns) good-starts
        (helper (cdr columns) (cdr values) (cons (vals-in (car columns) (car values)) good-starts))))
  (reverse (helper columns values '())))

;;;;;;;;;;;;;;;;;;;;;
;; Problem 7: End
;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;
;; Problem 8
;;;;;;;;;;;;;;;;;;;;;

;; Implement logic in Problems 6 and 7 for rows. 
;; See do-help below for code. 
;;;;;;;;;;;;;;;;;;;;;
;; Problem 8: End
;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;
;; Complete game code
;;;;;;;;;;;;;;;;;;;;;
  
(define (play-with-backtrack initial-values)
  ; Play Sudoku with the user.
  ; type: InitialValues -> void
  ; let commands be 
  (let ((commands '())
        (grid (make-grid)))
    (set-initial-values! grid initial-values)
    (display-grid grid)
    ;; go to driver loop
    (driver-loop-with-backtrack grid commands initial-values)))

(define (driver-loop-with-backtrack grid commands initial-values)
  ; Get one command from the user and do it on the grid.
  ; type: Grid -> void
  (define (do-quit) 
    ;; user is ready to quit
    (void)
    ;; the procedure void is used to return a non-printing value(define (play-with-backtrack initial-values)
    )
  ; Play Sudoku with the user.
  ; type: InitialValues -> void
  ; let commands be 
  (let ((commands '())
        (grid (make-grid)))
    (set-initial-values! grid initial-values)
    (display-grid grid)
    ;; go to driver loop
    (driver-loop-with-backtrack grid commands initial-values)))

(define (driver-loop-with-backtrack grid commands initial-values)
  ; Get one command from the user and do it on the grid.
  ; type: Grid -> void
  (define (do-quit) 
    ;; user is ready to quit
    (void)
    ;; the procedure void is used to return a non-printing value
    )
  
  (define (do-set)
    ;; user wants to set a value in the grid
    (newline)
    (printf "What row?")
    (let ((r (read)))
      (newline)
      (printf "What column?")
      (let ((c (read)))
        (newline)
        (printf "What value?")
        (let ((v (read)))
          (set-value! grid r c v)
          ;; update commands
          (set! commands (cons (list r c v) commands))
          (display (cell-lengths-rows (get-rows grid)))
          (newline)
          ;; update other cell values
          (iterate-simplify-grid grid)
          (printf "~nHere is your change ~n")
          (display-grid grid)
          (driver-loop-with-backtrack grid commands initial-values)))))
  
  (define (do-backtrack commands initial-values)
    (define (do-backtrack-helper grid update-commands commands)
      (if (null? update-commands)
          (begin (display-grid grid)
                 (driver-loop-with-backtrack grid (cdr commands) initial-values))
          (begin (let ((update-command (car update-commands)))
                   (set-value! grid (first update-command) (second update-command) (third update-command)))
                 (iterate-simplify-grid grid)
                 (do-backtrack-helper grid (cdr update-commands) commands))))
    (let ((grid (make-grid))
          (update-commands (reverse (cdr commands))))
      (set-initial-values! grid initial-values)
      (do-backtrack-helper grid update-commands commands)))
  
  (define (do-help grid)
    (let ((help (column-help (get-columns grid)))
          (row-help (column-help (get-rows grid))))
      ;; (display help) (newline)
      ;;; begin functions for pretty printing     
      (define (print-help values)
        (for-each (lambda (value) (printf "~a \t " value)) values))
      (define (print-help-row row)
        (newline)
        (if (null? row)
            (newline)
            (begin (print-help (first-n row 1))
                   (print-help (but-first-n row 1)))))
      (define (print-help-rows rows)
        (if (not (null? rows))
            (begin (for-each print-help-row (first-n rows 1))
                   (print-help-rows (but-first-n rows 1)))))
      ;;; end functions for pretty printing
      (display "Column-based help: ") (newline)
      (print-help-rows (make-columns (promising-cells (get-columns grid) help))) (newline)
      ;; Problem 8
      (display "Row-based help: ") (newline)
      (print-help-rows (promising-cells (get-rows grid) row-help))) (newline)
    (driver-loop-with-backtrack grid commands initial-values))

  (define (do-unknown input)
    ;; unrecognized command
    (printf "~n Don't recognize character ~a. Try again." input)
    (driver-loop-with-backtrack grid commands initial-values))
  
  (printf "~nEnter a command, one of (q s b h)~n")
  
  (let ((input (read)))
    (cond ((eq? input 'q) (do-quit))
          ((eq? input 's) (do-set))
          ((eq? input 'b) (do-backtrack commands initial-values))
          ((eq? input 'h) (do-help grid))
          (else (do-unknown input)))))


(define *size* 9)
(play-with-backtrack *initial-9x9-values*)











  




