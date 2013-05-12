;;===================================================;;
;;                                                   ;;
;; File:         boom-language.rkt                   ;;
;;               (Homework 12)                       ;;
;;                                                   ;;
;; Name:         Aaron Stroschein                    ;;
;; CatID:        stroscha                            ;;
;; Date Created: 4/28/13                             ;;
;;                                                   ;;
;;                                                   ;;
;; Boom Language Syntax Definitions                  ;;
;; (Homework 12 with while-loop EC)                  ;;
;;                                                   ;;
;; <exp> ::= <number>                                ;;
;;         | <boolean>                               ;;
;;         | <string>                                ;;
;;         | ( <exp> <binary-operator> <exp> )       ;;
;;                                                   ;;
;; <unary-operator> ::= - | sqrt                     ;;
;;                                                   ;;
;; <operator> ::= + | - | * | / | % | @ | < | > |    ;;
;;              | <= | gt | gte | lt | lte | => |    ;;
;;              | && | || | xor | ==                 ;;
;;                                                   ;;
;; <primitive> ::= pi | e                            ;;
;;                                                   ;;
;; <cell (name) > ::= <var>                          ;;
;;                                                   ;;
;; <with-do (var) > ::= <exp> <operator> <exp>       ;;
;;                                                   ;;
;; <list> ::= <exp+>                                 ;;
;;                                                   ;;
;; <switch-statement (var) > ::= <case (exp)> <exp+> ;;
;;                                                   ;;
;; <while-loop (while) > ::= <exp+>                  ;;
;;                                                   ;;
;; <function (name) > ::= (begin) <exp+> (end)       ;;
;;                                                   ;;
;; <display> ::= (display) exp | newline             ;;
;;                                                   ;;
;;===================================================;;

;; Accessors

(define 1st car)
(define 1st-in-list caar)
(define 2nd cadr)
(define 1st-of-the-2nd caadr)
(define 2nd-in-list cadar)
(define 3rd caddr)
(define 3rd-in-list caddar)
(define 4th cadddr)
(define 4th-in-list cdddar)
(define 2nd-and-beyond cdr)
(define 3rd-and-beyond cddr)
(define 4th-and-beyond cdddr)
(define 5th-and-beyond cddddr)


;; For function ADT

(define boom/function-list '())


;; List type-check

(define boom/list?
  (lambda (exp)
    (cond ((or (boom/number? exp)
               (boom/varref? exp)
               (boom/boolean? exp)) #f)
          (else #t))))


;; List constructor

(define boom/list
  (lambda args
    (define make-boom/list
      (lambda (items len count)
        (cond ((= len count)
               '())
              (else (cons (list-ref items count)
                          (make-boom/list items len (+ count 1)))))))
    (make-boom/list args (length args) 0)))


;; Now that we have a list-type we can store primatives as cells

(define boom-primatives (boom/list 'pi '(3.14) 'e '(2.72)))
(define top2-list boom-primatives)

;; String type-check

(define boom/string?
  (lambda (exp)
    (string? exp)))




;; Primative type-check

(define boom-primative?
  (lambda (a-cell)
    (or (= (cell-value a-cell) 3.14)
        (= (cell-value a-cell) 2.72))))


;; Primative constructors

(define boom-primative
  (lambda (exp)
    (cond ((= exp 'pi) 3.14)
          ((= exp 'e) 2.72))))   


;; Singular type-checks

(define boom/boolean?
  (lambda (exp)
    (or (eqv? exp #t)
        (eqv? exp #f))))

(define boom/number?
  (lambda (exp)
    (number? exp)))

(define boom/varref?
  (lambda (exp)
    (symbol? exp)))

(define boom/null?
  (lambda (exp)
    (null? exp)))


;; Cell type-check

(define cell?
  (lambda (exp)
    (and (or (boom/number? (1st exp))
             (boom/boolean? (1st exp))
             (boom/string? (1st exp)))
         (= (length exp) 1))))







;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                         ;
; Homework 12 - Problem 1 ;
;                         ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Cell constructor

(define cell
  (lambda (init-value)
    (boom/list init-value)))


;; Cell accessor

(define cell-value
  (lambda (a-cell)
    (1st a-cell)))


;; Cell setter

(define cell-set!
  (lambda (test-cell new-value)
    (set-car! test-cell new-value)
    (1st test-cell)))


;; Error constructor

(define error
  (lambda args
    (cons 'Error: args)))    


;; Regular operator type-check

(define boom/operator?
  (lambda (exp)
    (or (eqv? exp '+)
        (eqv? exp '-)
        (eqv? exp '*)
        (eqv? exp '/)
        (eqv? exp '%)
        (eqv? exp '=)
        (eqv? exp '==)        
        (eqv? exp '<=)
        (eqv? exp '=>)
        (eqv? exp 'gt)
        (eqv? exp 'lt)
        (eqv? exp 'lte)
        (eqv? exp 'gte)
        (eqv? exp '&&)
        (eqv? exp '||)
        (eqv? exp '!)
        (eqv? exp '!=)
        (eqv? exp 'xor))))


;; Special operator type-check

(define boom/special?
  (lambda (exp)
    (eqv? exp '@)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                  ;
; Homework 12 - Top Level Variables (Extra Credit) ;
;                                                  ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define boom/define?
  (lambda (exp)
    (eqv? exp 'define)))

(define boom/define
  (lambda (exp variable-list)
    (set! variable-list (cons (2nd exp)
                              (cons (cell (boom/eval
                                           (4th exp)))
                                    variable-list)))
    (set! top2-list (cons (2nd exp)
                          (cons (cell (boom/eval
                                       (4th exp)))
                                top2-list)))))


;; Unary type-check

(define boom/unary-operator?
  (lambda (operator)
    (cond ((or (eqv? operator '-)
               (eqv? operator 'sqrt)))
          (else #f))))


;; Unary constructors

(define boom/unary-operator
  (lambda (operator exp)
    (cond ((eqv? operator '-)
           (* -1 exp))
          ((eqv? operator 'sqrt)
           (boom/sqrt exp)))))

(define boom/sqrt
  (lambda (exp)
    (define approx-method
      (lambda (exp possible-root)
        (cond ((= (* possible-root possible-root) exp)
               possible-root)
              (else (approx-method (boom/eval exp)
                                   (+ possible-root 1))))))
    (approx-method exp 1)))


;; Singular expression type-check

(define boom/exp?
  (lambda (exp)
    (cond ((or (boom/number? exp)
               (boom/varref? exp)
               (boom/boolean? exp)
               (boom/string? exp)))
          ((and (= (length exp) 2)
                (boom/unary-operator? (1st exp))
                (boom/number? exp)))
          ((and (= (length exp) 3)
                (boom/exp? (1st exp))
                (boom/exp? (2nd exp))
                (boom/exp? (3rd exp))))
          ((and (= (length exp) 6)
                (boom/with? (1st exp))
                (boom/exp? (2nd exp))
                (boom/exp? (3rd exp))
                (boom/exp? (4th exp))
                (boom/exp? (5th-and-beyond exp))))
          ((and (= (length exp) 2)
                (boom/do? (1st exp))
                (boom/exp? (2nd exp))))
          (else error exp "is not a valid boom expression"))))


;; With/do type-checkers

(define boom/with?
  (lambda (exp) (eqv? exp 'with)))

(define boom/do?
  (lambda (exp) (eqv? exp 'do)))


;; Preprocessing method for expressions and ADTs

(define boom/preprocess
  (lambda (exp)
    (cond ((boom/null? exp)
           '())
          ((or (boom/number? exp)
               (boom/varref? exp)
               (boom/boolean? exp)
               (boom/string? exp)
               (boom/operator? exp)
               (boom/newline? exp)
               (boom/string? (1st exp))) exp)
          ((eqv? (1st exp) 'display)
           (cons (1st exp) (boom/list (boom/preprocess (2nd exp)))))
          
          ((eqv? (1st exp) 'function)
           (cons (1st exp) (cons (2nd exp) (list (boom/preprocess (3rd exp))))))
          
          ((eqv? (1st exp) 'define)
           (cons (boom/preprocess (1st exp))
                 (boom/preprocess (2nd-and-beyond exp))))
          ((eqv? (1st exp) 'with)
           (cons (boom/preprocess (1st exp))
                 (boom/preprocess (2nd-and-beyond exp))))
          ((eqv? (1st exp) 'do)
           (cons (boom/preprocess (1st exp))
                 (boom/list (boom/preprocess (2nd exp)))))
          ((eqv? (1st exp) 'begin)
           (cons (boom/preprocess (1st exp))
                 (cons (boom/preprocess (2nd exp))
                       (boom/preprocess (3rd-and-beyond exp)))))
          ((eqv? (1st exp) 'end) exp)
          ((eqv? (1st exp) 'boom/list)
           (cons (boom/preprocess (1st exp))
                 (boom/preprocess (2nd exp))))
          ((or (eqv? (1st exp) 'else) (eqv? (1st exp) 'if))
           (cons (1st exp) (process-if (2nd-and-beyond exp))))
          ((eqv? (1st exp) 'while)
           (cons (1st exp) (boom/preprocess (2nd-and-beyond exp))))
          ((eqv? (1st exp) 'switch)
           (cons (1st exp)
                 (cons (boom/preprocess (2nd exp))
                       (boom/list (process-cases (3rd exp)
                                                 (length (3rd exp)) 0)))))
          ((eqv? (1st exp) 'case)
           (cons (1st exp) (boom/preprocess (cdr exp))))
          ((eqv? (1st exp) 'default)
           (cons (1st exp)
                 (cons (2nd exp)
                       (boom/list (boom/preprocess (3rd exp))))))
          ((boom/unary-operator? (1st exp))
           (cons (1st exp)
                 (cons (boom/preprocess (2nd exp))
                       (boom/preprocess (3rd exp)))))
          ((boom/special? (2nd exp))
           (boom/list (boom/list (boom/preprocess (1st exp)) '+
                                 (boom/preprocess (3rd exp))) '/ 2))
          ((boom/operator? (2nd exp))
           (cons (boom/preprocess (1st exp))
                 (cons (2nd exp)
                       (cons (boom/preprocess (3rd exp))
                             (boom/preprocess (4th-and-beyond exp))))))
          ((boom/exp? (1st exp))
           (cons (boom/preprocess (1st exp))
                 (boom/preprocess (2nd-and-beyond exp)))))))


;; Evaluation method for general language operations

(define boom/evaluate
  (lambda (left operator right)
    (cond ((eqv? operator '+) (+ left right))
          ((eqv? operator '-) (- left right))
          ((eqv? operator '*) (* left right))
          ((eqv? operator '/) (if (and (exact? left) (exact? right))
                                  (quotient left right)
                                  (/ left right)))
          ((eqv? operator '%) (remainder left right))
          ((eqv? operator '@) (/ (+ left right) 2))
          ((eqv? operator '==) (= left right))
          ((eqv? operator 'gt) (> left right))
          ((eqv? operator 'lt) (< left right))
          ((eqv? operator 'lte) (<= left right))
          ((eqv? operator 'gte) (>= left right))
          ((eqv? operator '&&) (and left right))
          ((eqv? operator '||) (or left right))
          ((eqv? operator 'xor) (or (and (eqv? left #f) (eqv? right #t))
                                    (and (eqv? left #t) (eqv? right #f))))
          ((eqv? operator '!) ((eqv? (eqv? left right) #f)))
          ((eqv? operator '!=) ((eqv? (= left right) #f)))
          ((eqv? operator ':) (if (right (boom/eval left))
                                  left
                                  (error 'Parameter left 'not 'of 'type right)))
          (else error "Cannot evaluate operator and expression"))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                  ;
; Homework 12 - Problem 2                          ;
;                                                  ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define boom/eval 
  (let ((variable-list top2-list))
    (lambda (exp)      
      (define boom/eval-help
        (lambda (exp)
          (cond ((boom/null? exp)
                 '())
                ((boom/string? exp)
                 exp)
                ((member exp variable-list)
                 (cell-value (list-ref (member exp variable-list) 1)))
                ((or (boom/number? exp)
                     (boom/varref? exp)) exp)
                ((boom/newline? exp)
                 (boom/newline exp))
                ((boom/define? (1st exp))
                 (set! variable-list (cons (2nd exp)
                                           (cons (cell (4th exp))
                                                 variable-list)))
                 (set! top2-list (cons (2nd exp)
                                       (cons (cell (4th exp))
                                             top2-list)))
                 (cell-value (list-ref (member (2nd exp) top2-list) 1)))
                
                ((boom/function? exp)
                 (boom/addFunction (2nd exp) (3rd exp)))
                
                ((boom/string? (1st exp)) exp)
                
                ((eqv? (1st exp) 'display)
                 (boom/display (boom/preprocess (2nd exp))))
                
                ((boom/unary-operator? (1st exp))
                 (boom/unary-operator (1st exp) (boom/eval-help (2nd exp))))
                ((eqv? 'begin (1st exp))
                 (begin-eval-loop '() (2nd-and-beyond exp)))
                ((eqv? 'with (1st exp))
                 (boom/eval-help (2nd-and-beyond exp)))
                ((eqv? (1st exp) 'switch)
                 (switch-statement (2nd exp) (3rd exp) variable-list))
                ((eqv? (1st exp) 'boom/list)
                 (boom/list (2nd-and-beyond exp)))
                ((eqv? (1st exp) 'while)
                 (if (condition-true? (2nd exp))
                     (while-loop (2nd exp) (3rd-and-beyond exp)
                                 (length (3rd-and-beyond exp)) 0)))
                ((eqv? '= (2nd exp))
                 (if (not (member (1st exp) variable-list))                     
                     (set! variable-list (cons (1st exp)
                                               (cons (cell (boom/eval-help
                                                            (3rd exp)))
                                                     variable-list)))
                     (cell-set! (list-ref (member (1st exp) variable-list) 1)
                                (boom/eval-help (3rd exp))))
                 (boom/eval-help (4th-and-beyond exp)))
                ((eqv? 'do (1st exp))                
                 (boom/eval-help (2nd exp)))
                ((eqv? '<= (2nd exp))
                 (cond ((member (1st exp) variable-list)
                        (cell-set! (list-ref (member (1st exp) variable-list) 1)
                                   (boom/eval-help (3rd exp))))
                       (cell-set! (list-ref (member (1st exp) top2-list) 1)
                                  (boom/eval-help (3rd exp)))
                       (else (error "not in list"))))
                ((boom/operator? (2nd exp))
                 (boom/evaluate (boom/eval-help (1st exp))
                                (2nd exp)
                                (boom/eval-help (3rd exp))))
                (else (error "Not a valid expression to evaluate")))))
      (boom/eval-help (boom/preprocess exp)))))









;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                  ;
; Homework 12 - Problem 4                          ;
;                                                  ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define boom-run
  (let ((current-eval '())
        (top-list top2-list))
    (lambda ()
      (define boom-run-inner
        (lambda (current-eval top-list)
          (set! top-list top2-list)
          (display "\n")
          (display "**********************************\n")
          (display "*                                *\n")
          (display "*   The Stroschein Scheme REPL   *\n")
          (display "*       By Aaron Stroshein       *\n")
          (display "*                                *\n")
          (display "**********************************\n")
          (display "\n")
          (display "boom > ")
          (set! current-eval (read))
          (display (boom/eval current-eval))
          (display "\n")
          (boom-run-inner current-eval top-list)))
      (boom-run-inner current-eval top-list))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                  ;
; Homework 12 - Problem 3                          ;
;                                                  ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; After finding a begin-end list, run through the steps

(define begin-eval-loop
  (lambda (previous begin-list)
    (cond ((eqv? begin-list 'end)
           previous)
          ((eqv? (1st begin-list) 'end)
           previous)
          (else (begin-eval-loop (boom/eval (1st begin-list))
                                 (2nd-and-beyond begin-list))))))


;; A condition checker for while loops and if statements

(define condition-true?
  (lambda (exp)
    (cond ((and (= (length exp) 3)
                (boom/evaluate (boom/eval (1st exp))
                               (boom/eval (2nd exp))
                               (boom/eval (3rd exp)))))
          ((and (= (length exp 1))
                (boom/boolean? exp))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                  ;
; Homework 12 - Problem 5: switch-statments ADT    ;
;                                                  ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define switch-statement
  (lambda (case options variable-list)
    (cond ((null? options)
           (error "No default case defined for case" case))
          ((eqv? (2nd-in-list options) 'default)
           (cell-value (4th-in-list options)))
               ((boom/evaluate
                (boom/eval (cell-value
                            (list-ref
                             (member case variable-list) 1)))
                '== (boom/eval (2nd-in-list options)))
               (cell-value (4th-in-list options)))
           (else (switch-statement case (2nd-and-beyond options) variable-list)))))
  
  (define process-cases
    (lambda (c-list len count)
      (cond ((boom/evaluate len '== count)
             '())
            (else (cons (boom/preprocess (list-ref c-list count))
                        (process-cases c-list len (+ count 1)))))))
  
  
  ;; While-loop ADT
  
  (define while-loop
    (lambda (test steps len current-step)
      (cond ((or (= current-step len) (eqv? current-step 'continue))
             (if (condition-true? test)
                 (while-loop test steps len 0)))
            (else (boom/eval (list-ref steps current-step))
                  (while-loop test steps len
                              (+ current-step 1))))))
  
  
  
  ;; Function ADT type-checks
  
  (define boom/function?
    (lambda (exp)
      (and (eqv? (1st exp) 'function)
           (= (length exp) 3))))
  
  (define boom/function-call?
    (lambda (exp)
      (member exp boom/function-list)))
  
  
  ;; Function constructor
  
  (define boom/addFunction
    (lambda (ftnName p-list)
      (set! boom/function-list
            (cons ftnName (cons (cell p-list) boom/function-list)))))
  
  
  ;; Function processor
  
  (define boom/function
    (lambda (exp)
      (begin-eval-loop '()
                       (2nd (cell-value
                             (list-ref
                              (member exp boom/function-list) 1))))))
  
  
  ;; If-statement ADT processor
  
  (define process-if
    (lambda (exp)
      (cond ((and (boom/boolean? (boom/eval (1st exp)))
                  (= (length exp) 4)
                  (cons (1st exp)
                        (cons (boom/preprocess (2nd exp))
                              (cons (boom/preprocess (3rd exp))
                                    (boom/preprocess (4th exp)))))))
            ((and (boom/boolean? (boom/eval (1st exp)))
                  (= (length exp) 2))
             (cons (boom/preprocess (1st exp))
                   (boom/preprocess (2nd exp))))
            ((or (and (eqv? (1st exp) 'if)
                      (= (length exp) 3))
                 (and (boom/boolean? (boom/eval (1st exp)))
                      (= (length exp) 3)))
             (cons (boom/preprocess (1st exp))
                   (cons (boom/preprocess (2nd exp))
                         (list (boom/preprocess (3rd exp))))))
            ((not (eqv? (1st exp) 'if))
             (list (boom/preprocess (1st exp))))
            ((eqv? (1st-of-the-2nd exp) 'if)
             (cons (1st exp) (cons (boom/preprocess (2nd exp))
                                   (boom/preprocess (3rd-and-beyond exp)))))
            (error "illegal if statement"))))
  
  
  ;; Display ADT type-checker
  
  (define boom/display?
    (lambda (exp)
      (and (= (length exp) 2)
           (eqv? (1st exp) 'display))))
  
  
  ;; Display ADT
  
  (define boom/display
    (lambda (exp)
      (display exp)))
  
  ;; Read-in ADT
  
  (define boom/read
    (lambda ()
      (read)))
  
  
  ;; Newline type-check
  
  (define boom/newline?
    (lambda (exp)
      (eqv? (1st exp) 'newline)))
  
  
  ;; Newline ADT
  
  (define boom/newline
    (lambda (exp)
      (display "\n")))
  
  
  
  
  
  
  ;; This is a method I was toying with to see how easily one could
  ;; set up something like a tet-adventure game with the language
  ;; as defined above.
  
  (define text-story-test
    (lambda ()
      
      (boom/addFunction 'firstRoom '(begin
                                      (display "hello")
                                      (newline)
                                      (newline)
                                      end))
      (cell-value (list-ref (member 'firstRoom boom/function-list) 1))
      
      (boom/eval '(define choice <= "null"))
      (boom/eval '(display "We start our story in a remote area of the town."))
      (boom/eval '(newline))
      (boom/eval '(newline))
      (boom/eval '(display "(Enter commands in the text box at the bottom of the screen)"))
      (boom/eval '(newline))
      (boom/eval '(newline))
      (boom/eval '(display "[Landing - Inside House]"))
      (boom/eval '(newline))
      (boom/eval '(newline))
      (boom/eval '(display "You are standing on landing of a split foyee house."))
      (boom/eval '(newline))
      (boom/eval '(newline))))
