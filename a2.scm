;Part1: Permutation
(define (takeout x lst)
  (cond
    ;if null, return null list
    ((null? lst) '())
    ;if equal letter, takeout the rest list
    ((equal? x (car lst))   (takeout x (cdr lst))  )
    ;if not equal letter, construct x in the takeout list
    (else                   (cons   (car lst) (takeout x (cdr lst))))
  )
)

(define (permu-cal lst)
  (cond
    ;base for holding one element
    ((null? lst) '())
    ((= (length lst) 1)   (list lst)   )
    ;hanging combination for each element
    (else 
          (apply append(map(lambda (i) 
                              (map
                                ;add new element i to permutation j
                                (lambda (j)(cons i j)) (permu-cal (takeout i lst))
                              )
                            )
                        lst)
          )
    )
  )
)

(define (permute lst)
  (define permuteList (permu-cal lst))
  (printList permuteList)
)

;=================================================below is for list management
                                          
;print list
(define (printList lst)
          (cond ((null? lst) #f)        ;if reach the end of the list return false
                (else
                 (display (car lst))    ;for each element display the element out
                 (newline)
                 (printList (cdr lst))  ;recurse to print the rest of the list
                )
          )
)

;this function will convert ((a b c) (d e f))->(("a" "b" "c")("d" "e" "f"))->("abc" "def")
(define (convertPermuToString permuteList)
  (cond ((null? permuteList)  '())     ;end of list return () for initialize the list for cons
        (else
          ;cons the changed string form element to the list
          (cons (apply string-append (map symbol->string (car permuteList)))
                (convertPermuToString (cdr permuteList))
          )
        )
  )
)

;this function convert ("abc" "def")->((#\a #\b #\c)(#\d #\e #\f))->(("a" "b" "c")("d" "e" "f"))->((a b c)(d e f))
(define (reverseConvertPermuToString permuteListString)
  (cond ((null? permuteListString)  '()) ;end of list return () for initialize the list for cons
        (else
          ;cons the changed symbol form element to the list
          (cons (map string->symbol (map string (string->list (car permuteListString))))
                (reverseConvertPermuToString (cdr permuteListString))
          )
        )
  )
)

(define (checkContainInList lst obj)
  (cond ((null? lst) #f)                          ;search to end return false
        ((equal? (car lst) obj)    #t)                ;if found in list return true
        (else
            (cond ((equal? #t (checkContainInList (cdr lst) obj)) #t)  ;else keep on search until T/F
                  (else #f)
            )
        )
  )
)

(define (checkDictionary dictionary permuteList)
  (cond ((null? permuteList) '()    ) ;end, do nothing , or initially the list is empty
        ((null? dictionary)  '()    ) ;no dictionary , return empty list
        ((equal? #t (checkContainInList dictionary (car permuteList)))
                 ;the word is contained in dictionary so cons in list and continue search
                 (cons (car permuteList)(checkDictionary dictionary (cdr permuteList)))
        )
        (else
                ;the word car out is not in dictionary, invalid word so keep on search
                 (checkDictionary dictionary (cdr permuteList))
        )
  )
)

(define (anagram dictionary lst)
  (define stringDict (map symbol->string dictionary))           ;change the element in dictionary from symbol to string
  (define permuteList (permu-cal lst))
  (define permuStringList (convertPermuToString permuteList))   ;convert the permutation list to string
  (define result (checkDictionary stringDict permuStringList))
  (define resultSymbol (reverseConvertPermuToString result))    ;convert back the string list to symbol
  (printList resultSymbol)
)

(display '(============================Part1===========================))(newline)
(display '(The following dictionary is defined for you: dictionary '(a act ale at ate cat eat etc tea) ))(newline)
(display '(For the first function, when you type : (anagram dictionary '(a t e))) )(newline)
(display '(The following result would be displayed:))(newline)
(define dictionary '(a act ale at ate cat eat etc tea))
(anagram dictionary '(a e t))
(newline)


;=========================================
;Part2 my-or

(define-macro my-or
  (lambda (x y)
    `(let ([temp ,x])
       (if temp temp ,y))))

(display '(===============================Part2==========================))(newline)
(display '(for original my-or macro, if you input))(newline)
(display '(>(define temp 3)))(newline)
(display '(>(my-or #f temp)))(newline)
(display '(The result would be:))(newline)
(define temp 3)
(display (my-or #f temp))(newline)

(define-syntax new-my-or
  (syntax-rules ()
    [(new-my-or) #f]                   ;if no element return false
    [(_ element) element]              ;if one element return that element
    [(_ x y ...)                       ;if two element
         (let 
           ([temp x])                  ;let temp be x first
           (if 
                temp temp              ;if temp is not false, return it
                    (new-my-or y ...)  ;if temp is false check the second element by (new-my-or y)
           )
         )
    ]
  )
)
(newline)
(display '(for new new-my-or macro, if you input again))(newline)
(display '(>(define temp 3)))(newline)
(display '(>(new-my-or #f temp)))(newline)
(display '(The result would be:))(newline)
(display (new-my-or #f temp))(newline)

(newline)
(display '(if you input (new-my-or 1) the output is))(newline)
(display (new-my-or 1))(newline)

(newline)
(display '(if you input (new-my-or 2 1) the output is))(newline)
(display (new-my-or 2 1))