#lang racket
;First function: Permutation
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

(define (printList lst)
          (cond ((null? lst) #f)
                (else
                 (display (car lst))
                 (newline)
                 (printList (cdr lst))
                )
          )
)

(define (permute lst)
  (define permuteList (permu-cal lst))
  (printList permuteList)
)

(display '(For the first function, when you type : (permute '(a b c)))   )(newline)
(display '(The following result would be displayed:))(newline)
(permute '(a b c)) (newline)

;======================================
;Second function: Graph and path length

(define (distance-cycle-check graph s t)
  (cond
    ((equal? s t) ;handle self cycle
        (cond
          ((equal? #f (assq t (cdr(assq s graph))))  0);no self cycle
          (else (distance-cal graph s t))
        )
    )
    (else (distance-cal graph s t))
  )
)

(define (distance-cal graph s t)
    (if
      (assq s graph) ;check if s appear in g
        (if
            (assq t (cdr (assq s graph))  );check if t is linked with s
                    (cdr
                      (assq t (cdr (assq s graph)))
                    )
                    #f
        )
        #f
    )
)

(define (path-exist graph route)
  (cond
     ((equal? (cdr route) '()) #t) ;recursively run through successful
     (else (if (and
           (path-exist graph (cdr route)) ;recursive function call
           (integer? (distance-cycle-check graph (car route) (cadr route)))
           ;check head point is link to next point
          )
        #t
        #f
      ))
  )
)

(define (path-length-cal graph route)
  (cond
      ((equal? route '()) 0)              ;null route
      ((equal? (cdr route) '()) 0)       ;single item in route
      (else
          (+
            (distance-cycle-check graph (car route) (cadr route))
            (path-length-cal graph (cdr route))
          )
      )
  )
)

(define (path-length graph route)
  (if (path-exist graph route)
      (path-length-cal graph route)
      #f
  )
)

(define (distance . llist)
  (cond
    ((equal? (cdr llist) '()) #f) ;check invalid input
    ((equal? (cddr llist) '()) #f);check invalid input
    (else
        (path-length (car llist) (cdr llist))
    )
  )
)

(define g
  '((a . ((b . 5) (c . 8) (d . 3)))
   (b . ((a . 4) (c . 7)))
   (c . ((a . 2) (b . 6) (c . 2) (d . 9)))
   (d . ((b . 1) (c . 4)))))

(display '(For second function, a graph is defined for you by default))(newline)
(display '(When you type (distance g 'a 'b)))(newline)
(display '(the following would be returned:))(newline)
(display (distance g 'a 'b))(newline)
(display '(When you type (path-length g '(a c d b c))))(newline)
(display '(the following would be returned:))(newline)
(display (path-length g '(a c d b c)))(newline)
