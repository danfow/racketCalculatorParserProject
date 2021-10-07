#lang racket
(require parser-tools/lex)

;why are we here just to suffer?

(define (fileToCharString fileName) (string->list(file->string fileName))) ;converts file to a string and then converts that string to a list of characters

(define (getAddOpToken charList tokenList) ;deprecated
(cond [ (null? charList) tokenList]
      [ ( char=? #\+ (first charList)) (getAddOpToken(rest charList) (append (list "addOp") tokenList))]
      [ ( char=? #\- (first charList)) (getAddOpToken(rest charList) (append (list "addOp") tokenList))]
      [else (getAddOpToken (rest charList) tokenList)]

      ))

(define (getMultOpToken charList tokenList) ;deprecated
(cond [ (null? charList) tokenList]
      [ ( char=? #\* (first charList)) (getMultOpToken(rest charList) (append (list "multOp") tokenList))]
      [ ( char=? #\/ (first charList)) (getMultOpToken(rest charList) (cons (list "multOp") tokenList))]
      [else (getMultOpToken (rest charList) tokenList)]

      ))



(define (isAddOpToken? char)
(cond
      [ ( eq? char #\+ ) #t]
      [ ( eq? char #\-) #t]
      [else #f]

      ))

(define (isMultOpToken? char)
(cond 
      [ ( eq? char #\* ) #t]
      [ ( eq? char #\/) #t]
      [else #f]))

(define (isLeftParen? char)
(cond 
      [ ( eq? char #\( ) #t]
      [else #f]))

(define (isRightParen? char)
(cond 
      [ ( eq? char #\) ) #t]
      [else #f]))


(define (isLetter? char)
(cond 
      [ ( eq? (char-alphabetic? char)   #t)]
      [else #f]))

(define (isDigit? char)
(cond 
      [ (or ( eq? (char-numeric? char)   #t) (eq? #\. char))]
      [else #f]))

(define (isAssign? char)
(cond 
      [ ( equal? char #\=) #t ]
      [else #f]))

(define (toId charLists tokenList wordString)
(cond 
      [(and  (not(isLetter? (first charLists))) (and (not ( string=? wordString "read")) (not (string=? wordString "write")))) (cons "ID" charLists)]
      [(and (equal? wordString "write")) (not(isLetter? (first charLists))) (cons "write" charLists)]
       [(and (equal? wordString "read")) (not(isLetter? (first charLists))) (cons "read" charLists)]
      [(isLetter? (first charLists)) (toId (rest charLists) tokenList (string-append wordString (list->string  (list (first charLists)))))]
      [else "error"] 
))

(define (toNum charLists tokenList wordString)
(cond 
      [(not(isDigit? (first charLists))) (cons "Num" charLists)]
      [(isDigit? (first charLists)) (toNum (rest charLists) tokenList (string-append wordString (list->string  (list (first charLists)))))]
      [else "error"] 
))


(define (scanner charList tokenList )
  (cond [ (or  (and(equal? #\$ (first charList)) (equal? #\$ (first(rest charList)))) (null? charList)) tokenList]
        [ (equal? #t (isMultOpToken? (first charList) ))  (scanner(rest charList) (append tokenList (list "MultOp") ))] 
        [ (equal? #t (isAddOpToken? (first charList) ) )  (scanner(rest charList) (append tokenList (list "addOp") ))]
        [ (equal? #t (isLeftParen? (first charList) ) )  (scanner(rest charList) (append tokenList (list "lParen") ))]
        [ (equal? #t (isRightParen? (first charList) ) )  (scanner(rest charList) (append tokenList (list "RParen") ))]
        [ (and (equal? "ID" (first(toId charList tokenList ""))) (equal? #t (isLetter? (first charList)))) (scanner (rest(toId charList tokenList "")) (append tokenList (list "ID")))]
        [ (and (equal? "read" (first(toId charList tokenList ""))) (equal? #t (isLetter? (first  charList)))) (scanner (rest(toId charList tokenList "")) (append tokenList (list "read")))]
        [ (and (equal? "write" (first(toId charList tokenList ""))) (equal? #t (isLetter? (first charList)))) (scanner (rest(toId charList tokenList "")) (append tokenList (list "write")))]
        [ (and (equal? "Num" (first(toNum charList tokenList ""))) (equal? #t (isDigit? (first charList)))) (scanner (rest(toNum charList tokenList "")) (append tokenList (list "Num")))]
        [ (equal? #t (char-whitespace?  (first charList))) (scanner (rest charList) tokenList)]
        [ (and  (equal? #\:  (first charList))) (equal? #\=  (first (rest charList)))   (scanner (rest charList) (append tokenList (list "assignment")))] ;don't understand why this does not work
        [else "error" ] ;(scanner(rest charList) tokenList)
))
(define (file1) (fileToCharString "Input01.txt"))
(define (file2) (fileToCharString "Input02.txt"))
(define (file3) (fileToCharString "Input03.txt"))
(define (file4) (fileToCharString "Input04.txt"))
(define (file5) (fileToCharString "Input05.txt"))
(define (file6) (fileToCharString "Input06.txt"))












