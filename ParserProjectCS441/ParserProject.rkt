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



(define (scanner charList tokenList)
  (cond [ (or (null? charList) (and(equal? #\$ (first charList)) (equal? #\$ (first(rest charList))))) tokenList]
        [ (equal? #t (isMultOpToken? (first charList) ))  (scanner(rest charList) (append tokenList (list "MultOp") ))] 
        [ (equal? #t (isAddOpToken? (first charList) ) )  (scanner(rest charList) (append tokenList (list "addOp") ))]
        [ (equal? #t (isLeftParen? (first charList) ) )  (scanner(rest charList) (append tokenList (list "lParen") ))]
        [ (equal? #t (isRightParen? (first charList) ) )  (scanner(rest charList) (append tokenList (list "RParen") ))]
        [else (scanner(rest charList) tokenList)]
))
(define (file1) (fileToCharString "Input01.txt"))
(define (file3) (fileToCharString "Input03.txt"))
(define (file4) (fileToCharString "Input04.txt"))












