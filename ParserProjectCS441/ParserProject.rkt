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
      [(and (equal? wordString "write") (not(isLetter? (first charLists)))) (cons "write" charLists)]
      [(and (equal? wordString "read") (not(isLetter? (first charLists)))) (cons "read" charLists)]
      [(not(isLetter? (first charLists))) (cons "ID" charLists)]
      [(isLetter? (first charLists)) (toId (rest charLists) tokenList (string-append wordString (list->string  (list (first charLists)))))]
      [else "error"] 
))

(define (toNum charLists tokenList wordString)
(cond 
      [(not(isDigit? (first charLists))) (cons "Num" charLists)]
      [(isDigit? (first charLists)) (toNum (rest charLists) tokenList (string-append wordString (list->string  (list (first charLists)))))]
      [else "error"] 
))


(define (scanner charList tokenList lineNumber )
  (cond [ (or  (and(equal? #\$ (first charList)) (equal? #\$ (first(rest charList)))) (null? charList)) (append tokenList (list "EOF"))]
        [ (equal? #t (isMultOpToken? (first charList) ))  (scanner(rest charList) (append tokenList (list "MultOp") ) lineNumber)] 
        [ (equal? #t (isAddOpToken? (first charList) ) )  (scanner(rest charList) (append tokenList (list "addOp") ) lineNumber)]
        [ (equal? #t (isLeftParen? (first charList) ) )  (scanner(rest charList) (append tokenList (list "lParen") ) lineNumber)]
        [ (equal? #t (isRightParen? (first charList) ) )  (scanner(rest charList) (append tokenList (list "RParen") ) lineNumber)]
        [ (and (equal? "ID" (first(toId charList tokenList ""))) (equal? #t (isLetter? (first charList)))) (scanner (rest(toId charList tokenList "")) (append tokenList (list "ID")) lineNumber)]
        [ (and (equal? "read" (first(toId charList tokenList ""))) (equal? #t (isLetter? (first  charList)))) (scanner (rest(toId charList tokenList "")) (append tokenList (list "read")) lineNumber)]
        [ (and (equal? "write" (first(toId charList tokenList ""))) (equal? #t (isLetter? (first charList)))) (scanner (rest(toId charList tokenList "")) (append tokenList (list "write")) lineNumber)]
        [ (and (equal? "Num" (first(toNum charList tokenList ""))) (equal? #t (isDigit? (first charList)))) (scanner (rest(toNum charList tokenList "")) (append tokenList (list "Num")) lineNumber)]
        [ (equal? #\newline (first charList)) (scanner (rest charList) tokenList (+ lineNumber 1))]
        [ (equal? #t (char-whitespace?  (first charList))) (scanner (rest charList) tokenList lineNumber)]
        [ (and  (equal? #\:  (first charList))) (equal? #\=  (first (rest charList)))   (scanner (rest(rest charList)) (append tokenList (list "assignment")) lineNumber)] ;don't understand why this does not work
        [else (printf(string-append "Error on line "  (number->string lineNumber))) ] ;(scanner(rest charList) tokenList)
))







;start of parser logic

(define (parse filename)

  (program (scan filename))
 
      )


(define (program tokenList)
  {cond [ (equal? "ID" (first tokenList))  (stmt-list tokenList) (print tokenList)]
      [ (equal? "read" (first tokenList))  (stmt-list tokenList) (print tokenList)]
      [ (equal? "write" (first tokenList)) (stmt-list tokenList) (print tokenList)]
      [ (equal? "EOF" (first tokenList)) "parse passed!"]
      [else "parse error"]
      })

(define (stmt-list tokenList)
  {cond [ (equal? "ID" (first tokenList)) (stmt tokenList) (stmt-list tokenList)]
      [ (equal? "read" (first tokenList)) (stmt tokenList) (stmt-list tokenList)]
      [ (equal? "write" (first tokenList)) (stmt tokenList) (stmt-list tokenList)]
      [ (equal? "EOF" (first tokenList)) tokenList]
      [else "parse error"]
      })

(define (stmt tokenList)
  {cond [ (and (equal? "ID" (first tokenList)) (equal? "assignment" (first(rest tokenList)))) (expr (rest(rest tokenList)))]
      [ (and (equal? "read" (first tokenList)) (equal? "ID" (first(rest tokenList)))) (rest(rest tokenList))]
      [ (equal? "write" (first tokenList)) (expr (rest tokenList))]
      [else "parse error"]
      })

(define (term tokenList)
  {cond [ (equal? "ID" (first tokenList)) (factor tokenList) (factor-tail tokenList) ]
      [ (equal? "Num" (first tokenList)) (factor tokenList) (factor-tail tokenList)]
      [ (equal? "lParen" (first tokenList)) (factor tokenList) (factor-tail tokenList)]
      [else "parse error"]
      })

(define (term-tail tokenList)
  {cond [ (equal? "addOp" (first tokenList)) (addOpp tokenList) (term tokenList) (term-tail tokenList) ]
      [ (equal? "lParen" (first tokenList)) tokenList]
      [ (equal? "ID" (first tokenList)) tokenList]
      [ (equal? "read" (first tokenList)) tokenList]
      [ (equal? "write" (first tokenList)) tokenList]
      [ (equal? "EOF" (first tokenList)) tokenList]
      [else "parse error"]
      })

(define (expr tokenList)
  {cond [ (equal? "ID" (first tokenList)) (term tokenList) (term-tail tokenList) ]
      [ (equal? "Num" (first tokenList)) (term tokenList) (term-tail tokenList)]
      [ (equal? "lParen" (first tokenList)) (term tokenList) (term-tail tokenList)]
      [else "parse error"]
      })

(define (factor tokenList)
(cond
      [ (equal? "ID" (first tokenList)) (rest tokenList) ]
      [ (equal? "Num" (first tokenList)) (rest tokenList)]
      [  (equal? "lParen" (first tokenList)) (expr (rest tokenList))]  ; needs to be modified to reflect grammar
      [ (equal? "RParen" (first tokenList)) (rest tokenList)]
      [else " parse error"] 
))

(define (factor-tail tokenList)
(cond
      [ (equal? "MultOp" (first tokenList)) (factor tokenList) (factor-tail tokenList) ]
      [ (equal? "addOp" (first tokenList)) tokenList]
      [  (equal? "Rparen" (first tokenList)) tokenList ]  ; needs to be modified to reflect grammar
      [  (equal? "ID" (first tokenList)) tokenList ]
      [  (equal? "read" (first tokenList)) tokenList ]
      [ (equal? "write" (first tokenList)) tokenList ]
      [  (equal? "EOF" (first tokenList)) tokenList ]
      [else " parse error"] 
))

(define (addOpp tokenList)
(cond
      [ (equal? "addOp" (first tokenList)) (rest tokenList) ]
      [else " parse error"] 
))

(define (multOpp tokenList)
(cond
      [ (equal? "MultOp" (first tokenList)) (rest tokenList) ]
      [else " parse error"] 
))





(define (scan fileName) (scanner fileName '() 1) )

(define (file1) (fileToCharString "Input01.txt"))
(define (file2) (fileToCharString "Input02.txt"))
(define (file3) (fileToCharString "Input03.txt"))
(define (file4) (fileToCharString "Input04.txt"))
(define (file5) (fileToCharString "Input05.txt"))
(define (file6) (fileToCharString "Input06.txt"))












