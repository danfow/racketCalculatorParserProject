#lang racket
(require parser-tools/lex)
(require racket/trace)



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


(define (match tokenList expected)
  {cond
      [ (equal? "parse error" tokenList) "parse error"]
      [ (equal? expected (first tokenList))  (rest tokenList ) ]
      [else "parse error"]
      })

(define (parse filename)
  {cond 
     [(equal? "parse error" (program (scan filename))) (print "parse error occured")]
     [(equal? "scanner error occured" (program (scan filename))) (print "scanner error occured")]
     [else (print "parse completed successfully!")]
     
     })
 
      


(define (program tokenList)
  {cond
      [ (equal? "parse error" tokenList) (print "parse error")]
      [(or (empty? tokenList) (void? tokenList)) "scanner error occured"]
      [ (equal? "ID" (first tokenList))  (match (stmt-list tokenList) "EOF") ]
      [ (equal? "read" (first tokenList))  (match (stmt-list tokenList) "EOF") ]
      [ (equal? "write" (first tokenList)) (match (stmt-list tokenList) "EOF")]
      [else "parse error"]
      })

(define (stmt-list tokenList)
  {cond
      [ (equal? "parse error" tokenList) "parse error"]
      [ (equal? "ID" (first tokenList)) (stmt-list (stmt tokenList))]
      [ (equal? "read" (first tokenList)) (stmt-list (stmt tokenList))]
      [ (equal? "write" (first tokenList)) (stmt-list (stmt tokenList))]
      [ (equal? "EOF" (first tokenList)) tokenList]
      [else "parse error"]
      })

(define (stmt tokenList)
  {cond
      [ (equal? "parse error" tokenList) "parse error"]
      [ (and (equal? "ID" (first tokenList)) (equal? "assignment" (first(rest tokenList))))  (expr ( match (match tokenList "ID") "assignment"))]
      [ (and (equal? "read" (first tokenList)) (equal? "ID" (first(rest tokenList)))) (match (match tokenList "read") "ID")]
      [ (equal? "write" (first tokenList)) (expr (match tokenList "write"))]
      [else "parse error"]
      })

(define (term tokenList)
  {cond
      [ (equal? "parse error" tokenList) "parse error"]
      [ (equal? "ID" (first tokenList)) (factor-tail (factor tokenList)) ]     
      [ (equal? "Num" (first tokenList)) (factor-tail (factor tokenList))]    
      [ (equal? "lParen" (first tokenList)) (factor-tail (factor tokenList))]
      [else " parse error"]
      })

(define (term-tail tokenList)
  {cond
      [ (equal? "parse error" tokenList) "parse error"]
      [ (equal? "addOp" (first tokenList)) (term-tail (term (addOpp tokenList))) ]
      [ (equal? "RParen" (first tokenList)) tokenList]
      [ (equal? "ID" (first tokenList)) tokenList]
      [ (equal? "read" (first tokenList)) tokenList]
      [ (equal? "write" (first tokenList)) tokenList]
      [ (equal? "EOF" (first tokenList)) tokenList]
      [else "parse error"]
      })

(define (expr tokenList)
  {cond
      [ (equal? "parse error" tokenList) "parse error"]
      [ (equal? "ID" (first tokenList)) (term-tail (term tokenList)) ]
      [ (equal? "Num" (first tokenList)) (term-tail (term tokenList))]
      [ (equal? "lParen" (first tokenList)) (term-tail (term tokenList))]
      [else "parse error"]
      })

(define (factor-tail tokenList)
(cond
      [ (equal? "parse error" tokenList) "parse error"]
      [ (equal? "MultOp" (first tokenList)) (factor-tail (factor (multOpp tokenList))) ]
      [ (equal? "addOp" (first tokenList)) tokenList]
      [  (equal? "RParen" (first tokenList)) tokenList ]
      [  (equal? "ID" (first tokenList)) tokenList ]
      [  (equal? "read" (first tokenList)) tokenList ]
      [ (equal? "write" (first tokenList)) tokenList ]
      [  (equal? "EOF" (first tokenList)) tokenList ]
      [else "parse error"] 
))

(define (factor tokenList)
(cond
      [ (equal? "parse error" tokenList) "parse error"]
      [ (equal? "ID" (first tokenList)) (match tokenList "ID") ]
      [ (equal? "Num" (first tokenList)) (match tokenList "Num")]
      [  (equal? "lParen" (first tokenList)) (match (expr (match tokenList "lParen")) "RParen")]  
      [else "parse error"] 
))


(define (addOpp tokenList)
(cond
      [ (equal? "parse error" tokenList) "parse error"]
      [ (equal? "addOp" (first tokenList)) (match tokenList "addOp") ]
      [else "parse error"] 
))

(define (multOpp tokenList)
(cond
      [ (equal? "parse error" tokenList) "parse error"]
      [ (equal? "MultOp" (first tokenList)) (match tokenList "MultOp") ]
      [else "parse error"] 
))

(define (scan fileName) (scanner fileName '() 1) )

(define (file1) (fileToCharString "Input01.txt"))
(define (file2) (fileToCharString "Input02.txt"))
(define (file3) (fileToCharString "Input03.txt"))
(define (file4) (fileToCharString "Input04.txt"))
(define (file5) (fileToCharString "Input05.txt"))
(define (file6) (fileToCharString "Input06.txt"))




#|(trace program)
(trace stmt-list)
(trace stmt)
(trace term)
(trace term-tail)
(trace expr)
(trace factor)
(trace factor-tail)
(trace addOpp)
(trace multOpp)
(trace scan)|#














