;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Eduardo Acuña Yeomans 2015                                                                ;;
;;                                                                                           ;;
;; Programa que realiza un escaneo y análisis léxico MUY primitivo a expresiones aritéticas  ;;
;; sencillas utilizando puertos de I/O y una tabla de símbolos basada en una lista enlazada. ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import (scheme base))
(import (scheme file))
(import (scheme char))
(import (scheme write))
(import (srfi 1))
(import (larceny records printer))


                                  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                                  ;; ;;;;;;;;;;;;;;;;;;;;;;;;; ;;
                                  ;; ;; ;;;;;;;;;;;;;;;;;;; ;; ;;
                                  ;; ;; ;; ;;;;;;;;;;;;; ;; ;; ;;
                                  ;; ;; ;; ;; SCANNER ;; ;; ;; ;;
                                  ;; ;; ;; ;;;;;;;;;;;;; ;; ;; ;;
                                  ;; ;; ;;;;;;;;;;;;;;;;;;; ;; ;;
                                  ;; ;;;;;;;;;;;;;;;;;;;;;;;;; ;;
                                  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; consume-whitespace : <port> <list> -> <void>                                                 ;;
;; Take an input port and a list of the characters that are delimiters but not part of a lexeme ;;
;; and consume all of the contiguous elements from this list from the input port.               ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (consume-whitespace input-port blank-delimiters)

  ;; iterate an indeterminate  number of times with a new peeked character per iteration
  (let loop ((char (peek-char input-port)))

    ;; conditioning for char in blank-delimiters 
    (when (member char blank-delimiters)   ; if char is an element of blank-delimiters:
          (read-char input-port)           ;   consume char from input-port
          (loop (peek-char input-port))))) ;   repeat procedure


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; non-delimiters : <port> <list> <list> -> <list>                                                 ;;
;; Take an input port, a list of the characters that are delimiters but not part of a lexeme       ;;
;; and a list of the characters that are delimiters and lexemes and return a list representing the ;;
;; sequence of characters of the non delimeter lexeme consumed form input-port.                    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (non-delimiters input-port blank-delimiters textual-delimiters)

  ;; peek a character from input-port
  (define char (peek-char input-port))

  ;; conditioning on char
  (cond ((eof-object? char)           ; if char is EOF:
         '())                         ;   return NULL

        ((member char blank-delimiters)                   ; if blank-delimiters contains char:
         (read-char input-port)                           ;   consume char from input-port
         (consume-whitespace input-port blank-delimiters) ;   consume the contiguous blank-delimiters from input-port
         '())                                             ;   return NULL

        ((member char textual-delimiters) ; if textual-delimiters contains char:
         '())                             ;   return NULL

        (else                                                                      ; otherwise:
         (cons (read-char input-port)                                              ;   prepend the next char form input-port to
               (non-delimiters input-port blank-delimiters textual-delimiters))))) ;    the value of recursively calling non-delimiters





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; next-lexeme : <port> <list> <list> -> <string>                                            ;;
;; Take an input port, a list of the characters that are delimiters but not part of a lexeme ;;
;; and a list of the characters that are delimiters and lexemes and return a string of the   ;;
;; first valid lexeme consumed from the input port.                                          ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (next-lexeme input-port blank-delimiters textual-delimiters)

  ;; peek a character from input-port
  (define char (peek-char input-port))

  ;; conditioning on char
  (cond ((eof-object? char)             ; if char is EOF:
         'EOF)                          ;   return the symbol EOF

        ((member char blank-delimiters)                                ; if blank-delimiters contains char:
         (read-char input-port)                                        ;   consume char from input-port
         (consume-whitespace input-port blank-delimiters)              ;   consume the contiguous blank-delimiters from input-port
         (next-lexeme input-port blank-delimiters textual-delimiters)) ;   recursively call next-lexeme

        ((member char textual-delimiters) ; if textual-delimiters contains char:
         (string (read-char input-port))) ;   return the textual delimiter as a string

        (else                                                     ; otherwise:
         (let ((lexeme-list (non-delimiters input-port            ;   obtain a list containing the sequence of characters not in
                                            blank-delimiters      ;   blank-delimiters and textual-delimiters with a call to the
                                            textual-delimiters))) ;   non-delimiters procedure.
           (apply string lexeme-list)))))                         ;   return a string with the characters from that list




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; scan : <list> <list> -> (<input-port> -> <list>)                                           ;;
;; Take a list of non-lexeme delimiters and lexeme delimiters and return a scanner that takes ;;
;; an input port and returns a list of lexemes.                                               ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (make-scanner blank-delimiters textual-delimiters)

  ;; return an anonymous function that scans an input port and returns a list of lexemes
  (lambda (input-port)

    ;; get the next lexeme from the input port
    (define lexeme (next-lexeme input-port blank-delimiters textual-delimiters))

    ;; conditioning on lexeme
    (if (eq? lexeme 'EOF)                  ; if lexeme is EOF:
        '()                                ;   return the empty list
        (cons lexeme (scan input-port))))) ;   otherwise prepend the lexeme to the result of
                                           ;    recursively calling scan on the rest of the input



                                      ;;;;;;;;;;;;;;;;;;
                                      ;; STOP!...     ;;
                                      ;;              ;;
                                      ;; EXAMPLE TIME ;;
                                      ;;;;;;;;;;;;;;;;;;

(define port0 (open-input-string "2+8*6/(5-1)"))

(define port1 (open-input-string "9*x-sqrt(2)"))

(define port2 (open-input-string "foo         bar          baz     123132-213+123-123\n 312 \t asd321\n\n\t"))

(define scan (make-scanner (list #\space     ; ' '
                                 #\newline   ; '\n'
                                 #\tab       ; '\t'
                                 )
                           (list #\+         ; '+'
                                 #\-         ; '-'
                                 #\*         ; '*'
                                 #\/         ; '/'
                                 #\(         ; '('
                                 #\)         ; ')'
                                 )))

(define lexemes0 (scan port0))

(define lexemes1 (scan port1))

(define lexemes2 (scan port2))

;(close-input-port port0)
;(close-input-port port1)
;(close-input-port port2)



                                  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                                  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;
                                  ;; ;; ;;;;;;;;;;;;;;;;;;;;;;;; ;; ;;
                                  ;; ;; ;; ;;;;;;;;;;;;;;;;;; ;; ;; ;;
                                  ;; ;; ;; ;; SYMBOL TABLE ;; ;; ;; ;;
                                  ;; ;; ;; ;;;;;;;;;;;;;;;;;; ;; ;; ;;
                                  ;; ;; ;;;;;;;;;;;;;;;;;;;;;;;; ;; ;;
                                  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;
                                  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-record-type <symbol-table>
  (symbol-table alist)
  symbol-table?
  (alist alist set-alist!))

(rtd-printer-set!
 <symbol-table>
 (lambda (table port)
   ;(display "(<symbol-table> " port)
   (display (alist table) port)
   ;(display ")" port)
   ))

(define (empty-symbol-table)
  (symbol-table '()))

(define (lookup table key)
  (assoc key (alist table)))

(define (insert! table key value)
  (set-alist! table (cons (cons key value) (alist table))))

(define (lexeme entry)
  (car entry))

(define (token entry)
  (cdr entry))

                                      ;;;;;;;;;;;;;;;;;;
                                      ;; STOP!...     ;;
                                      ;;              ;;
                                      ;; EXAMPLE TIME ;;
                                      ;;;;;;;;;;;;;;;;;;

(define table (empty-symbol-table))

(insert! table "foo" 'TOKEN-ID)

(insert! table "bar" 'TOKEN-ID)

(insert! table "baz" 'TOKEN-ID)

(insert! table "666" 'TOKEN-NUM)

(insert! table "/"   'TOKEN-DIV)

(insert! table "+"   'TOKEN-SUM)

(lookup table "baz")

(display table)


                                 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                                 ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;
                                 ;; ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;; ;;
                                 ;; ;; ;; ;;;;;;;;;;;;;;;;;;;;;; ;; ;; ;;
                                 ;; ;; ;; ;; LEXICAL ANALYSIS ;; ;; ;; ;;
                                 ;; ;; ;; ;;;;;;;;;;;;;;;;;;;;;; ;; ;; ;;
                                 ;; ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;; ;;
                                 ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;
                                 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (chars-satisfy? lst pred)
  (cond ((null? lst)        #true)
        ((pred (car lst))   (chars-satisfy? (cdr lst) pred))
        (else               #false)))


;; (0|1|2|3|4|5|6|7|8|9)+
(define (token-num? str)
  (and (> (string-length str) 0)
       (chars-satisfy? (string->list str) char-numeric?)))

;; (a|...|z|A|...|Z)+
(define (token-id? str)
  (and (> (string-length str) 0)
       (chars-satisfy? (string->list str) char-alphabetic?)))

;; +
(define (token-sum? str)
  (string=? str "+"))

;; -
(define (token-sub? str)
  (string=? str "-"))

;; *
(define (token-mul? str)
  (string=? str "*"))

;; /
(define (token-div? str)
  (string=? str "/"))

;; (
(define (token-lp? str)
  (string=? str "("))

;; )
(define (token-rp? str)
  (string=? str ")"))

;; associated-token : <string> -> <symbol>
(define (associated-token str)
  (cond ((token-num? str)  'TOKEN-NUM)
        ((token-id?  str)  'TOKEN-ID)
        ((token-sum? str)  'TOKEN-SUM)
        ((token-sub? str)  'TOKEN-SUB)
        ((token-mul? str)  'TOKEN-MUL)
        ((token-div? str)  'TOKEN-DIV)
        ((token-lp?  str)  'TOKEN-LP)
        ((token-rp?  str)  'TOKEN-RP)
        (else              'TOKEN-INVALID)))

;; zip-cons : <list> <list> -> <alist>
(define (zip-cons lst1 lst2)
  (map cons lst1 lst2))

;; tokenize : <list> -> <symbol-table>
(define (tokenize lexemes)
  (define table (empty-symbol-table))
  (for-each (lambda (entry)
              (unless (lookup table (lexeme entry))
                      (insert! table (lexeme entry) (token entry))))
            (zip-cons lexemes
                      (map associated-token
                           lexemes)))
  table)


                                      ;;;;;;;;;;;;;;;;;;
                                      ;; STOP!...     ;;
                                      ;;              ;;
                                      ;; EXAMPLE TIME ;;
                                      ;;;;;;;;;;;;;;;;;;

(define tokens0 (map associated-token lexemes0))

(define tokens1 (map associated-token lexemes1))

(define tokens2 (map associated-token lexemes2))

(define table0 (tokenize lexemes0))

(define table1 (tokenize lexemes1))

(define table2 (tokenize lexemes2))

(equal?  (map (lambda (lexeme) (token (lookup table0 lexeme))) lexemes0)
         (map associated-token lexemes0))

(equal?  (map (lambda (lexeme) (token (lookup table1 lexeme))) lexemes1)
         (map associated-token lexemes1))

(equal?  (map (lambda (lexeme) (token (lookup table2 lexeme))) lexemes2)
         (map associated-token lexemes2))
