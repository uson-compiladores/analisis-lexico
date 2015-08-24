;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Eduardo Acuña Yeomans 2015                                                      ;;
;;                                                                                 ;;
;; Programa para escanear un archivo de texto y enlistar el contenido considerando ;;
;; a los espacios en blanco como delimitadores.                                    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import (scheme base)
        (scheme file)
        (scheme char)
        (scheme write))

;; consume el espacio en blanco contiguo del inicio del puerto
(define (consume-whitespace input-port)
  (let loop ((c (peek-char input-port)))
    (when (and (char? c) (char-whitespace? c))
          (read-char input-port)
          (loop (peek-char input-port)))))

;; realiza el escaneo del contenido del archivo con nombre file-name y coloca en el
;; puerto de salida el resultado
(define (scan-into-port file-name output-port)
  (call-with-input-file file-name
    (lambda (input-port)
      (let loop ((c (peek-char input-port)))
        (cond ((eof-object? c))
              ((char-whitespace? c)
               (consume-whitespace input-port)
               (write-char #\newline output-port)
               (loop (peek-char input-port)))
              (else
               (write-char (read-char input-port) output-port)
               (loop (peek-char input-port))))))))

(define (scan file-name)
  (define output-port (open-output-string))
  (scan-into-port file-name output-port)
  output-port)

