#lang racket
(require rackunit)

(provide write-to-string
         read-from-string
         string->struct/maker)

;; writes a value into a string
(define (write-to-string val)
  (with-output-to-string 
   (lambda () (write val))))

;; reads a value from a string
(define (read-from-string str)
  (with-input-from-string str read))

;; turn strings into structures recursively
;; (listof (list/c symbol maker)) string -> any
(define (string->struct/maker maker-table string)
  (define vectorized (read-from-string string))
  (vectors->structs maker-table vectorized))

;; find all vectors, turn them into the corresponding structures
(define (vectors->structs table data)
  (cond
    [(vector? data) 
     (unless (<= 1 (vector-length data))
       (raise-argument-error 'vector->structs "vector of length 1 or more"
                             1 table data))
     (define label (vector-ref data 0))
     (match (regexp-match #px"^struct:(.*)$" (symbol->string label))
       [(list _ name)
        (match (assoc name table)
          [#f (raise-argument-error 'vectors->structs
                                    "struct with name appearing in table"
                                    1 table data)]
          [(list dc maker) (apply maker (map (lambda (data)
                                               (vectors->structs table data)) 
                                             (rest (vector->list data))))])])]
    [(list? data)
     (map (lambda (data) (vectors->structs table data)) data)]
    [else data]))


