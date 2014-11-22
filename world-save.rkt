#lang racket
(require rackunit)

;;world structure for the save-load function
(define-struct world (worldlist tempo curbeat modestate selected page) #:transparent)
(define default_list empty)
(define INITIAL_WORLD (make-world default_list 44100 0 "paused" "piano" 1))
(define-struct note (type pitch beat) #:transparent)

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

(define my-table (list (list "world" make-world)))

