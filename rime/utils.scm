(define-module (rime utils)
  #:use-module (bytestructures guile)
  #:use-module (rnrs bytevectors)
  #:use-module ((system foreign) #:select (null-pointer?
                                           bytevector->pointer
                                           make-pointer
                                           pointer?
                                           pointer->procedure
                                           pointer->bytevector
                                           pointer->string
                                           string->pointer
                                           sizeof
                                           pointer-address
                                           (int . ffi:int)))
  #:use-module (srfi srfi-26)
  #:use-module (rime check)
  #:export
  (bytestructure->pointer
   pointer->bytestructure
   make-pointer->string
   string->pointer-address
   struct-init
   char*
   bool
   session-id
   true
   false
   c-int->bool
   bool->c-int
   define-get-api-funcation)
  #:export-syntax (check-number?
                   check-pointer?
                   check-string?)
  #:re-export (&check-error
               check-error?
               check-error-checker
               check-error-value
               check-error-file-name
               check-error-message
               check-error-line
               check-error-column)
  #:re-export-syntax (define-check))

(define-check check-string?
  string? "This is not string!")
(define-check check-number?
  number? "This is not number!")
(define-check check-pointer?
  pointer? "This is not pointer!")

(define bytestructure->pointer
  (compose bytevector->pointer bytestructure-bytevector))

(define (pointer->bytestructure pointer struct)
  (make-bytestructure
   (pointer->bytevector
    pointer (bytestructure-descriptor-size struct))
   0
   struct))

(define make-pointer->string (compose (lambda (a) (if (null-pointer? a)
                                                      ""
                                                      (pointer->string a))) make-pointer))

(define string->pointer-address (compose pointer-address string->pointer))
(define (struct-init make-funcation struct . other-option)
  (make-funcation (bytestructure struct `((data-size ,(- (bytestructure-descriptor-size struct)
                                                         (sizeof ffi:int)))
                                          ,@other-option))))

(define char* (bs:pointer int8))
(define bool int)

(define session-id uintptr_t)
(define true 1)
(define false 0)
(define (c-int->bool num)
  (case num
    ((0) #f)
    ((1) #t)
    (else => (cut error "rime: unknow num:" <>))))

(define (bool->c-int bool)
  (case bool
    ((#f) false)
    ((#t) true)
    (else => (cut error "rime: unknow item:" <>))))

(define-syntax define-get-api-funcation
  (lambda (x)
    (syntax-case x ()
      ((_ name make-api get-bytestructure-funcation)
       #'(define-inlinable (name funcname return params)
           (pointer->procedure
            return (make-pointer
                    (bytestructure-ref
                     (get-bytestructure-funcation make-api)
                     funcname)) params))))))
;; (defmacro (define-get-api-funcation name make-api-funcation get-bytestructure-funcation)
;;   `(define-inlinable (name funcname return params)
;;      (pointer->procedure
;;       return (make-pointer
;;               (bytestructure-ref
;;                (,get-bytestructure-funcation ,make-api-funcation)
;;                funcname)) params)))
