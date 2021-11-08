(define-module (rime types)
  #:use-module (rnrs bytevectors)
  #:use-module (system foreign)
  #:export (make-double-pointer
            make-size_t-pointer
            pointer->size_t))

(define-syntax define-librime-type
  (lambda (s)
    "Define a wrapped pointer type for an opaque type of librime."
    (syntax-case s ()
      ((_ name)
       (let ((symbol     (syntax->datum #'name))
             (identifier (lambda (symbol)
                           (datum->syntax #'name symbol))))
         (with-syntax ((rtd    (identifier (symbol-append '< symbol '>)))
                       (pred   (identifier (symbol-append symbol '?)))
                       (wrap   (identifier (symbol-append 'pointer-> symbol)))
                       (unwrap (identifier (symbol-append symbol '->pointer))))
           #`(define-wrapped-pointer-type rtd
               pred
               wrap unwrap
               (lambda (obj port)
                 (format port "#<rime-~a ~a>"
                         #,(symbol->string symbol)
                         (number->string (pointer-address (unwrap obj))
                                         16))))))))))

(define-librime-type config)

(define (make-double-pointer)
  (bytevector->pointer (make-bytevector (sizeof '*))))

(define (make-size_t-pointer)
  (bytevector->pointer (make-bytevector (sizeof size_t))))

(define (pointer->size_t ptr)
  (bytevector-uint-ref (pointer->bytevector ptr (sizeof size_t))
                       0
                       (endianness little)
                       (sizeof size_t)))
