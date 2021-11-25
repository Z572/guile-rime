(define-module (rime module)
  #:use-module (rime api)
  #:use-module (rime utils)
  #:use-module (srfi srfi-9)
  #:use-module (bytestructures guile)
  #:use-module ((system foreign) #:select (make-pointer
                                           string->pointer
                                           pointer->procedure
                                           void
                                           (int . ffi:int)
                                           (uintptr_t . ffi:uintptr_t)
                                           (size_t . ffi:size_t)))
  #:export (custom-api?
            module?
            custom-api?
            module->pointer
            pointer->module
            module-get-api
            module-module-name
            module-initialize
            module-finalize
            register-module
            find-module))

(define get-api-funcation %guile-rime-get-api-funcation)

(define %custom-api
  (bs:struct
   `((data-size ,int))))

(define-record-type <custom-api>
  (%make-custom-api bytestructure)
  custom-api?
  (bytestructure custom-api-bytestructure))

(define %module
  (bs:struct
   `((data-size ,int)
     (module-name ,char*)
     (initialize ,(bs:pointer void))
     (finalize ,(bs:pointer void))
     (get-api ,(bs:pointer '*);; %custom-api
              ))))

(define-record-type <module>
  (%make-module bytestructure)
  module?
  (bytestructure module-bytestructure))

(define (make-module-bytestructure)
  (struct-init %make-module %module))

(define (module->pointer module)
  (bytestructure->pointer
   (module-bytestructure module)))

(define (pointer->module pointer)
  (%make-module
   (pointer->bytestructure pointer %module)))

(define (module-get-api module)
  (let ((p (pointer->procedure
            '* (make-pointer (bytestructure-ref
                              (module-bytestructure module)
                              'get-api)) '())))
    (p)))

(define (module-module-name module)
  (make-pointer->string (bytestructure-ref (module-bytestructure module) 'module-name)))

(define (module-initialize module)
  (pointer->procedure void (make-pointer (bytestructure-ref (module-bytestructure module) 'initialize)) '()))

(define (module-finalize module)
  (pointer->procedure void (make-pointer (bytestructure-ref (module-bytestructure module) 'finalize)) '()))

(define %register-module
  (get-api-funcation 'register-module ffi:int '(*)))

(define (register-module module)
  (%register-module (module->pointer module)))

(define %find-module
  (get-api-funcation 'find-module '* '(*)))

(define (find-module module-name)
  (pointer->module (%find-module (string->pointer module-name))))
