(define-module (rime composition)
  #:use-module (rime configuration)
  #:use-module (rime utils)
  #:use-module (rime api)
  #:use-module (bytestructures guile)
  #:use-module (srfi srfi-9)
  #:use-module ((system foreign) #:select ((int . ffi:int)
                                           (uintptr_t . ffi:uintptr_t)))
  #:export (%composition
            %make-composition
            composition?
            composition->pointer
            composition-length
            composition-cursor-post
            composition-sel-start
            composition-sel-end
            composition-preedit
            commit-composition
            clear-composition)
  #:export-syntax
  (check-composition?))

(define get-api-funcation %guile-rime-get-api-funcation)

(define %composition
  (bs:struct `((length ,int)
               (cursor-pos ,int)
               (sel-start ,int)
               (sel-end ,int)
               (preedit ,char*))))

(define-record-type <composition>
  (%make-composition bytestructure)
  composition?
  (bytestructure composition-bytestructure))

(define-check check-composition?
  composition? "This is not a <composition> record!")

(define (make-composition-bytestructure)
  (%make-composition (bytestructure %composition)))

(define (composition->pointer composition)
  (check-composition? composition)
  (bytestructure->pointer (composition-bytestructure composition-bytestructure)))

(define (composition-length composition)
  (check-composition? composition)
  (bytestructure-ref (composition-bytestructure composition) 'length))

(define (composition-cursor-post composition)
  (check-composition? composition)
  (bytestructure-ref (composition-bytestructure composition) 'cursor-pos))

(define (composition-sel-start composition)
  (check-composition? composition)
  (bytestructure-ref (composition-bytestructure composition) 'sel-start))

(define (composition-sel-end composition)
  (check-composition? composition)
  (bytestructure-ref (composition-bytestructure composition) 'sel-end))

(define (composition-preedit composition)
  (check-composition? composition)
  (make-pointer->string (bytestructure-ref (composition-bytestructure composition) 'preedit)))

(define %commit-composition
  (get-api-funcation 'commit-composition ffi:int (list ffi:uintptr_t)))

(define (commit-composition session-id)
  (check-number? session-id)
  (c-int->bool (%commit-composition session-id)))

(define %clear-composition
  (get-api-funcation
   'clear-composition
   ffi:int (list ffi:uintptr_t)))

(define (clear-composition session-id)
  (check-number? session-id)
  (%clear-composition session-id))
