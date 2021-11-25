(define-module (rime candidate)
  #:use-module (rime api)
  #:use-module (rime utils)
  #:use-module (bytestructures guile)
  #:use-module (srfi srfi-9)
  #:use-module ((system foreign) #:select (void
                                           (int . ffi:int)
                                           (size_t . ffi:size_t)
                                           (uintptr_t . ffi:uintptr_t)))
  #:export (%candidate
            %make-candidate
            candidate?
            pointer->candidate
            candidate-text
            candidate-comment
            candidate-reserved
            candidate-list-iterator?
            candidate-list-iterator->point
            candidate-list-iterator-index
            candidate-list-iterator-candidate
            select-candidate
            select-candidate-on-current-page
            candidate-list-begin
            candidate-list-next
            candidate-list-end
            candidate-list-from-index))

(define get-api-funcation %guile-rime-get-api-funcation)

(define %candidate
  (bs:struct
   `((text ,char*)
     (comment ,char*)
     (reserved ,(bs:pointer void)))))

(define-record-type <candidate>
  (%make-candidate bytestructure)
  candidate?
  (bytestructure candidate-bytestructure))

(define (make-candidate-bytestructure)
  (%make-candidate (bytestructure %candidate)))

(define (pointer->candidate pointer)
  (pointer->bytestructure pointer %candidate))

(define (candidate-text candidate)
  (make-pointer->string (bytestructure-ref (candidate-bytestructure candidate) 'text)))

(define (candidate-comment candidate)
  (make-pointer->string (bytestructure-ref (candidate-bytestructure candidate) 'comment)))

(define (candidate-reserved candidate)
  (bytestructure-ref (candidate-bytestructure candidate) 'reserved))

(define %candidate-list-iterator (bs:struct `((ptr ,(bs:pointer void))
                                              (index ,int)
                                              (candidate ,%candidate))))

(define-record-type <candidate-list-iterator>
  (%make-candidate-list-iterator bytestructure)
  candidate-list-iterator?
  (bytestructure candidate-list-iterator-bytestructure))

(define (make-candidate-list-iterator-bytestructure)
  (%make-candidate-list-iterator (bytestructure %candidate-list-iterator)))

(define (candidate-list-iterator->point iterator)
  (bytestructure->pointer
   (candidate-list-iterator-bytestructure
    iterator)))

(define (candidate-list-iterator-ptr iterator)
  (bytestructure-ref (candidate-list-iterator-bytestructure iterator) 'ptr))

(define (candidate-list-iterator-index iterator)
  (bytestructure-ref (candidate-list-iterator-bytestructure iterator) 'index))

(define (candidate-list-iterator-candidate iterator)
  (%make-candidate (bytestructure-ref
                    (candidate-list-iterator-bytestructure iterator)
                    'candidate)))

(define %select-candidate
  (get-api-funcation
   'select-candidate
   ffi:int (list ffi:uintptr_t ffi:size_t)))

(define (select-candidate session-id index)
  (%select-candidate session-id index))

(define %select-candidate-on-current-page
  (get-api-funcation 'select-candidate-on-current-page
                     ffi:int
                     (list ffi:uintptr_t ffi:size_t)))

(define (select-candidate-on-current-page session-id index)
  (%select-candidate-on-current-page session-id index))

(define %candidate-list-begin
  (get-api-funcation 'candidate-list-begin ffi:int (list ffi:uintptr_t '*)))

(define* (candidate-list-begin session-id
                               #:optional
                               (iterator
                                (make-candidate-list-iterator-bytestructure)))
  (c-int->bool (%candidate-list-begin
                session-id
                (candidate-list-iterator->point
                 iterator)))
  iterator)

(define %candidate-list-next
  (get-api-funcation 'candidate-list-next ffi:int '(*)))

(define (candidate-list-next iterator)
  (c-int->bool (%candidate-list-next (candidate-list-iterator->point iterator)))
  iterator)

(define %candidate-list-end
  (get-api-funcation 'candidate-list-end void '(*)))

(define (candidate-list-end iterator)
  (%candidate-list-end (candidate-list-iterator->point iterator)))

(define %candidate-list-from-index
  (get-api-funcation
   'candidate-list-from-index
   ffi:int
   (list ffi:uintptr_t '* ffi:int)))

(define (candidate-list-from-index session-id iterator index)
  (%candidate-list-from-index
   session-id
   (candidate-list-iterator->point iterator)
   index))
