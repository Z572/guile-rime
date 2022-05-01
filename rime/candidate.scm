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

(define-check check-candidate?
  candidate? "This is not <candidate> record!")

(define (make-candidate-bytestructure)
  (%make-candidate (bytestructure %candidate)))

(define (pointer->candidate pointer)
  (check-pointer? pointer)
  (pointer->bytestructure pointer %candidate))

(define (candidate->pointer candidate)
  (check-candidate? candidate)
  (bytestructure->pointer (candidate-bytestructure candidate)))

(set-rime-record-printer! <candidate> "candidate" candidate->pointer)

(define (candidate-text candidate)
  (check-candidate? candidate)
  (make-pointer->string (bytestructure-ref (candidate-bytestructure candidate) 'text)))

(define (candidate-comment candidate)
  (check-candidate? candidate)
  (make-pointer->string (bytestructure-ref (candidate-bytestructure candidate) 'comment)))

(define (candidate-reserved candidate)
  (check-candidate? candidate)
  (bytestructure-ref (candidate-bytestructure candidate) 'reserved))

(define %candidate-list-iterator (bs:struct `((ptr ,(bs:pointer void))
                                              (index ,int)
                                              (candidate ,%candidate))))

(define-record-type <candidate-list-iterator>
  (%make-candidate-list-iterator bytestructure)
  candidate-list-iterator?
  (bytestructure candidate-list-iterator-bytestructure))

(define-check check-candidate-list-iterator?
  candidate-list-iterator? "This is not <candidate-list-iterator> record!")

(define (make-candidate-list-iterator-bytestructure)
  (%make-candidate-list-iterator (bytestructure %candidate-list-iterator)))

(define (candidate-list-iterator->point iterator)
  (check-candidate-list-iterator? iterator)
  (bytestructure->pointer
   (candidate-list-iterator-bytestructure
    iterator)))

(define (candidate-list-iterator-ptr iterator)
  (check-candidate-list-iterator? iterator)
  (bytestructure-ref (candidate-list-iterator-bytestructure iterator) 'ptr))

(define (candidate-list-iterator-index iterator)
  (check-candidate-list-iterator? iterator)
  (bytestructure-ref (candidate-list-iterator-bytestructure iterator) 'index))

(define (candidate-list-iterator-candidate iterator)
  (check-candidate-list-iterator? iterator)
  (%make-candidate (bytestructure-ref
                    (candidate-list-iterator-bytestructure iterator)
                    'candidate)))

(define %select-candidate
  (get-api-funcation
   'select-candidate
   ffi:int (list ffi:uintptr_t ffi:size_t)))

(define (select-candidate session-id index)
  (check-number? index)
  (%select-candidate session-id index))

(define %select-candidate-on-current-page
  (get-api-funcation 'select-candidate-on-current-page
                     ffi:int
                     (list ffi:uintptr_t ffi:size_t)))

(define (select-candidate-on-current-page session-id index)
  (check-number? index)
  (%select-candidate-on-current-page session-id index))

(define %candidate-list-begin
  (get-api-funcation 'candidate-list-begin ffi:int (list ffi:uintptr_t '*)))

(define* (candidate-list-begin session-id
                               #:optional
                               (iterator
                                (make-candidate-list-iterator-bytestructure)))
  (check-candidate-list-iterator? iterator)
  (c-int->bool (%candidate-list-begin
                session-id
                (candidate-list-iterator->point
                 iterator)))
  iterator)

(define %candidate-list-next
  (get-api-funcation 'candidate-list-next ffi:int '(*)))

(define (candidate-list-next iterator)
  (check-candidate-list-iterator? iterator)
  (c-int->bool (%candidate-list-next (candidate-list-iterator->point iterator)))
  iterator)

(define %candidate-list-end
  (get-api-funcation 'candidate-list-end void '(*)))

(define (candidate-list-end iterator)
  (check-candidate-list-iterator? iterator)
  (%candidate-list-end (candidate-list-iterator->point iterator)))

(define %candidate-list-from-index
  (get-api-funcation
   'candidate-list-from-index
   ffi:int
   (list ffi:uintptr_t '* ffi:int)))

(define (candidate-list-from-index session-id iterator index)
  (check-candidate-list-iterator? iterator)
  (check-number? index)
  (%candidate-list-from-index
   session-id
   (candidate-list-iterator->point iterator)
   index))
