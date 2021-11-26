(define-module (rime commit)
  #:use-module (rime utils)
  #:use-module (rime api)
  #:use-module (srfi srfi-9)
  #:use-module (bytestructures guile)
  #:use-module ((system foreign) #:select ((int . ffi:int)
                                           (uintptr_t . ffi:uintptr_t)))
  #:export (commit?
            commit->pointer
            commit-text
            get-commit
            free-commit)
  #:export-syntax
  (check-commit?))

(define get-api-funcation %guile-rime-get-api-funcation)

(define %commit (bs:struct `((data-size ,int)
                             (text ,char*))))

(define-record-type <commit>
  (%make-commit bytestructure)
  commit?
  (bytestructure commit-bytestructure))

(define (make-commit-bytestructure)
  (struct-init %make-commit %commit))

(define-check check-commit?
  commit? "This is not <commit> record!")

(define (commit->pointer commit)
  (check-commit? commit)
  (bytestructure->pointer (commit-bytestructure commit)))

(define (commit-text commit)
  (check-commit? commit)
  (make-pointer->string (bytestructure-ref
                         (commit-bytestructure commit)
                         'text)))


(define %get-commit
  (get-api-funcation
   'get-commit
   ffi:int (list ffi:uintptr_t '*)))

(define* (get-commit session-id #:optional (commit (make-commit-bytestructure)))
  (check-commit? commit)
  (%get-commit session-id (commit->pointer commit))
  commit)

(define %free-commit
  (get-api-funcation
   'free-commit
   ffi:int
   (list  '*)))

(define (free-commit commit)
  (check-commit? commit)
  (%free-commit (commit->pointer commit)))
