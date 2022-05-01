(define-module (rime status)
  #:use-module (rime configuration)
  #:use-module (bytestructures guile)
  #:use-module (srfi srfi-9)
  #:use-module (rime api)
  #:use-module (rime utils)
  #:use-module ((system foreign) #:select ((int . ffi:int)
                                           (uintptr_t . ffi:uintptr_t)))
  #:export (status-is-ascii-mode
            status-is-ascii-punct
            status-is-composing
            status-is-disabled
            status-is-full-shape
            status-is-simplified
            status-is-traditional
            status-schema-id
            status-schema-name
            status->point
            make-status
            status?
            get-status
            free-status)
  #:export-syntax
  (check-status?))

(define get-api-funcation %guile-rime-get-api-funcation)

(define %status (bs:struct `((data-size ,int)
                             (schema-id ,char*)
                             (schema-name ,char*)
                             (is-disabled ,bool)
                             (is-composing ,bool)
                             (is-ascii-mode ,bool)
                             (is-full-shape ,bool)
                             (is-simplified ,bool)
                             (is-traditional ,bool)
                             (is-ascii-punct ,bool))))

(define-record-type <status>
  (%make-status bytestructure)
  status?
  (bytestructure status-bytestructure))

(define-check check-status?
  status?
  "This is not a <status> record!")

(define (make-status)
  (struct-init %make-status %status))

(define (status->point status)
  (check-status? status)
  (bytestructure->pointer (status-bytestructure status)))

(set-rime-record-printer! <status> "status" status->point)

(define (status-schema-id status)
  (check-status? status)
  (make-pointer->string (bytestructure-ref
                         (status-bytestructure status)
                         'schema-id)))

(define (status-schema-name status)
  (check-status? status)
  (make-pointer->string (bytestructure-ref
                         (status-bytestructure status)
                         'schema-name)))

(define (status-is-disabled status)
  (check-status? status)
  (c-int->bool (bytestructure-ref (status-bytestructure status) 'is-disabled)))

(define (status-is-composing status)
  (check-status? status)
  (c-int->bool (bytestructure-ref (status-bytestructure status) 'is-composing)))

(define (status-is-ascii-mode status)
  (check-status? status)
  (c-int->bool (bytestructure-ref (status-bytestructure status) 'is-ascii-mode)))

(define (status-is-full-shape status)
  (check-status? status)
  (c-int->bool (bytestructure-ref (status-bytestructure status) 'is-full-shape)))

(define (status-is-simplified status)
  (check-status? status)
  (c-int->bool (bytestructure-ref (status-bytestructure status) 'is-simplified)))

(define (status-is-traditional status)
  (check-status? status)
  (c-int->bool (bytestructure-ref (status-bytestructure status) 'is-traditional)))

(define (status-is-ascii-punct status)
  (check-status? status)
  (c-int->bool (bytestructure-ref (status-bytestructure status) 'is-ascii-punct)))

(define %get-status
  (get-api-funcation 'get-status ffi:int (list ffi:uintptr_t '*)))

(define* (get-status session-id #:optional
                     (status (make-status)))
  (check-status? status)
  (%get-status session-id (status->point status))
  status)

(define %free-status
  (get-api-funcation 'free-status ffi:int '(*)))

(define (free-status status)
  (check-status? status)
  (%free-status (status->point status)))
