(define-module (rime schema)
  #:use-module (rime api)
  #:use-module (rime utils)
  #:use-module (srfi srfi-9)
  #:use-module (bytestructures guile)
  #:use-module ((system foreign) #:select (make-pointer
                                           string->pointer
                                           void
                                           (int . ffi:int)
                                           (uintptr_t . ffi:uintptr_t)
                                           (size_t . ffi:size_t)))
  #:export (schema-list-item?
            schema-list-item-id
            schema-list-item-name
            schema-list?
            schema-list->pointer
            schema-list-size
            schema-list-list
            get-schema-list
            free-schema-list
            get-current-schema
            select-schema))

(define get-api-funcation %guile-rime-get-api-funcation)

(define %schema-list-item
  (bs:struct
   `((schema-id ,char*)
     (name ,char*)
     (reserved ,(bs:pointer void)))))

(define-record-type <schema-list-item>
  (%make-schema-list-item bytestructure)
  schema-list-item?
  (bytestructure schema-list-item-bytestructure))

(define (schema-list-item-id schema-list-item)
  (make-pointer->string (bytestructure-ref
                         (schema-list-item-bytestructure
                          schema-list-item)
                         'schema-id)))

(define (schema-list-item-name schema-list-item)
  (make-pointer->string (bytestructure-ref
                         (schema-list-item-bytestructure
                          schema-list-item)
                         'name)))

(define (make-schema-list-item-bytestructure)
  (%make-schema-list-item
   (bytestructure %schema-list-item)))

(define %schema-list
  (bs:struct
   `((size ,size_t)
     (list ,(bs:pointer
             %schema-list-item)))))

(define-record-type <schema-list>
  (%make-schema-list bytestructure)
  schema-list?
  (bytestructure
   schema-list-bytestructure))

(define (make-schema-list-bytestructure)
  (%make-schema-list
   (bytestructure %schema-list)))

(define (schema-list->pointer schema-list)
  (bytestructure->pointer
   (schema-list-bytestructure schema-list)))

(define (schema-list-size schema-list)
  (bytestructure-ref
   (schema-list-bytestructure schema-list)
   'size))

(define (schema-list-list schema-list)
  (define size (schema-list-size schema-list))
  (define items (pointer->bytestructure
                 (make-pointer (bytestructure-ref
                                (schema-list-bytestructure schema-list)
                                'list))
                 (bs:vector size
                            %schema-list-item)))
  (let loop ((l '())
             (num 0))
    (if (< num size)
        (loop (cons (%make-schema-list-item
                     (bytestructure-ref items num))
                    l)
              (1+ num))
        (reverse l))))

(define %get-schema-list
  (get-api-funcation 'get-schema-list ffi:int '(*)))

(define* (get-schema-list #:optional (schema-list (make-schema-list-bytestructure)))
  (%get-schema-list (schema-list->pointer schema-list))
  schema-list)

(define %free-schema-list
  (get-api-funcation 'free-schema-list void '(*)))

(define (free-schema-list schema-list)
  (%free-schema-list (schema-list->pointer schema-list)))

(define %get-current-schema
  (get-api-funcation 'get-current-schema
                     ffi:int (list ffi:uintptr_t '* ffi:size_t)))

(define* (get-current-schema session-id #:optional
                             (schema-id (bytestructure (bs:string 10 'utf8)))
                             (buffer-size (bytestructure-size schema-id) ))

  (%get-current-schema session-id
                       (bytestructure->pointer schema-id)
                       buffer-size)
  (bytestructure-ref schema-id))

(define %select-schema
  (get-api-funcation 'select-schema ffi:int (list ffi:uintptr_t '* )))

(define (select-schema session-id schema-id)
  (%select-schema session-id (string->pointer schema-id)))
