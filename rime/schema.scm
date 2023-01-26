(define-module (rime schema)
  #:use-module (rime api)
  #:use-module (rime utils)
  #:use-module (srfi srfi-9)
  #:use-module (bytestructures guile)
  #:use-module ((system foreign) #:select (make-pointer
                                           string->pointer
                                           pointer->string
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
            select-schema)
  #:export-syntax
  (check-schema-list-item?))

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

(define-check check-schema-list-item?
  schema-list-item? "This is not a <schema-list-item> record!")

(define (schema-list-item-id schema-list-item)
  (check-schema-list-item? schema-list-item)
  (make-pointer->string (bytestructure-ref
                         (schema-list-item-bytestructure
                          schema-list-item)
                         'schema-id)))

(define (schema-list-item-name schema-list-item)
  (check-schema-list-item? schema-list-item)
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

(define-check check-schema-list?
  schema-list? "This is not a <schema-list> record!")

(define (make-schema-list)
  (%make-schema-list
   (bytestructure %schema-list)))

(define (schema-list->pointer schema-list)
  (check-schema-list? schema-list)
  (bytestructure->pointer
   (schema-list-bytestructure schema-list)))

(define (schema-list-size schema-list)
  (check-schema-list? schema-list)
  (bytestructure-ref
   (schema-list-bytestructure schema-list)
   'size))

(define (schema-list-list schema-list)
  (check-schema-list? schema-list)
  (let* ((size (schema-list-size schema-list))
         (items (pointer->bytestructure
                 (make-pointer (bytestructure-ref
                                (schema-list-bytestructure schema-list)
                                'list))
                 (bs:vector size
                            %schema-list-item))))
    (let loop ((l '())
               (num 0))
      (if (< num size)
          (loop (cons (%make-schema-list-item
                       (bytestructure-ref items num))
                      l)
                (1+ num))
          (reverse l)))))

(define %get-schema-list
  (get-api-funcation 'get-schema-list ffi:int '(*)))

(define* (get-schema-list #:optional (schema-list (make-schema-list)))
  (check-schema-list? schema-list)
  (%get-schema-list (schema-list->pointer schema-list))
  schema-list)

(define %free-schema-list
  (get-api-funcation 'free-schema-list void '(*)))

(define (free-schema-list schema-list)
  (check-schema-list? schema-list)
  (%free-schema-list (schema-list->pointer schema-list)))

(define %get-current-schema
  (get-api-funcation 'get-current-schema
                     ffi:int (list ffi:uintptr_t '* ffi:size_t)))

(define* (get-current-schema session-id #:optional
                             (schema-id (make-string 20))
                             (buffer-size (string-length schema-id)))
  (check-number? session-id)
  (check-string? schema-id)
  (check-number? buffer-size)
  (let ((p (string->pointer schema-id)))
    (%get-current-schema session-id p buffer-size)
    (pointer->string p)))

(define %select-schema
  (get-api-funcation 'select-schema ffi:int (list ffi:uintptr_t '* )))

(define (select-schema session-id schema-id)
  ;; (check-number? session-id)
  ;; (check-string? schema-id)
  (%select-schema session-id (string->pointer schema-id)))
