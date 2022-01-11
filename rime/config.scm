(define-module (rime config)
  #:use-module (rime api)
  #:use-module (rime utils)
  #:use-module (srfi srfi-9)
  #:use-module (bytestructures guile)
  #:use-module (rnrs bytevectors)
  #:use-module ((system foreign) #:select (pointer->string
                                           string->pointer
                                           void
                                           (int . ffi:int)
                                           (double . ffi:double)
                                           (size_t . ffi:size_t)))
  #:export (config->pointer
            pointer->config
            config-iterator?
            config-iterator->pointer
            schema-open
            config-open
            config-close
            config-get-bool
            config-get-int
            config-get-double
            config-get-string
            config-get-cstring
            config-update-signature
            config-begin-map
            config-next
            config-end
            user-config-open
            config-init
            config-load-string
            config-set-bool
            config-set-int
            config-set-double
            config-set-string
            config-get-item
            config-set-item
            config-clear
            config-create-list
            config-create-map
            config-list-size
            config-begin-list)
  #:export-syntax
  (check-config?
   check-config-iterator?))

(define get-api-funcation %guile-rime-get-api-funcation)

(define %config
  (bs:struct
   `((ptr ,(bs:pointer void)))))

(define-record-type <config>
  (%make-config bytestructure)
  config?
  (bytestructure config-bytestructure))

(define-check check-config?
  config? "This is not a <config> record!")

(define (make-config-bytestructure)
  (%make-config (bytestructure %config)))

(define (config->pointer config)
  (check-config? config)
  (bytestructure->pointer (config-bytestructure config)))

(define (pointer->config pointer)
  (%make-config (pointer->bytestructure pointer %config)))

(define (config-ptr config)
  (check-config? config)
  (bytestructure-ref
   (config-bytestructure config)
   'ptr))

(define %config-iterator
  (bs:struct
   `((list ,(bs:pointer void))
     (map ,(bs:pointer void))
     (index ,int)
     (key ,char*)
     (path ,char*))))

(define-record-type <config-iterator>
  (%make-config-iterator bytestructure)
  config-iterator?
  (bytestructure config-iterator-bytestructure))

(define-check check-config-iterator?
  config-iterator? "This is not a <config-iterator> record!")

(define (config-iterator->pointer iterator)
  (check-config-iterator? iterator)
  (bytestructure->pointer
   (config-iterator-bytestructure
    iterator)))

(define %schema-open
  (get-api-funcation 'schema-open ffi:int '(* *)))

(define (schema-open schema-id)
  (check-string? schema-id)
  (let ((p (config->pointer (make-config-bytestructure))))
    (%schema-open (string->pointer schema-id) p)
    (pointer->config p)))

(define %config-open
  (get-api-funcation 'config-open ffi:int '(* *)))

(define* (config-open config-id #:optional (config (make-config-bytestructure)))
  (check-string? config-id)
  (check-config? config)
  (%config-open (string->pointer config-id) (config->pointer config))
  config)

(define %config-close
  (get-api-funcation
   'config-close ffi:int
   (list '*)))

(define (config-close config)
  (check-config? config)
  (%config-close (config->pointer config)))

(define %config-get-bool
  (get-api-funcation
   'config-get-bool ffi:int
   (list '* '* '*)))

(define (config-get-bool config key)
  (check-string? key)
  (check-config? config)
  (let ((v (bytestructure->pointer (bytestructure int))))
    (c-int->bool (%config-get-bool (config->pointer config)
                                   (string->pointer key) v))
    (c-int->bool (bytestructure-ref (pointer->bytestructure v int)))))

(define %config-get-int
  (get-api-funcation
   'config-get-int ffi:int
   (list '* '* '*)))

(define (config-get-int config key)
  (check-string? key)
  (check-config? config)
  (let ((v (bytestructure->pointer (bytestructure int))))
    (%config-get-int (config->pointer config) (string->pointer key) v)
    (bytestructure-ref (pointer->bytestructure v int))))

(define %config-get-double
  (get-api-funcation
   'config-get-double
   ffi:int
   (list '* '* '*)))

(define (config-get-double config key)
  (check-string? key)
  (check-config? config)
  (let ((v (bytestructure->pointer (bytestructure double))))
    (%config-get-double (config->pointer config) (string->pointer key) v)
    (bytestructure-ref (pointer->bytestructure v double))))

(define %config-get-string
  (get-api-funcation
   'config-get-string
   ffi:int
   (list '* '* '* ffi:size_t)))

(define* (config-get-string config key #:optional (value (make-string 30)) (buffer-size (string-length value)))
  (check-string? key)
  (check-config? config)
  (let ((s (string->pointer value)))
    (if (c-int->bool
         (%config-get-string
          (config->pointer config)
          (string->pointer key)
          s buffer-size))
        (pointer->string s))))

(define %config-get-cstring
  (get-api-funcation
   'config-get-cstring
   '*
   (list '* '* )))

(define (config-get-cstring config key)
  (check-config? config)
  (check-string? key)
  (let ((ptr (%config-get-cstring
              (config->pointer config)
              (string->pointer key))))
    (pointer->string ptr)))

(define %config-update-signature
  (get-api-funcation
   'config-update-signature ffi:int
   (list '* '*)))

(define (config-update-signature config key)
  (check-config? config)
  (check-string? key)
  (let* ((c (config->pointer config))
         (out (%config-update-signature c (string->pointer key))))
    (if (c-int->bool out)
        (pointer->config c))))

(define %config-begin-map
  (get-api-funcation
   'config-begin-map ffi:int
   (list '* '* '*)))

(define (config-begin-map iterator config key)
  (check-config-iterator? iterator)
  (check-config? config)
  (check-string? key)
  (%config-begin-map (config-iterator->pointer iterator)
                     (config->pointer config)
                     (string->pointer key)))

(define %config-next
  (get-api-funcation
   'config-next ffi:int
   (list '*)))

(define (config-next iterator)
  (check-config-iterator? iterator)
  (%config-next (config-iterator->pointer iterator)))

(define %config-end
  (get-api-funcation
   'config-end ffi:int
   (list '*)))

(define (config-end iterator)
  (check-config-iterator? iterator)
  (%config-end (config-iterator->pointer iterator)))

(define %user-config-open
  (get-api-funcation
   'user-config-open
   ffi:int
   (list '* '*)))

(define* (user-config-open config-id #:optional
                           (config (make-config-bytestructure)))
  (check-string? config-id)
  (check-config? config)
  (and (c-int->bool
        (%user-config-open
         (string->pointer config-id)
         (config->pointer config)))
       config))
(define %config-init
  (get-api-funcation 'config-init ffi:int '(*)))

(define (config-init config)
  (check-config? config)
  (%config-init (config->pointer config))
  config)

(define %config-load-string
  (get-api-funcation
   'config-load-string ffi:int '(* *)))

(define (config-load-string config yaml)
  (check-config? config)
  (check-string? yaml)
  (%config-load-string
   (config->pointer config)
   (string->pointer yaml)))

(define %config-set-bool
  (get-api-funcation
   'config-set-bool
   ffi:int
   (list '* '* ffi:int)))

(define (config-set-bool config key value)
  (check-config? config)
  (check-string? key)
  (%config-set-bool
   (config->pointer config)
   (string->pointer key)
   (bool->c-int value)))

(define %config-set-int
  (get-api-funcation
   'config-set-int
   ffi:int
   (list '* '* ffi:int)))

(define (config-set-int config key value)
  (check-config? config)
  (check-string? key)
  (%config-set-int
   (config->pointer config)
   (string->pointer key)
   value))

(define %config-set-double
  (get-api-funcation
   'config-set-double
   ffi:int
   (list '* '* ffi:double)))

(define (config-set-double config key value)
  (check-config? config)
  (check-string? key)
  (c-int->bool (%config-set-double (config->pointer config) (string->pointer key) value)))

(define %config-set-string
  (get-api-funcation
   'config-set-string
   ffi:int
   (list '* '* '*)))

(define (config-set-string config key value)
  (check-config? config)
  (check-string? key)
  (c-int->bool
   (%config-set-string
    (config->pointer config)
    (string->pointer key)
    (string->pointer value))))

(define %config-get-item
  (get-api-funcation
   'config-get-item
   ffi:int
   (list '* '* '*)))

(define (config-get-item config key)
  (check-config? config)
  (check-string? key)
  (let ((p (config->pointer (make-config-bytestructure))))
    (if (c-int->bool (%config-get-item
                      (config->pointer config)
                      (string->pointer key)
                      p))
        (pointer->config p))))

(define %config-set-item
  (get-api-funcation
   'config-set-item
   ffi:int (list '* '* '*)))

(define (config-set-item config key value)
  (check-config? config)
  (check-string? key)
  (%config-set-item
   (config->pointer config)
   (string->pointer key)
   (config->pointer value)))

(define %config-clear
  (get-api-funcation
   'config-clear
   ffi:int '(* *)))

(define (config-clear config key)
  (check-config? config)
  (check-string? key)
  (%config-clear
   (config->pointer config)
   (string->pointer key)))

(define %config-create-list
  (get-api-funcation
   'config-create-list ffi:int '(* *)))

(define (config-create-list config key)
  (check-config? config)
  (check-string? key)
  (%config-create-list (config->pointer config) (string->pointer key)))

(define %config-create-map
  (get-api-funcation
   'config-create-map ffi:int '(* *)))

(define (config-create-map config key)
  (check-config? config)
  (check-string? key)
  (if (c-int->bool
       (%config-create-map
        (config->pointer config)
        (string->pointer key)))
      config))

(define %config-list-size
  (get-api-funcation
   'config-list-size ffi:size_t '(* *)))

(define (config-list-size config key)
  (check-config? config)
  (check-string? key)
  (%config-list-size (config->pointer config) (string->pointer key)))

(define %config-begin-list
  (get-api-funcation
   'config-begin-list ffi:int '(* * *)))

(define (config-begin-list iterator config key)
  (check-config-iterator? iterator)
  (check-config? config)
  (check-string? key)
  (%config-begin-list
   (config-iterator->pointer iterator)
   (config->pointer config)
   (string->pointer key)))
