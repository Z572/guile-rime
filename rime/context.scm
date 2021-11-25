(define-module (rime context)
  #:use-module (rime api)
  #:use-module (rime traits)
  #:use-module (rime utils)
  #:use-module (rime menu)
  #:use-module (rime composition)
  #:use-module (bytestructures guile)
  #:use-module ((system foreign) #:select ((int . ffi:int)
                                           (uintptr_t . ffi:uintptr_t)))
  #:use-module (srfi srfi-9)
  #:export (context?
            context->pointer
            context-composition
            context-select-labels
            context-menu
            context-commit-text-preview
            get-context
            free-context))

(define get-api-funcation %guile-rime-get-api-funcation)

(define %context (bs:struct `((data-size ,int)
                              (composition ,%composition)
                              (menu ,%menu)
                              (commit-text-preview ,char*)
                              (select-labels ,(bs:pointer '*;; char*
                                                          )))))

(define-record-type <context>
  (%make-context bytestructure)
  context?
  (bytestructure context-bytestructure))

(define (make-context-bytestructure)
  (struct-init %make-context %context))

(define (context->pointer context)
  (bytestructure->pointer (context-bytestructure context)))

(define (context-composition context)
  (%make-composition (bytestructure-ref (context-bytestructure context) 'composition)))

(define (context-select-labels context)
  ;; (bytestructure-ref (context-bytestructure context)
  ;;                    'select-labels)
  (let*  ((select-labels (bytestructure-ref (context-bytestructure context)
                                            'select-labels))
          ;; (menu (context-menu context))
          )
    select-labels
    ;; (if (= 0 select-labels)
    ;;     (menu-select-keys menu)
    ;;     (pk 'vvv "hhh"))
    )
  ;; (make-pointer->string (bytestructure-ref (pointer->bytestructure
  ;;                                           (make-pointer sdlkfj)
  ;;                                           (bs:vector (menu-page-size (context-menu context)) char* )) 1))
  )

(define (context-menu context)
  (%make-menu (bytestructure-ref (context-bytestructure context) 'menu)))

(define (context-commit-text-preview context)
  (make-pointer->string (bytestructure-ref (context-bytestructure context) 'commit-text-preview)))

(define %get-context
  (get-api-funcation 'get-context ffi:int (list ffi:uintptr_t '*)))

(define* (get-context session #:optional
                      (context (make-context-bytestructure)))
  (%get-context session (context->pointer context))
  context)
(define %free-context
  (get-api-funcation 'free-context ffi:int '(*)))

(define (free-context context)
  (%free-context (context->pointer context)))
