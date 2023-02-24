(define-module (rime structs)
  #:use-module (rime configuration)
  #:use-module (rime traits)
  #:use-module (rime menu)
  #:use-module (rime config)
  #:use-module (rime api)
  #:use-module (bytestructures guile)
  #:use-module (rnrs bytevectors)
  #:use-module ((system foreign) #:select (null-pointer?
                                           bytevector->pointer
                                           make-pointer
                                           procedure->pointer
                                           pointer->procedure
                                           pointer->bytevector
                                           pointer->string
                                           string->pointer
                                           sizeof
                                           %null-pointer
                                           dereference-pointer
                                           pointer-address
                                           void
                                           (int . ffi:int)
                                           (double . ffi:double)
                                           (size_t . ffi:size_t)
                                           (uintptr_t . ffi:uintptr_t)))
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-26)
  #:use-module (rime utils)
  #:export (finalize
            set-caret-pos
            initialize
            is-maintenance-mode
            join-maintenance-thread

            process-key
            set-notification-handler
            setup
            start-maintenance
            deploy-config-file
            simulate-key-sequence
            set-option
            get-input
            get-caret-pot
            deployer-initialize
            run-task
            option
            property))

(define get-api-funcation %guile-rime-get-api-funcation)

(define %setup (get-api-funcation 'setup void '(*)))

(define (setup traits)
  "Call this function before accessing any other rime functions."
  (check-traits? traits)
  (unless (traits-setuped? traits)
    (%setup (traits->pointer traits))
    (set-traits-setuped? traits #t)))

(define %set-notification-handler
  (get-api-funcation 'set-notification-handler void '(* *)))

(define* (set-notification-handler handler #:optional (context-object %null-pointer))
  " s
XXX: I don't know why ,sometime set it will let @{}join-maintenance-thread{} fail."
  (%set-notification-handler
   (procedure->pointer
    void
    (lambda (a b c d) (handler a b (pointer->string c) (pointer->string d)))
    (list '* ffi:uintptr_t '* '*))
   context-object))

(define %initialize
  (get-api-funcation 'initialize void '(*)))

(define (initialize traits)
  (check-traits? traits)
  (%initialize (traits->pointer traits)))

(define %finalize
  (get-api-funcation 'finalize void '()))

(define (finalize)
  (%finalize))

(define %start-maintenance
  (get-api-funcation 'start-maintenance ffi:int (list ffi:int)))

(define* (start-maintenance #:optional full-check)
  (c-int->bool (%start-maintenance (bool->c-int full-check))))

(define %is-maintenance-mode
  (get-api-funcation 'is-maintenance-mode ffi:int '()))
(define* (is-maintenance-mode)
  (c-int->bool (%is-maintenance-mode)))

(define %join-maintenance-thread
  (get-api-funcation
   'join-maintenance-thread
   void
   '()))
(define (join-maintenance-thread)
  (%join-maintenance-thread))

(define %deployer-initialize
  (get-api-funcation
   'deployer-initialize
   void
   '(*)))
(define (deployer-initialize traits)
  (check-traits? traits)
  (%deployer-initialize (traits->pointer traits)))

(define %prebuild
  (get-api-funcation
   'prebuild ffi:int '()))
(define (prebuild)
  (%prebuild))

(define %deploy
  (get-api-funcation
   'deploy ffi:int '()))

(define (deploy)
  (%deploy))

(define %deploy-schema
  (get-api-funcation
   'deploy-schema ffi:int '(*)))

(define (deploy-schema schema-file)
  (check-string? schema-file)
  (%deploy-schema (string->pointer schema-file)))
(define %deploy-config-file
  (get-api-funcation
   'deploy-config-file ffi:int '(* *)))

(define (deploy-config-file file-name version-key)
  (check-string? file-name)
  (check-string? version-key)
  (%deploy-config-file (string->pointer file-name)
                       (string->pointer version-key)))

(define %sync-user-data
  (get-api-funcation
   'sync-user-data ffi:int '()))

(define (sync-user-data)
  (%sync-user-data))

;;; input

(define %process-key
  (get-api-funcation
   'process-key ffi:int
   (list ffi:uintptr_t ffi:int ffi:int)))

(define (process-key session-id keycode mask)
  (check-number? session-id)
  (check-number? keycode)
  (check-number? mask)
  (c-int->bool (%process-key session-id keycode mask)))

;;; runtime options

(define %set-option
  (get-api-funcation 'set-option void (list ffi:uintptr_t '* ffi:int)))

(define (set-option session-id option value)
  (check-number? session-id)
  (check-string? option)
  (%set-option session-id
               (string->pointer option)
               (bool->c-int value)))

(define %get-option
  (get-api-funcation 'get-option ffi:int (list ffi:uintptr_t '*)))

(define (get-option session-id option)
  (check-number? session-id)
  (check-string? option)
  (%get-option session-id  (string->pointer option)))

(define option (make-procedure-with-setter get-option set-option))


(define %set-property
  (get-api-funcation 'set-property ffi:int (list ffi:uintptr_t '* '*)))

(define (set-property session-id prop value)
  (check-number? session-id)
  (check-string? prop)
  (check-string? value)
  (%set-property session-id
                 (string->pointer prop)
                 (string->pointer value)))

(define %get-property
  (get-api-funcation 'get-property ffi:int (list ffi:uintptr_t '* ffi:int ffi:size_t)))

(define (get-property session-id prop value buffer-size)
  (check-number? session-id)
  (check-string? prop)
  (check-string? value)
  (check-number? buffer-size)
  (%get-property session-id
                 (string->pointer prop)
                 (string->pointer-address value)
                 buffer-size))

(define property
  (make-procedure-with-setter get-property set-property))

;;; testing

(define %simulate-key-sequence
  (get-api-funcation 'simulate-key-sequence ffi:int
                     (list ffi:uintptr_t '*)))

(define (simulate-key-sequence session-id key-sequence)
  (check-number? session-id)
  (check-string? key-sequence)
  (%simulate-key-sequence
   session-id (string->pointer key-sequence)))

;;; module

(define %run-task
  (get-api-funcation 'run-task ffi:int '(*)))
(define (run-task task-name)
  (check-string? task-name)
  (%run-task (string->pointer task-name)))



;;; initialize an empty config object


;;; get raw input

(define %get-input
  (get-api-funcation
   'get-input
   '* (list ffi:uintptr_t)))

(define (get-input session-id)
  (check-number? session-id)
  "get raw input string for @{}process-key inputs"
  (pointer->string (%get-input session-id)))

(define %get-caret-pot
  (get-api-funcation 'get-caret-pot ffi:size_t (list ffi:uintptr_t)))

(define (get-caret-pot session-id)
  (check-number? session-id)
  (%get-caret-pot session-id))

(define %set-caret-pos
  (get-api-funcation 'set-caret-pos void (list ffi:uintptr_t ffi:size_t)))

(define (set-caret-pos session-id caret-pos)
  (check-number? session-id)
  (check-number? caret-pos)
  (%set-caret-pos session-id caret-pos))
