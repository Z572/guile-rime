(define-module (rime structs)
  #:use-module (rime configuration)
  #:use-module (rime traits)
  #:use-module (rime candidate)
  #:use-module (rime composition)
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
  #:export (commit-text
            context-commit-text-preview
            context-composition
            context-menu
            context-select-labels
            finalize
            free-commit
            free-status
            set-caret-pos
            get-commit
            get-context
            get-current-schema
            get-schema-list
            get-shared-data-dir
            get-status
            get-user-data-dir
            get-user-id
            get-version
            initialize
            is-maintenance-mode
            join-maintenance-thread

            custom-api-bytestructure
            menu-page-size
            menu-page-no
            menu-is-last-page
            menu-highlighted-candidate-index
            menu-num-candidates
            menu-candidates
            menu-select-keys
            process-key
            schema-list-item-id
            schema-list-item-name
            schema-list-list
            select-schema
            set-notification-handler
            ;; set-traits-app-name!
            ;; set-traits-distribution-name!
            ;; set-traits-log-dir!
            ;; set-traits-shared-data-dir!
            setup
            free-context
            start-maintenance
            status-is-ascii-mode
            status-is-ascii-punct
            status-is-composing
            status-is-disabled
            status-is-full-shape
            status-is-simplified
            status-is-traditional
            status-schema-id
            status-schema-name
            get-prebuilt-data-dir
            get-staging-dir
            deploy-config-file
            get-user-data-sync-dir
            simulate-key-sequence
            set-option
            find-module
            get-input
            get-caret-pot
            deployer-initialize
            module-get-api
            module-module-name
            module-initialize
            module-finalize
            run-task
            get-sync-dir))

(define get-api-funcation %guile-rime-get-api-funcation)

;;; menu
(define %menu
  (bs:struct
   `((page-size ,int)
     (page-no ,int)
     (is-last-page ,bool)
     (highlighted-candidate-index ,int)
     (num-candidates ,int)
     (candidates ,(bs:pointer %candidate))
     (select-keys ,char*))))

(define-record-type <menu>
  (%make-menu bytestructure)
  menu?
  (bytestructure menu-bytestructure))

(define (make-menu-bytestructure)
  (%make-menu (bytestructure %menu)))

(define (menu->pointer menu)
  (bytestructure->pointer (menu-bytestructure menu)))

(define (menu-page-size menu)
  (bytestructure-ref (menu-bytestructure menu) 'page-size))

(define (menu-page-no menu)
  (bytestructure-ref (menu-bytestructure menu) 'page-no))

(define (menu-is-last-page menu)
  (c-int->bool (bytestructure-ref (menu-bytestructure menu) 'is-last-page)))

(define (menu-highlighted-candidate-index menu)
  (bytestructure-ref (menu-bytestructure menu) 'highlighted-candidate-index))

(define (menu-num-candidates menu)
  (bytestructure-ref (menu-bytestructure menu) 'num-candidates))

(define (menu-candidates menu)
  (if (zero? (menu-num-candidates menu))
      '()
      (let ((candidates* (pointer->bytestructure
                          (make-pointer
                           (bytestructure-ref (menu-bytestructure menu) 'candidates))
                          (bs:vector (menu-num-candidates menu) %candidate))))
        (let loop ((l '())
                   (num (- (menu-num-candidates menu) 1)))
          (if (< num 0)
              l
              (loop (cons (%make-candidate
                           (bytestructure-ref
                            candidates*
                            num)) l)
                    (- num 1)))))))

(define (menu-select-keys menu)
  (bytestructure-ref (menu-bytestructure menu) 'select-keys))


(define %commit (bs:struct `((data-size ,int)
                             (text ,char*))))

(define-record-type <commit>
  (%make-commit bytestructure)
  commit?
  (bytestructure commit-bytestructure))

(define (make-commit-bytestructure)
  (struct-init %make-commit %commit))

(define (commit->pointer commit)
  (bytestructure->pointer (commit-bytestructure commit)))

(define (commit-text commit)
  (make-pointer->string (bytestructure-ref
                         (commit-bytestructure commit)
                         'text)))

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

(define (make-status-bytestructure)
  (struct-init %make-status %status))

(define (status->point status)
  (bytestructure->pointer (status-bytestructure status)))

(define (status-schema-id status)
  (make-pointer->string (bytestructure-ref
                         (status-bytestructure status)
                         'schema-id)))

(define (status-schema-name status)
  (make-pointer->string (bytestructure-ref
                         (status-bytestructure status)
                         'schema-name)))

(define (status-is-disabled status)
  (c-int->bool (bytestructure-ref (status-bytestructure status) 'is-disabled)))

(define (status-is-composing status)
  (c-int->bool (bytestructure-ref (status-bytestructure status) 'is-composing)))

(define (status-is-ascii-mode status)
  (c-int->bool (bytestructure-ref (status-bytestructure status) 'is-ascii-mode)))

(define (status-is-full-shape status)
  (c-int->bool (bytestructure-ref (status-bytestructure status) 'is-full-shape)))

(define (status-is-simplified status)
  (c-int->bool (bytestructure-ref (status-bytestructure status) 'is-simplified)))

(define (status-is-traditional status)
  (c-int->bool (bytestructure-ref (status-bytestructure status) 'is-traditional)))

(define (status-is-ascii-punct status)
  (c-int->bool (bytestructure-ref (status-bytestructure status) 'is-ascii-punct)))





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

(define %custom-api
  (bs:struct
   `((data-size ,int))))

(define-record-type <custom-api>
  (%make-custom-api bytestructure)
  custom-api?
  (bytestructure custom-api-bytestructure))

(define %module
  (bs:struct
   `((data-size ,int)
     (module-name ,char*)
     (initialize ,(bs:pointer void))
     (finalize ,(bs:pointer void))
     (get-api ,(bs:pointer '*);; %custom-api
              ))))

(define-record-type <module>
  (%make-module bytestructure)
  module-handler?
  (bytestructure module-bytestructure))

(define (make-module-bytestructure)
  (struct-init %make-module %module))

(define (module->pointer module)
  (bytestructure->pointer
   (module-bytestructure module)))

(define (pointer->module pointer)
  (%make-module
   (pointer->bytestructure pointer %module)))

(define (module-get-api module)
  (let ((p (pointer->procedure
            '* (make-pointer (bytestructure-ref
                              (module-bytestructure module)
                              'get-api)) '())))
    (p)))
(define (module-module-name module)
  (make-pointer->string (bytestructure-ref (module-bytestructure module) 'module-name)))

(define (module-initialize module)
  (pointer->procedure void (make-pointer (bytestructure-ref (module-bytestructure module) 'initialize)) '()))

(define (module-finalize module)
  (pointer->procedure void (make-pointer (bytestructure-ref (module-bytestructure module) 'finalize)) '()))

(define %setup (get-api-funcation 'setup void '(*)))

(define (setup traits)
  "Call this function before accessing any other rime functions."
  (%setup (traits->pointer traits)))

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
  (%deploy-schema (string->pointer schema-file)))
(define %deploy-config-file
  (get-api-funcation
   'deploy-config-file ffi:int '(* *)))

(define (deploy-config-file file-name version-key)
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
  (c-int->bool (%process-key session-id keycode mask)))

;;; output

(define %get-commit
  (get-api-funcation
   'get-commit
   ffi:int (list ffi:uintptr_t '*)))

(define* (get-commit session-id #:optional (commit (make-commit-bytestructure)))
  (%get-commit session-id (commit->pointer commit))
  commit)

(define %free-commit
  (get-api-funcation
   'free-commit
   ffi:int
   (list  '*)))

(define (free-commit commit)
  (%free-commit (commit->pointer commit)))

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

(define %get-status
  (get-api-funcation 'get-status ffi:int (list ffi:uintptr_t '*)))

(define* (get-status session-id #:optional
                     (status (make-status-bytestructure)))
  (%get-status session-id (status->point status))
  status)

(define %free-status
  (get-api-funcation 'free-status ffi:int '(*)))
(define (free-status status)
  (%free-status (status->point status)))

;;; runtime options

(define %set-option
  (get-api-funcation 'set-option void (list ffi:uintptr_t '* ffi:int)))

(define (set-option session-id option value)
  (%set-option session-id
               (string->pointer option)
               (bool->c-int value)))

(define %get-option
  (get-api-funcation 'get-option ffi:int (list ffi:uintptr_t '*)))

(define (get-option session-id option)
  (%get-option session-id  (string->pointer option)))

(define %set-property
  (get-api-funcation 'set-property ffi:int (list ffi:uintptr_t '* '*)))

(define (set-property session-id prop value)
  (%set-property session-id
                 (string->pointer prop)
                 (string->pointer value)))

(define %get-property
  (get-api-funcation 'get-property ffi:int (list ffi:uintptr_t '* ffi:int ffi:size_t)))

(define (get-property session-id prop value buffer-size)
  (%get-property session-id
                 (string->pointer prop)
                 (string->pointer-address value)
                 buffer-size))

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

;;; configuration

(define %schema-open
  (get-api-funcation 'schema-open ffi:int '(* *)))

(define (schema-open schema-id config)
  (%schema-open (string->pointer schema-id) (config->pointer config)))

;;; testing

(define %simulate-key-sequence
  (get-api-funcation 'simulate-key-sequence ffi:int
                     (list ffi:uintptr_t '*)))

(define (simulate-key-sequence session-id key-sequence)
  (%simulate-key-sequence
   session-id (string->pointer key-sequence)))

;;; module

(define %register-module
  (get-api-funcation 'register-module ffi:int '(*)))

(define (register-module module)
  (%register-module (module->pointer module)))

(define %find-module
  (get-api-funcation 'find-module '* '(*)))

(define (find-module module-name)
  (pointer->module (%find-module (string->pointer module-name))))

(define %run-task
  (get-api-funcation 'run-task ffi:int '(*)))
(define (run-task task-name)
  (%run-task (string->pointer task-name)))

(define %get-shared-data-dir
  (get-api-funcation 'get-shared-data-dir '* '()))

(define (get-shared-data-dir)
  (pointer->string (%get-shared-data-dir)))

(define %get-user-data-dir
  (get-api-funcation 'get-user-data-dir '* '()))

(define (get-user-data-dir)
  (pointer->string (%get-user-data-dir)))

(define %get-sync-dir
  (get-api-funcation 'get-sync-dir '* '()))

(define (get-sync-dir)
  (pointer->string (%get-sync-dir)))

(define %get-user-id
  (get-api-funcation 'get-user-id '* '()))

(define (get-user-id)
  (pointer->string (%get-user-id)))

(define %get-user-data-sync-dir
  (get-api-funcation 'get-user-data-sync-dir void (list '* ffi:size_t)))

(define* (get-user-data-sync-dir
          #:optional
          (dir (bytestructure (bs:string 250 'utf8)))
          (buffer-size (bytestructure-size dir)))
  (%get-user-data-sync-dir (bytestructure->pointer dir) buffer-size)
  (bytestructure-ref dir))

;;; initialize an empty config object


;;; get raw input

(define %get-input
  (get-api-funcation
   'get-input
   '* (list ffi:uintptr_t)))

(define (get-input session-id)
  "get raw input string for @{}process-key inputs"
  (pointer->string (%get-input session-id)))

(define %get-caret-pot
  (get-api-funcation 'get-caret-pot ffi:size_t (list ffi:uintptr_t)))

(define (get-caret-pot session-id)
  (%get-caret-pot session-id))

(define %get-version
  (get-api-funcation 'get-version '* '()))

(define (get-version)
  (pointer->string (%get-version)))

(define %set-caret-pos
  (get-api-funcation 'set-caret-pos void (list ffi:uintptr_t ffi:size_t)))

(define (set-caret-pos session-id caret-pos)
  (%set-caret-pos session-id caret-pos))

(define %get-prebuilt-data-dir
  (get-api-funcation
   'get-prebuilt-data-dir
   '* '()))

(define (get-prebuilt-data-dir)
  (pointer->string (%get-prebuilt-data-dir)))

(define %get-staging-dir
  (get-api-funcation
   'get-staging-dir
   '* '()))

(define (get-staging-dir)
  (pointer->string (%get-staging-dir)))
