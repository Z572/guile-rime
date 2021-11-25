(define-module (rime levers-api)
  #:use-module (rime configuration)
  #:use-module (rime structs)
  #:use-module (bytestructures guile)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-26)
  #:use-module (rime utils)
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
                                           (uintptr_t . ffi:uintptr_t))))

(define levers-module (find-module "levers"))

(define-get-api-funcation get-api-funcation
  (module-get-api levers-module)
  (lambda (a) (pointer->bytestructure
               a;; (bytestructure->pointer (custom-api-bytestructure a))
               %levers-api)))

(define levers-initialize
  (module-initialize levers-module))

(define %custom-settings
  (bs:struct
   `((placeholder ,uintptr_t))))

(define-record-type <custom-settings>
  (%make-custom-settings bytestructure)
  custom-settings?
  (bytestructure custom-settings-bytestructure))

(define %switcher-settings
  (bs:struct
   `((placeholder ,char*))))

(define %schema-info
  (bs:struct
   `((placeholder ,char*))))

(define %user-dict-iterator
  (bs:struct
   `((ptr ,(bs:pointer void))
     (i ,size_t))))

(define %levers-api
  (bs:struct
   `((data-size ,int)
     (custom-settings-init ,(bs:pointer '*;; %custom-settings
                                        );; (bs:pointer '*)
                           )
     (custom-settings-destroy ,(bs:pointer void))
     (load-settings ,bool)
     (save-settings ,bool)
     (customize-bool ,bool)
     (customize-int ,bool)
     (customize-double ,bool)
     (customize-string ,bool)
     (is-first-run ,bool)
     (settings-is-modified ,bool)
     (settings-get-config ,bool)

     (switcher-settings-init ,(bs:pointer '*))
     (get-available-schema-list ,bool)
     (get-selected-schema-list ,bool)
     (schema-list-destroy ,(bs:pointer void))
     (get-schema-id ,char*)
     (get-schema-name ,char*)
     (get-schema-version ,char*)
     (get-schema-author ,char*)
     (get-schema-description ,char*)
     (get-schema-file-path ,char*)
     (select-schemas ,bool)
     (get-hotkeys ,char*)
     (set-hotkeys ,bool)
     (user-dict-iterator-init ,bool)
     (user-dict-iterator-destroy ,(bs:pointer void))
     (next-user-dict ,char*)
     (backup-user-dict ,bool)
     (restore-user-dict ,bool)
     (export-user-dict ,int)
     (import-user-dict ,int)
     (customize-item ,bool))))

(define %custom-settings-init
  (get-api-funcation 'custom-settings-init '* (list ffi:int ffi:int)))
(define (custom-settings-init config-id generator-id)
  (%make-custom-settings
   (%custom-settings-init (string->pointer-address config-id)
                          (string->pointer-address generator-id))))

(define %custom-settings-destroy
  (get-api-funcation 'custom-settings-destroy void '(*)))
(define (custom-settings-destroy settings)
  (%custom-settings-destroy (custom-settings-bytestructure settings)))

;; (define %load-settings (get-api-funcation 'load-settings ffi:int '(*)))
;; (define (load-settings settings)
;;   (c-int->bool (%load-settings (custom-settings-bytestructure settings))))

;;(define %save-settings (get-api-funcation 'save-settings ffi:int '(*)))
;; (define (save-settings settings)
;;   (c-int->bool (%save-settings (custom-settings-bytestructure settings))))

;; (define %is-first-run (get-api-funcation 'is-first-run ffi:int '(*)))
;; (define (is-first-run settings)
;;   (c-int->bool (%is-first-run (custom-settings-bytestructure settings))))

(define %switcher-settings-init (get-api-funcation 'switcher-settings-init '* '()))
(define (switcher-settings-init)
  (%switcher-settings-init))
;; (define %get-schema-file-path (get-api-funcation 'get-schema-file-path ffi:int '(*)))
;; (define (get-schema-file-path)
;;   (%get-schema-file-path))
