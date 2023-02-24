(define-module (rime api)
  #:use-module (rime configuration)
  #:use-module (rime utils)
  #:use-module (bytestructures guile)
  #:use-module (rnrs bytevectors)
  #:use-module ((system foreign) #:select (pointer->procedure))
  #:use-module (srfi srfi-9)
  #:export (get-api
            %guile-rime-get-api-funcation))

(define (librime->procedure return name params)
  (catch #t
    (lambda ()
      (let ((ptr (dynamic-func name (dynamic-link %librime))))
        (pointer->procedure return ptr params)))
    (lambda args
      (lambda _
        (throw 'system-error name  "~A" (list (strerror ENOSYS))
               (list ENOSYS))))))

(define %api
  (bs:struct
   `((data-size ,int)
     (setup ,(bs:pointer '*))
     (set-notification-handler ,(bs:pointer '*))
     (initialize ,(bs:pointer '*))
     (finalize ,(bs:pointer '*))
     (start-maintenance ,(bs:pointer '*))
     (is-maintenance-mode ,(bs:pointer '*))
     (join-maintenance-thread ,(bs:pointer '*))
     (deployer-initialize ,(bs:pointer '*))
     (prebuild ,(bs:pointer '*))
     (deploy ,(bs:pointer '*))
     (deploy-schema ,(bs:pointer '*))
     (deploy-config-file ,(bs:pointer '*))
     (sync-user-data ,(bs:pointer '*))
     (create-session ,session-id)
     (find-session ,(bs:pointer '*))
     (destroy-session ,(bs:pointer '*))
     (cleanup-stale-sessions ,(bs:pointer '*))
     (cleanup-all-sessions ,(bs:pointer '*))
     (process-key ,(bs:pointer '*))
     (commit-composition ,(bs:pointer '*))
     (clear-composition ,(bs:pointer '*))
     (get-commit ,(bs:pointer '*))
     (free-commit ,(bs:pointer '*))
     (get-context ,(bs:pointer '*))
     (free-context ,(bs:pointer '*))
     (get-status ,(bs:pointer '*))
     (free-status ,(bs:pointer '*))
     (set-option ,(bs:pointer '*))
     (get-option ,(bs:pointer '*))
     (set-property ,(bs:pointer '*))
     (get-property ,(bs:pointer '*))
     (get-schema-list ,(bs:pointer '*))
     (free-schema-list ,(bs:pointer '*))
     (get-current-schema ,(bs:pointer '*))
     (select-schema ,(bs:pointer '*))
     (schema-open ,(bs:pointer '*))
     (config-open ,(bs:pointer '*))
     (config-close ,(bs:pointer '*))
     (config-get-bool ,(bs:pointer '*))
     (config-get-int ,(bs:pointer '*))
     (config-get-double ,(bs:pointer '*))
     (config-get-string ,(bs:pointer '*))
     (config-get-cstring ,(bs:pointer '*))
     (config-update-signature ,(bs:pointer '*))
     (config-begin-map ,(bs:pointer '*))
     (config-next ,(bs:pointer '*))
     (config-end ,(bs:pointer '*))
     (simulate-key-sequence ,(bs:pointer '*))
     (register-module ,(bs:pointer '*))
     (find-module ,(bs:pointer '*))
     (run-task ,(bs:pointer '*))

     (get-shared-data-dir ,(bs:pointer '*))
     (get-user-data-dir ,(bs:pointer '*))
     (get-sync-dir ,(bs:pointer '*))
     (get-user-id ,(bs:pointer '*))
     (get-user-data-sync-dir ,(bs:pointer '*))
     (config-init ,(bs:pointer '*))
     (config-load-string ,(bs:pointer '*))
     (config-set-bool ,(bs:pointer '*))
     (config-set-int ,(bs:pointer '*))
     (config-set-double ,(bs:pointer '*))
     (config-set-string ,(bs:pointer '*))
     (config-get-item ,(bs:pointer '*))
     (config-set-item ,(bs:pointer '*))
     (config-clear ,(bs:pointer '*))
     (config-create-list ,(bs:pointer '*))
     (config-create-map ,(bs:pointer '*))
     (config-list-size ,size_t)
     (config-begin-list ,(bs:pointer '*))
     (get-input ,(bs:pointer '*))
     (get-caret-pot ,size_t)
     (select-candidate ,(bs:pointer '*))
     (get-version ,(bs:pointer '*))
     (set-caret-pos ,(bs:pointer '*))
     (select-candidate-on-current-page ,(bs:pointer '*))
     (candidate-list-begin ,(bs:pointer '*))
     (candidate-list-next ,(bs:pointer '*))
     (candidate-list-end ,(bs:pointer '*))
     (user-config-open ,(bs:pointer '*))
     (candidate-list-from-index ,(bs:pointer '*))
     (get-prebuilt-data-dir ,(bs:pointer '*))
     (get-staging-dir ,(bs:pointer '*))
     (commit-proto ,(bs:pointer '*))
     (context-proto ,(bs:pointer '*))
     (status-proto ,(bs:pointer '*))
;;; >= 1.8.0
     ,@(if %guile-rime-1.8?
           `((get-state-label ,(bs:pointer '*))
             (delete-candidate ,(bs:pointer '*))
             (delete-candidate-on-current-page ,(bs:pointer '*)))
           '()))))

(define-record-type <api>
  (%make-api bytestructure)
  api?
  (bytestructure api-bytestructure))

(define (pointer->api pointer)
  (%make-api (pointer->bytestructure pointer %api)))

(define (get-api)
  (pointer->api ((librime->procedure '* "rime_get_api" '()))))
(define-get-api-funcation %guile-rime-get-api-funcation
  (get-api) api-bytestructure)
