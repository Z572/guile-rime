(define-module (rime session)
  #:use-module (rime api)
  #:use-module (rime utils)
  #:use-module ((system foreign) #:select (void
                                           (int . ffi:int)
                                           (uintptr_t . ffi:uintptr_t)))
  #:export (create-session
            find-session
            destroy-session
            cleanup-stale-sessions
            cleanup-all-sessions))

(define get-api-funcation %guile-rime-get-api-funcation)
(define %create-session
  (get-api-funcation
   'create-session ffi:uintptr_t '()))
(define (create-session)
  (%create-session))

(define %find-session
  (get-api-funcation
   'find-session
   ffi:int
   (list ffi:uintptr_t)))

(define (find-session session-id)
  (c-int->bool (%find-session session-id)))

(define %destroy-session
  (get-api-funcation
   'destroy-session
   ffi:int
   (list ffi:uintptr_t)))

(define (destroy-session session-id)
  (c-int->bool (%destroy-session session-id)))

(define %cleanup-stale-sessions
  (get-api-funcation 'cleanup-stale-sessions void '()))

(define (cleanup-stale-sessions)
  (%cleanup-stale-sessions))

(define %cleanup-all-sessions
  (get-api-funcation 'cleanup-all-sessions void '()))
(define (cleanup-all-sessions)
  (%cleanup-all-sessions))
