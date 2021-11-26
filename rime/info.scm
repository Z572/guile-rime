(define-module (rime info)
  #:use-module (rime api)
  #:use-module (rime utils)
  #:use-module (bytestructures guile)
  #:use-module ((system foreign) #:select (pointer->string
                                           string->pointer
                                           void
                                           (int . ffi:int)
                                           (size_t . ffi:size_t)
                                           (uintptr_t . ffi:uintptr_t)))
  #:export (get-shared-data-dir
            get-user-data-dir
            get-sync-dir
            get-user-id
            get-user-data-sync-dir
            get-version
            get-prebuilt-data-dir
            get-staging-dir))

(define get-api-funcation %guile-rime-get-api-funcation)
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
          (dir (make-string 250))
          (buffer-size (string-length dir)))
  (let ((p (string->pointer dir)))
    (%get-user-data-sync-dir p buffer-size)
    (pointer->string p)))

(define %get-version
  (get-api-funcation 'get-version '* '()))

(define (get-version)
  (pointer->string (%get-version)))

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
