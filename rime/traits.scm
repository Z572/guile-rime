(define-module (rime traits)
  #:use-module (rime configuration)
  #:use-module (bytestructures guile)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-26)
  #:use-module (rime utils)
  #:export (make-traits
            traits->pointer
            traits-shared-data-dir
            traits-app-name
            traits-user-data-dir
            traits-log-dir
            traits-modules
            traits-setuped?
            set-traits-setuped?)
  #:export-syntax
  (check-traits?))

(define %traits
  (bs:struct
   `((data-size ,int)
     (shared-data-dir ,char*)
     (user-data-dir ,char*)
     (distribution-name ,char*)
     (distribution-code-name ,char*)
     (distribution-version ,char*)
     (app-name ,char*)
     (modules ,(bs:pointer char*))
     (min-log-level ,int)
     (log-dir ,char*)
     (prebuilt-data-dir ,char*)
     (staging-dir ,char*))))

(define-record-type <traits>
  (%make-traits bytestructure setuped?)
  traits?
  (bytestructure traits-bytestructure)
  (setuped? traits-setuped? set-traits-setuped?))

(define-check check-traits? traits?
  "This is not a <traits> record!")

(define* (make-traits #:key
                      (shared-data-dir %rime-shared-data-dir)
                      user-data-dir
                      log-dir
                      (app-name "rime.guile")
                      (distribution-name "Grime")
                      (distribution-code-name %guile-rime-package-name)
                      (distribution-version %guile-rime-version)
                      min-log-level
                      prebuilt-data-dir
                      staging-dir)
  (apply struct-init (cut %make-traits <> #f) %traits
         `((shared-data-dir ,(string->pointer-address shared-data-dir))
           ,@(if user-data-dir `((user-data-dir ,(string->pointer-address user-data-dir))) '())
           ,@(if log-dir `((log-dir ,(string->pointer-address log-dir))) '())
           ,@(if min-log-level `((min-log-level ,min-log-level)) '())
           ,@(if prebuilt-data-dir `((prebuilt-data-dir ,(string->pointer-address prebuilt-data-dir))) '())
           ,@(if staging-dir `((staging-dir ,(string->pointer-address staging-dir))) '())
           (app-name ,(string->pointer-address app-name))
           (distribution-name ,(string->pointer-address distribution-name))
           (distribution-code-name ,(string->pointer-address  distribution-code-name))
           (distribution-version ,(string->pointer-address  distribution-version)))))

(define (traits->pointer traits)
  (check-traits? traits)
  (bytestructure->pointer (traits-bytestructure traits)))

(define (traits-shared-data-dir traits)
  (check-traits? traits)
  (make-pointer->string
   (bytestructure-ref
    (traits-bytestructure traits)
    'shared-data-dir)))

(define (traits-app-name traits)
  (check-traits? traits)
  (make-pointer->string
   (bytestructure-ref
    (traits-bytestructure traits)
    'app-name)))

(define (traits-user-data-dir traits)
  (check-traits? traits)
  (make-pointer->string
   (bytestructure-ref
    (traits-bytestructure traits)
    'user-data-dir)))

(define (traits-log-dir traits)
  (check-traits? traits)
  (bytestructure-ref (traits-bytestructure traits) 'log-dir))

(define (traits-modules traits)
  (check-traits? traits)
  (bytestructure-ref (traits-bytestructure traits) 'modules))
