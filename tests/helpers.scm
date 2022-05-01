(define-module (tests helpers)
  #:use-module (rime traits)
  #:export (make-test-traits
            values->list
            make-test-dir))

(define* (make-test-traits base-dir #:optional dry-run?
                           #:key
                           (log-dir (string-append base-dir "/log") )
                           (shared-data-dir (string-append base-dir "/shared-data"))
                           (user-data-dir (string-append base-dir "/user-dir")))
  (unless dry-run?
    (mkdir log-dir)
    (mkdir shared-data-dir))
  (values (make-traits
           #:log-dir log-dir
           #:shared-data-dir shared-data-dir
           #:user-data-dir user-data-dir) log-dir shared-data-dir user-data-dir))

(define (make-test-dir name) (mkdtemp (string-append "/tmp/test-" name "-XXXXXX")))

(define-syntax-rule (values->list sexp)
  (call-with-values (lambda () sexp)
    (lambda a a)))

(define (call-with-directory directory thunk)
  (dynamic-wind
    (lambda ()
      (when (access? directory F_OK)
        (rmtree directory))
      (path-mkdir directory #true))
    thunk
    (lambda ()
      (rmtree directory))))
