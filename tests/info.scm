(define-module (tests info)
  #:use-module (rime info)
  #:use-module (rime traits)
  #:use-module (rime structs)
  #:use-module (srfi srfi-64)
  #:use-module (tests helpers))

(define test-dir (mkdtemp "/tmp/test-info-XXXXXX"))
(define-values (traits log-dir shared-data-dir user-data-dir)
  (make-test-traits
   test-dir))


(setup traits)
(initialize traits)
(start-maintenance #f)
(join-maintenance-thread)
(test-group-with-cleanup "info"
  (test-equal "get-user-data-dir"
    user-data-dir
    (get-user-data-dir))
  (test-equal "get-shared-data-dir"
    shared-data-dir
    (get-shared-data-dir))
  (test-equal "get-prebuilt-data-dir"
    (string-append shared-data-dir "/build")
    (get-prebuilt-data-dir))

  (begin (finalize) (system* "rm" "-r" test-dir)))
