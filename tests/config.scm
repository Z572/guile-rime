(define-module (tests config)
  #:use-module (rime info)
  #:use-module (rime config)
  #:use-module (rime configuration)
  #:use-module (rime traits)
  #:use-module (rime structs)
  #:use-module (srfi srfi-64)
  #:use-module (tests helpers))

(define base-dir (dirname (current-filename)))
(define test-dir (make-test-dir "config"))

(define-values (traits log-dir shared-data-dir user-data-dir)
  (make-test-traits
   test-dir))

(setup traits)
(initialize traits)
(start-maintenance #f)
(join-maintenance-thread)
(%guile-rime-debug-mode? #t)

(test-group-with-cleanup "config"
  (let ((config (config-open (string-append base-dir "/data/user")))
        (installation (user-config-open "installation"))
        (config-2 (config-open (string-append base-dir "/data/config"))))
    (test-assert "user-config-open-1"
      (user-config-open "installation"))
    ;; (test-assert "config-update-signature"
    ;;   ())
    (test-equal "config-get-bool-1"
      (list #t #t) (values->list (config-get-bool config "config/get-bool-1")))
    (test-equal "config-get-bool-2"
      (list #f #t) (values->list (config-get-bool config "config/get-bool-2")))
    (test-equal "config-get-bool-not-found"
      (list #f #f) (values->list (config-get-bool config "config/get-bool-not-found")))
    (test-equal "config-get-int"
      20000 (config-get-int config "config/get-int"))
    (test-equal "config-get-int-not-found"
      #f (config-get-int config "config/get-int-not-found"))
    (test-equal "config-get-double"
      1.1 (config-get-double config "config/get-double"))
    (test-equal "config-get-double-not-found"
      #f (config-get-double config "config/get-double-not-found"))
    (test-equal "config-get-string"
      "PASS1" (config-get-string config "config/get-string"))
    (test-equal "config-get-cstring"
      "PASS2" (config-get-cstring config "config/get-cstring"))
    (test-equal "config-get-cstring-not-found"
      #f (config-get-cstring config "config/get-cstring-not-found"))
    ;; (test-equal "config-get-item"
    ;;   20 (config-get-item config "config"))
    (test-equal "config-list-size-1"
      2 (config-list-size config "config/v"))
    (test-equal "config-list-size-not-found"
      0 (config-list-size config "config/config-list-size-not-found"))
    (test-equal "config-set-bool-1"
      (list #t #f) (let ((key "config/set-bool-1"))
                     (list (begin (config-set-bool config key #t)
                                  (config-get-bool config key))
                           (begin (config-set-bool config key #f)
                                  (config-get-bool config key)))))
    (test-equal "config-set-bool-2"
      (list #f #t) (let ((key "config/set-bool-1"))
                     (list (begin (config-set-bool config key #f)
                                  (config-get-bool config key))
                           (begin (config-set-bool config key #t)
                                  (config-get-bool config key)))))
    (test-equal "config-set-double"
      (list 1.23 23.1)
      (let ((key "config/set-double"))
        (list (begin (config-set-double config key 1.23)
                     (config-get-double config key))
              (begin (config-set-double config key 23.1)
                     (config-get-double config key)))))
    (test-equal "config-set-int"
      (list 21 29)
      (let ((key "config/set-int"))
        (list (begin (config-set-int config key 21)
                     (config-get-int config key))
              (begin (config-set-int config key 29)
                     (config-get-int config key)))))
    (test-equal "config-set-string"
      (list "666" "fff")
      (let ((key "config/set-string"))
        (list (begin (config-set-string config key "666")
                     (config-get-string config key))
              (begin (config-set-string config key "fff")
                     (config-get-string config key)))))
    (test-equal "config-list-size-2"
      2 (config-list-size config-2 "config"))

    (begin
      (config-close config)
      (config-close config-2)
      (config-close installation)
      (finalize)
      (system* "rm" "-r" test-dir))))
