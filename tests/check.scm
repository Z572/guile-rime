(define-module (tests check)
  #:use-module (rime check)
  #:use-module (srfi srfi-64)
  #:use-module (srfi srfi-34)
  #:use-module (srfi srfi-35))

;;(define-check check-string string? "")
(test-group "check"
  (with-check
   (test-assert "define-check"
     (begin (test-read-eval-string
             "(define-check check-string string? \"test\")")
            (module-ref (current-module) 'check-string)))
   (test-error &check-error (begin (test-read-eval-string
                                    "(define-check c 1 \"\")")
                                   (test-read-eval-string
                                    "(c 1)")))
   (test-error "check-error"
               &check-error
               (check-string #f)))

  (with-not-check
   (define-check check-list list? "")
   (test-equal "define-check-out"
     "" (check-list ""))
   (test-equal "define-check-error"
     1 (check-list 1))))
