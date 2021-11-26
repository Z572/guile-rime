(define-module (rime check)
  #:use-module (rime configuration)
  #:use-module (srfi srfi-34)
  #:use-module (srfi srfi-35)
  #:export (&check-error
            check-error?
            check-error-checker
            check-error-value
            check-error-source
            check-error-file-name
            check-error-message
            check-error-line
            check-error-column
            call-with-check
            call-with-not-check)
  #:export-syntax (with-check
                   with-not-check
                   define-check))

(define-condition-type &check-error &message
  check-error?
  (checker check-error-checker)
  (value check-error-value)
  (source check-error-source)
  (filename check-error-file-name)
  (line check-error-line)
  (column check-error-column))

(define check-error-message condition-message)

(define-syntax define-check
  (lambda (x)
    (syntax-case x ()
      ((_ name proc message*)
       #'(define-syntax name
           (lambda (y)
             (syntax-case y ()
               ((_ obj)
                (with-syntax ((obje #`'#,(datum->syntax
                                          y (syntax-source #'obj))))
                  #`(if (%guile-rime-debug-mode?)
                        (let* ((location obje)
                               (filename (assoc-ref location 'filename))
                               (line (assoc-ref location 'line))
                               (column (assoc-ref location 'column)))
                          (or (and (proc obj) obj)
                              (raise (condition
                                      (&check-error
                                       (checker (quote name))
                                       (value obj)
                                       (source (quote obj))
                                       (filename filename)
                                       (line (1+ line))
                                       (column column)
                                       (message message*))))))
                        obj))))))))))
(define-syntax-rule (with-check body ...)
  (parameterize ((%guile-rime-debug-mode? #t))
    body ...))

(define (call-with-check proc)
  (with-check (proc)))

(define-syntax-rule (with-not-check body ...)
  (parameterize ((%guile-rime-debug-mode? #f))
    body ...))

(define (call-with-not-check proc)
  (with-not-check (proc)))
