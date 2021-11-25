(define-module (rime))

(eval-when (eval load compile)
  (begin
    (define %public-modules
      '((rime structs)
        (rime traits)
        (rime api)))

    (let* ((current-module (current-module))
           (current-module-interface (resolve-interface (module-name current-module))))
      (for-each
       (lambda (submodule)
         (let ((submodule-interface (resolve-interface submodule)))
           (module-use! current-module submodule-interface)
           (module-use! current-module-interface submodule-interface)))
       %public-modules))))
