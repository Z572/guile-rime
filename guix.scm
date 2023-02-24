(use-modules
 (guix packages)
 ((guix licenses) #:prefix license:)
 (guix download)
 (guix git-download)
 (guix gexp)
 (guix build-system gnu)
 (gnu packages)
 (gnu packages autotools)
 (gnu packages guile)
 (gnu packages guile-xyz)
 (gnu packages ibus)
 (gnu packages pkg-config)
 (gnu packages texinfo)
 (guix transformations))

(define %srcdir
  (dirname (current-filename)))
(define trans (options->transformation
               '((with-version . "librime=1.8.3"))))
(package
  (name "guile-rime")
  (version "0.1")
  (source (local-file "." "guile-rime-checkout"
                      #:recursive? #t
                      #:select? (git-predicate %srcdir)))
  (build-system gnu-build-system)
  (arguments (list #:configure-flags
                   #~(list (string-append "--with-rime-data-dir="
                                          #$(this-package-input "rime-data")
                                          "/share/rime-data"))
                   #:make-flags
                   #~(list "GUILE_AUTO_COMPILE=0")))
  (native-inputs
   (list autoconf
         automake
         pkg-config
         texinfo
         ;;; for demo
         guile-ncurses))
  (inputs (list guile-3.0
                (trans librime)
                (trans rime-data)))
  (propagated-inputs
   (list guile-bytestructures))
  (synopsis "")
  (description "")
  (home-page "")
  (license license:gpl3+))

