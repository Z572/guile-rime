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
 (gnu packages texinfo))

(define %srcdir
  (dirname (current-filename)))
(package
  (name "guile-rime")
  (version "0.1")
  (source (local-file "." "guile-rime-checkout"
                      #:recursive? #t
                      #:select? (git-predicate %srcdir)))
  (build-system gnu-build-system)
  (arguments `(#:configure-flags
               (list (string-append "--with-rime-data-dir="
                                    (assoc-ref %build-inputs "rime-data")
                                    "/share/rime-data"))
               #:make-flags '("GUILE_AUTO_COMPILE=0")))
  (native-inputs
   `(("autoconf" ,autoconf)
     ("automake" ,automake)
     ("pkg-config" ,pkg-config)
     ("texinfo" ,texinfo)))
  (inputs `(("guile" ,guile-3.0)
            ("librime" ,librime)
            ("rime-data" ,rime-data)))
  (propagated-inputs
   `(("guile-bytestructures" ,guile-bytestructures)
     ("guile-ncurses" ,guile-ncurses)))
  (synopsis "")
  (description "")
  (home-page "")
  (license license:gpl3+))

