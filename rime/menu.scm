(define-module (rime menu)
  #:use-module (rime api)
  #:use-module (rime utils)
  #:use-module (rime candidate)
  #:use-module (bytestructures guile)
  #:use-module (srfi srfi-9)
  #:use-module ((system foreign) #:select (make-pointer))
  #:export (%menu
            menu?
            %make-menu
            menu-page-size
            menu-page-no
            menu-is-last-page
            menu-highlighted-candidate-index
            menu-num-candidates
            menu-candidates
            menu-select-keys
            check-menu?))

(define %menu
  (bs:struct
   `((page-size ,int)
     (page-no ,int)
     (is-last-page ,bool)
     (highlighted-candidate-index ,int)
     (num-candidates ,int)
     (candidates ,(bs:pointer %candidate))
     (select-keys ,char*))))

(define-record-type <menu>
  (%make-menu bytestructure)
  menu?
  (bytestructure menu-bytestructure))

(define-check check-menu?
  menu? "This is not a <menu> record!")

(define (make-menu-bytestructure)
  (%make-menu (bytestructure %menu)))

(define (menu->pointer menu)
  (check-menu? menu)
  (bytestructure->pointer (menu-bytestructure menu)))

(define (menu-page-size menu)
  (check-menu? menu)
  (bytestructure-ref (menu-bytestructure menu) 'page-size))

(define (menu-page-no menu)
  (check-menu? menu)
  (bytestructure-ref (menu-bytestructure menu) 'page-no))

(define (menu-is-last-page menu)
  (check-menu? menu)
  (c-int->bool (bytestructure-ref (menu-bytestructure menu) 'is-last-page)))

(define (menu-highlighted-candidate-index menu)
  (check-menu? menu)
  (bytestructure-ref (menu-bytestructure menu) 'highlighted-candidate-index))

(define (menu-num-candidates menu)
  (check-menu? menu)
  (bytestructure-ref (menu-bytestructure menu) 'num-candidates))

(define (menu-candidates menu)
  (check-menu? menu)
  (if (zero? (menu-num-candidates menu))
      '()
      (let ((candidates* (pointer->bytestructure
                          (make-pointer
                           (bytestructure-ref (menu-bytestructure menu) 'candidates))
                          (bs:vector (menu-num-candidates menu) %candidate))))
        (let loop ((l '())
                   (num (- (menu-num-candidates menu) 1)))
          (if (< num 0)
              l
              (loop (cons (%make-candidate
                           (bytestructure-ref
                            candidates*
                            num)) l)
                    (- num 1)))))))

(define (menu-select-keys menu)
  (check-menu? menu)
  (bytestructure-ref (menu-bytestructure menu) 'select-keys))
