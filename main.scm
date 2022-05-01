#!/usr/bin/env -S guix shell -D --file=guix.scm -- guile --no-auto-compile
!#

(add-to-load-path (dirname (current-filename)))
(use-modules ;; (rime main)
 (ncurses curses)
 (ncurses menu)
 (rime)
 (rime utils)
 (ice-9 match)
 (srfi srfi-1)
 (srfi srfi-26)
 (srfi srfi-34))

(define ~ (cut format #f <...>))
(define-syntax-rule (push! elt v) (set! v (cons elt v)))
(define get-preedit
  (compose composition-preedit
           context-composition
           get-context))


(define (move-end session-id)
  (set-caret-pos
   session-id
   (string-length
    (get-preedit session-id))))

(define (move-beginning session-id)
  (set-caret-pos session-id 0))

(define (move-right session-id)
  (set-caret-pos session-id
                 (min (1+ (get-caret-pot session-id))
                      (string-length
                       (get-preedit session-id)))))

(define (move-left session-id)
  (set-caret-pos session-id
                 (max 0 (- (get-caret-pot session-id) 1))))

(define (build-composition-predit-with-cursor composition)
  (let* ((preedit (composition-preedit composition))
         (leng (composition-length composition))
         (cursor-post (composition-cursor-post composition))
         (cursor (max (- (string-length preedit)
                         (- leng cursor-post))
                      0)))
    (string-append
     (string-take
      preedit
      cursor)
     "|"
     (string-drop
      preedit
      cursor))))
;; (define ncurses-to-x11-alist
;;   `(;; (265 . #xffbe)
;;     ;; #xffc9
;;     ,@(map (lambda (a b)))))
(define* (r-process-key #:optional (session-id (create-session)) keycode (mask 0))
  (push! keycode notifications)
  (match keycode
    (#\dc1
     (clear stdscr)
     (refresh stdscr)
     (endwin)
     (finalize)
     (exit 0))
    (#\esc
     (process-key session-id 27;; #xff1b
                  mask))
    ((and (? char?) (? (cut char-set-contains? char-set:iso-control <>) ))
     (let ((i (char->integer keycode)))
       (process-key session-id (if (<= i 26)
                                   (+ 96 i)
                                   i)
                    (if (<= i 31)

                        (+ 4 mask)
                        mask))))

    ((? (cut eqv? <> (key-f 4)))
     (process-key session-id #xffc1 mask))
    ((? (cut member <> (list KEY_ENTER #\cr #\nl)))
     (process-key session-id #xff0d mask))
    ((? (cut eqv? KEY_LEFT <>))
     (process-key session-id #xff51 mask))
    ((? (cut eqv? KEY_NPAGE <>))
     (process-key session-id #xff9b mask))
    ((? (cut eqv? KEY_RIGHT <>))
     (process-key session-id #xff53 mask))
    ((? (cut eqv? KEY_UP <>))
     (process-key session-id #xff52 mask))
    ((? (cut eqv? KEY_BACKSPACE <>))
     (process-key session-id #xff08 mask))
    ((and (? number?) (? (lambda (a) (and (<= 200 a) (>= 237)))))
     (process-key session-id keycode mask))
    (_ (process-key session-id (if (number? keycode)
                                   keycode
                                   (char->integer keycode))
                    mask))))

(define (draw-candidates win candidates highlighted-index)
  (let ((v 0))
    (map (lambda (a)
           (when (= v highlighted-index)
             (attr-on! win (color-pair 1)))
           (set! v (1+ v))
           (addstr win (string-append (number->string v) ". "
                                      (candidate-text a)
                                      (let ((comment (candidate-comment a)))
                                        (if (string-null? comment)
                                            ""
                                            (string-append "(" comment ")")))
                                      " "))
           (when (= v (1+ highlighted-index))
             (attr-off! win (color-pair 1))))
         candidates)))

(define* (draw-info session-id #:optional win)
  (let* ((s (get-status session-id))
         (commit (get-commit session-id))
         (context (get-context session-id))
         (comp (context-composition context))
         (menu (context-menu context)))

    (clear win)
    (box win (acs-vline) (acs-hline))
    (move win 1 1)
    (addstr win (~ " is-composing: ~S"   (status-is-composing s)))
    (addstr win (~ " is-ascii-mode: ~S"  (status-is-ascii-mode s)))
    (addstr win (~ " is-ascii-punct: ~S" (status-is-ascii-punct s)))
    (addstr win (~ " is-full-shape: ~S"  (status-is-full-shape s)))
    (addstr win (~ " is-simplified: ~S"  (status-is-simplified s)))
    (addstr win (~ " is-disabled: ~S"    (status-is-disabled s)))
    (addstr win (~ " is-traditional: ~S" (status-is-traditional s)))
    (move win 2 1)
    (addstr win (~ " commit: ~S"       (commit-text commit)))
    (addstr win " candidates: ")
    (draw-candidates win (menu-candidates menu)(menu-highlighted-candidate-index menu))
    (move win 3 1)
    (addstr win (~ " get-current-schema: ~S" (get-current-schema session-id)))
    (addstr win (~ " schema-name: ~S" (status-schema-name s)))

    (addstr win (~ " page-size: ~S" (menu-page-size menu)))
    (addstr win (~ " page-no: ~S" (menu-page-no menu)))
    (addstr win (~ " is-last-page: ~S" (menu-is-last-page menu)))
    (addstr win (~ " menu-highlighted-candidate-index: ~S" (menu-highlighted-candidate-index menu)))
    (addstr win (~ " num-candidates: ~S" (menu-num-candidates menu)))
    (addstr win (~ " select-keys: ~S" (menu-select-keys menu)))
    (move win 4 1)
    (addstr win (~ " text-preview: ~S" (context-commit-text-preview context)))
    (addstr win (~ " context-select-labels: ~S" (context-select-labels context)))
    (move win 5 1)
    (addstr win (~ " preedit: \"~a\"" (composition-preedit comp)))
    (addstr win (~ " length: ~S" (composition-length comp)))
    (addstr win (~ " raw-input: ~a" (get-input session-id)))
    (addstr win (~ " raw-input-length: ~S" (string-length (get-input session-id))))
    (move win 6 1)
    (addstr win (~ " cursor-pos: ~S" (composition-cursor-post comp)))
    (addstr win (~ " sel-start: ~S" (composition-sel-start comp)) )
    (addstr win (~ " sel-end: ~S" (composition-sel-end comp)))
    (free-context context)
    (free-status s)

    (free-commit commit)
    (noutrefresh win)))

(define (draw-util session-id win)
  (clear win)
  (box win (acs-vline) (acs-hline))
  (move win 1 1)
  (addstr win (~ " shared-data-dir: ~S" (get-shared-data-dir)))
  (addstr win (~ " user-data-dir: ~S\n" (get-user-data-dir)))
  (addstr win (~ " sync-dir: ~S" (get-sync-dir)))
  (addstr win (~ " staging-dir: ~S" (get-staging-dir)))
  (addstr win (~ " user-id: ~S" (get-user-id)))
  (addstr win (~ " user-data-sync-dir: ~S" (get-user-data-sync-dir)))
  (addstr win (~ " prebuilt-data-dir: ~S\n" (get-prebuilt-data-dir)))


  (noutrefresh win))

(define (draw-notifications win)
  (clear win)
  (move win 1 1)
  (for-each (lambda (v)
              (addstr win (~ " ~a " (getcury win) ))
              (addch win (acs-vline))
              (addstr win (~ "~S" v))

              (move win (1+ (getcury win)) 1))
            notifications)
  (box win (acs-vline) (acs-hline))
  (noutrefresh win))

(define (draw-main session-id win)
  (let* ((context (get-context session-id))
         (comp (context-composition context))
         (menu (context-menu context)))
    (clear win)
    (move win 1 1)
    (box win (acs-vline) (acs-hline))
    (addch win (acs-vline))
    (draw-candidates
     win
     (menu-candidates menu)
     (menu-highlighted-candidate-index menu))
    (unless (null? (menu-candidates menu))
      (addstr win (string-append
                   " "
                   (if (= 0 (menu-page-no menu)) " " "<")
                   (number->string (1+ (menu-page-no menu)))
                   (if (menu-is-last-page menu) " " ">"))))
    (move win 2 2)
    (addstr win (build-composition-predit-with-cursor comp))
    ;; (addstr win (composition-preedit comp))
    (move win 3 1)
    (addstr win (string-append " " (context-commit-text-preview context)))
    (free-context context)
    (noutrefresh win)))


(define notifications '())
(define (start-rime)
  (let ((traits (make-traits
                 #:user-data-dir
                 (or (and=>
                      (getenv "XDG_CONFIG_HOME")
                      (cut string-append <> "/grime/"))
                     (string-append (getenv "HOME") "/.config/grime/"))
                 #:min-log-level 0)))
    (display "set-notification-handler..." )
    (when #f
      (set-notification-handler (lambda v
                                  (push! v notifications))))
    (display " done\n")
    (display "setup..." )(setup traits) (display " done\n")
    (display "initialize...")(initialize traits) (display " done\n")

    (display "deployer-initialize...") (deployer-initialize traits) (display " done\n"))
  (when (start-maintenance #f)
    ;;(deploy-config-file "guile_rime.yaml" "config_version")
    (display "join-maintenance-thread...") (join-maintenance-thread) (display " done\n")))

(start-rime)
(define stdscr (initscr))
(raw!)
(start-color!)
(init-pair! 1 COLOR_BLACK COLOR_WHITE)
(nonl!)
(intrflush! #f)
(curs-set 0)
(keypad! stdscr #t)
(noecho!)

(define ww (newwin 5 (- (cols) 2) 1 1))
(define winn (newwin 10 (- (cols) 2) 6 1))
(define utils-win (newwin 9 (- (cols) 4) (- (lines) 9) 1))
(define log-win (newwin (- (lines) 29) 120 20 1))

(define session-id (create-session))




(define (choose-schema item)
  (let ((id (schema-list-item-id item))
        (conf (user-config-open "user")))
    (select-schema session-id id)
    (config-set-string conf "var/previously_selected_schema" id)
    (set-option session-id "simplification" #t)
    (config-close conf)))

(let* ((l (schema-list-list (get-schema-list)))
       (my-menu (new-menu (let ((b 0))
                            (map (lambda (a)
                                   (set! b (1+ b))
                                   (new-item (string-append
                                              (number->string b) ". "
                                              (schema-list-item-name a))
                                             (schema-list-item-id a))) l))))
       (my-menu-win (newwin (round (/ (lines) 2)) (round (/ (cols) 2))
                            (round (/ (lines) 4)) (round (/ (cols) 4))))
       (my-menu-subwin (derwin my-menu-win (length l)
                               50
                               (round (/ (- (getmaxy my-menu-win) (length l)) 2))
                               (round (/ (- (getmaxx my-menu-win) 50) 2))
                               ;; (getmaxx my-menu-win)
                               ;; (map (lambda (i) (round (/ i 2)))
                               ;;      )

                               ;; (round (/ (lines) 8)) (list (round (/ (cols) 8)))
                               )))

  (keypad! my-menu-win #t)
  (addstr my-menu-win (~ "~S" (get-current-schema session-id)))
  (set-menu-mark! my-menu "=> ")
  (set-menu-win! my-menu my-menu-win)
  (set-menu-sub! my-menu  my-menu-subwin)
  (box my-menu-win (acs-vline) (acs-hline))
  (box my-menu-subwin (acs-vline) (acs-hline))
  (post-menu my-menu)
  (refresh my-menu-win)
  (let loop ((c (getch my-menu-win)))
    (cond

     ;; Move down the menu when down arrow is pressed and then loop.
     ((member c (list KEY_DOWN #\so))
      (begin
        (menu-driver my-menu REQ_DOWN_ITEM)
        (loop (getch my-menu-win))))

     ;; Move up the menu when the up arrow is pressed and then loop.
     ((member c (list KEY_UP #\dle))
      (begin
        (menu-driver my-menu REQ_UP_ITEM)
        (loop (getch my-menu-win))))

     ;; When enter is pressed, return the selection and quit.
     ((member c (list KEY_ENTER #\cr #\nl))
      (begin
        (unpost-menu my-menu)
        (refresh my-menu-win)
        (choose-schema (list-ref l (item-index (current-item my-menu))))))
     ((false-if-exception (char-numeric? c))
      (choose-schema (list-ref l (- (string->number (~ "~a" c)) 1)))
      (unpost-menu my-menu)
      (refresh my-menu-win))
     ;; If 'Q' , 'q' or 'C-q' is pressed, quit.  Otherwise, loop.
     ((member c (list #\Q #\q #\dc1))
      (begin (clear stdscr)
             (refresh stdscr)
             (endwin)
             (finalize)
             (exit 0)))
     (else
      (loop (getch my-menu-win))))))


;; (box winn (acs-vline) (acs-hline))
;; (box ww (acs-vline) (acs-hline))
;; (box utils-win (acs-vline) (acs-hline))
;; (box log-win (acs-vline) (acs-hline))
;; (noutrefresh winn)
;; (noutrefresh ww)
;; (noutrefresh utils-win)
;; (noutrefresh log-win)
;; (doupdate)
;; (scrollok! ww #t)
(clearok! ww #t)
(clearok! winn #f)
(clearok! log-win #t)
(clearok! utils-win #t)

                                        ;(sleep 2)
(while #t
  (let* ((ch (getch stdscr))
         (is-esc? (eqv? ch #\esc)))
    ;; (when (eqv? ch #\dc1)
    ;;   (clear stdscr)
    ;;   (refresh stdscr)
    ;;   (endwin)
    ;;   (finalize)
    ;;   (exit 0))
    (guard (c ((check-error? c)
               (push! (format #f "~a:~a:~a: ~a: source is `~S'. Value is `~S'.  ~a"
                              (check-error-file-name c)
                              (check-error-line c)
                              (check-error-column c)
                              (check-error-checker c)
                              (check-error-source c)
                              (check-error-value c)
                              (check-error-message c))
                      notifications)
               ;; (raise c)
               ))
      (draw-notifications log-win)
      (r-process-key session-id
                     (if is-esc? (getch stdscr) ch)
                     (if is-esc? 8 0))
      (draw-main session-id ww)

      (draw-info session-id winn)
      (draw-util session-id utils-win))
    (doupdate)))
