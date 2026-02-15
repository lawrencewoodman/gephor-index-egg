;;; Index file definitions to process NEX style index files
;;;
;;; Definitions are exported in gephor-index.scm
;;; From this file the following are exported:
;;;   process-index
;;;
;;; Copyright (C) 2025 Lawrence Woodman <https://lawrencewoodman.github.io/>
;;;
;;; Licensed under an MIT licence.  Please see LICENCE.md for details.
;;;

(define-type menu-item (list string string string string fixnum))

;; Exported Definitions ------------------------------------------------------


;; Process a NEX style index file.  This describes a menu and returns a list
;; of menu items.
;; TODO: rename?
(: process-index (string string string --> (or (list-of menu-item) false)))
(define (process-index root-dir request-selector nex-index)
  (define (parse-line-add-to-result result line-num line)
    (if (and (null? result) (string-every char-set:whitespace line))
        '()  ; Remove first blank lines
        (and-let* ((item (parse-line line-num root-dir request-selector line)))
          (cons item result) ) ) )

  (let* ((lines (string-split (string-trim-right nex-index char-set:whitespace)
                              "\n"
                              #t))
         (parsed-lines
           (let loop ((lines lines) (line-num 1) (result '()))
             (if (null? lines)
                 result
                 (and-let* ((result (parse-line-add-to-result result
                                                              line-num
                                                              (car lines))))
                   (loop (cdr lines) (add1 line-num) result))))))
    (and parsed-lines
         (reverse parsed-lines) ) ) )


;; Internal Definitions ------------------------------------------------------

;; Regular expression to split a => style link
(define index-link-split-regex (string->irregex "^=>[ ]+([^ ]+)[ ]*(.*)$"))

;; Regular expression to identify a URL in a => style link
(define url-regex (string->irregex "^.+?:\/\/[^:/]+(:[0-9]*)?.*$"))

(define (is-dir? path)
  (substring-index "/" path (sub1 (string-length path))))

(define (is-url? path)
  (let ((url-match (irregex-match url-regex path)))
    (irregex-match-data? url-match)))


(define (fail/log-error-invalid-url line-num username url)
  (apply log-error
         "problem processing index: invalid URL"
         (cons 'line line-num)
         (cons 'username username)
         (cons 'url url)
         (log-context))
  #f)


(define (fail/log-error-directory-not-file line-num username path local-path)
  (apply log-error
         "problem processing index: path is a directory but link doesn't have a trailing '/'"
         (cons 'line line-num)
         (cons 'username username)
         (cons 'path path)
         (cons 'local-path local-path)
         (log-context))
  #f)


(define (fail/log-error-file-nonexistent line-num username path local-path)
  (apply log-error
         "problem processing index: path doesn't exist or unknown type"
         (cons 'line line-num)
         (cons 'username username)
         (cons 'path path)
         (cons 'local-path local-path)
         (log-context))
  #f)


(define (fail/log-error-path-not-safe line-num username path local-path)
  (apply log-error
         "problem processing index: path isn't safe"
         (cons 'line line-num)
         (cons 'username username)
         (cons 'path path)
         (cons 'local-path local-path)
         (log-context))
  #f)


;; Return a menu item from a URL
;; TODO: Test the error from this
(define (url-item line-num path username)
  (or (menu-item-url username path)
      (fail/log-error-invalid-url line-num username path) ) )


;; Return a file menu item
;; If the path on the => line is a directory and doesn't have a training /, it will log an error and return #f
;; If the path on the => line doesn't exist or is an unknown type it will log an error and return #f
;; If the path on the => line isn't safe it will log an error and return #f
(: file-item (integer string string string string --> (or menu-item false)))
(define (file-item line-num root-dir request-selector path username)
  (define (make-item full-path item-selector)
    (if (safe-path? root-dir full-path)
        (if (directory? full-path)
            (fail/log-error-directory-not-file line-num username path full-path)
            (let ((item (menu-item-file full-path username item-selector)))
              (or item
                  (fail/log-error-file-nonexistent line-num
                                                   username
                                                   path
                                                   full-path))))
        (fail/log-error-path-not-safe line-num username path full-path)))

  (if (absolute-pathname? path)
      (let ((full-path (make-pathname root-dir (trim-path-selector path))))
        (make-item full-path (trim-path-selector path)))
      (let ((full-path (make-pathname (list root-dir request-selector)
                                      (trim-path-selector path)))
            (item-selector (make-pathname request-selector path)))
        (make-item full-path item-selector) ) ) )


;; Return a menu item menu
(define (dir-item selector path username)
  (let ((item-selector (if (absolute-pathname? path)
                           (trim-path-selector path)
                           (make-pathname selector (string-chomp path "/")))))
    (menu-item 'menu username item-selector (server-hostname) (server-port) ) ) )

;; Parse an index line and return a menu item
(define (parse-line line-num root-dir selector line)
  (let ((link-match (irregex-search index-link-split-regex line)))
    (if (irregex-match-data? link-match)
        (let* ((path (irregex-match-substring link-match 1))
               (maybe-username (irregex-match-substring link-match 2))
               (username (if (string=? maybe-username "")
                             path
                             maybe-username))
               (chomped-username (if (string=? maybe-username "")
                                     ;; Ensure that we don't create an empty selector
                                     (if (string=? path "/")
                                         path
                                         (string-chomp path "/"))
                                     maybe-username)))
          (cond
            ((is-url? path)
              (url-item line-num path username))
            ((is-dir? path)
              (dir-item selector path chomped-username))
            (else
              (file-item line-num root-dir selector path username))))
        ;; Current selector is used for info itemtype so that if type
        ;; not supported by client but still displayed then it
        ;; will just link to the page that it is being displayed on
        (menu-item 'info line selector (server-hostname) (server-port) ) ) ) )
