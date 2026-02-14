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
(: process-index (string string string --> (list-of menu-item)))
(define (process-index root-dir request-selector nex-index)
  (define (parse-line-add-to-result result line-num line)
    (if (and (null? result) (string-every char-set:whitespace line))
        '()  ; Remove first blank lines
        (cons (parse-line line-num root-dir request-selector line) result) ) )

  (let* ((lines (string-split (string-trim-right nex-index
                                                 char-set:whitespace) "\n" #t))
         (parsed-lines
           (do ((lines lines (cdr lines))
                (line-num 1 (+ line-num 1))
                (result '() (parse-line-add-to-result result line-num (car lines))))
               ((null? lines) result))))
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


(define (error-index-line/invalid-url location line-num url)
  (error 'file-item
         (sprintf "problem processing index on line: ~A, invalid URL: ~A"
                  line-num
                  url) ) )

(define (error-index-line/directory-not-file location line-num path)
  (error 'file-item
         (sprintf "problem processing index on line: ~A, path is a directory but link doesn't have a trailing '/': ~A"
                  line-num
                  path) ) )

(define (error-index-line/file-nonexistent location line-num path local-path)
  (error 'file-item
         (sprintf "problem processing index on line: ~A, path doesn't exist or unknown type: ~A"
                  line-num
                  path) ) )

(define (error-index-line/path-not-safe location line-num path)
  (error 'file-item
         (sprintf "problem processing index on line: ~A, path isn't safe: ~A"
                  line-num
                  path) ) )

;; Return a menu item from a URL
;; TODO: Test the error from this
(define (url-item line-num path username)
  (let ((item (menu-item-url username path)))
    (if item
        item
        (error-index-line/invalid-url 'url-item line-num path) ) ) )



;; Return a file menu item
;; If the path on the => line is a directory and doesn't have a training /, it will raise an error
;; If the path on the => line doesn't exist or is an unknown type it will raise an error
;; If the path on the => line isn't safe it will raise an error
(: file-item (integer string string string string --> menu-item))
(define (file-item line-num root-dir request-selector path username)
  (define (make-item full-path item-selector)
    (if (safe-path? root-dir full-path)
        (if (directory? full-path)
            (error-index-line/directory-not-file 'file-item line-num path)
            (let ((item (menu-item-file full-path username item-selector)))
              (if item
                  item
                  (error-index-line/file-nonexistent 'file-item
                                                     line-num
                                                     path
                                                     full-path))))
        (error-index-line/path-not-safe 'file-item line-num path)))

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
