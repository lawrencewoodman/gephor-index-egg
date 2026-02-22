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
;;
;; Returns:
;;   Ok if everything was ok
;;   Error if their was a problem
;; TODO: rename?
(: process-index (string string string --> *))
(define (process-index root-dir request-selector nex-index)
  (define (strip-initial-blank-lines lines)
    (if (null? lines)
        '()
        (if (string-every char-set:whitespace (car lines))
            (strip-initial-blank-lines (cdr lines))
            lines) ) )

  (let* ((lines (string-split
                   (string-trim-right nex-index char-set:whitespace) "\n" #t))
         (slines (strip-initial-blank-lines lines))
         (first-non-blank-line (add1 (- (length lines) (length slines)))))
    (let loop ((lines slines) (line-num first-non-blank-line) (plines '()))
      (if (null? lines)
          (Ok (reverse plines))
          (let* ((line (car lines))
                 (item (parse-line line-num root-dir request-selector line)))
            (cases Result item
                   (Ok (v) (loop (cdr lines) (add1 line-num) (cons v plines)))
                   (else item) ) ) ) ) ) )


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

(define (Error-invalid-url line-num username url)
  (Error "problem processing index: invalid URL"
         (list (cons 'line line-num)
               (cons 'username username)
               (cons 'url url) ) ) )

;; TODO: should path be renamed to link in the Error- funcs below?

(define (Error-directory-not-file line-num username path local-path)
  (Error "problem processing index: path is a directory but link doesn't have a trailing '/'"
         (list (cons 'line line-num)
               (cons 'username username)
               (cons 'path path)
               (cons 'local-path local-path) ) ) )


; TODO: Think about this should the error come from menu-file?
(define (Error-file-nonexistent line-num username path local-path)
  (Error "problem processing index: path doesn't exist or unknown type"
         (list (cons 'line line-num)
               (cons 'username username)
               (cons 'path path)
               (cons 'local-path local-path) ) ) )


(define (Error-path-not-safe line-num username path local-path)
  (Error "problem processing index: path isn't safe"
         (list (cons 'line line-num)
               (cons 'username username)
               (cons 'path path)
               (cons 'local-path local-path) ) ) )


;; Return a menu item from a URL
;; TODO: Test the error from this
;;
;; Returns:
;;   Ok with a menu item
;;   Error if the URL is invalid
(define (url-item line-num path username)
  (let ((item (menu-item-url username path)))
    (if item
        (Ok item)
        (Error-invalid-url line-num username path) ) ) )


;; Return a file menu item
;; TODO: Test the error from this
;; Returns:
;;   Ok with a menu item pointing to the file
;;   Error if the path on the => line is a directory and doesn't have a training /
;;   Error if the path on the => line doesn't exist or is an unknown type
;;   Error if the path on the => line isn't safe
(: file-item (integer string string string string --> *))
(define (file-item line-num root-dir request-selector path username)
  (define (make-item full-path item-selector)
    (if (safe-path? root-dir full-path)
        (if (directory? full-path)
            (Error-directory-not-file line-num username path full-path)
            (let ((item (menu-item-file full-path username item-selector)))
              (if item
                  (Ok item)
                  (Error-file-nonexistent line-num
                                          username
                                          path
                                          full-path))))
        (Error-path-not-safe line-num username path full-path)))

  (if (absolute-pathname? path)
      (let ((full-path (make-pathname root-dir (trim-path-selector path))))
        (make-item full-path (trim-path-selector path)))
      (let ((full-path (make-pathname (list root-dir request-selector)
                                      (trim-path-selector path)))
            (item-selector (make-pathname request-selector path)))
        (make-item full-path item-selector) ) ) )


;; Return a menu item menu
;;
;; Returns:
;;   Ok with a menu item pointing to the directory
(define (dir-item selector path username)
  (let ((item-selector (if (absolute-pathname? path)
                           (trim-path-selector path)
                           (make-pathname selector (string-chomp path "/")))))
    (Ok (menu-item 'menu username item-selector (server-hostname) (server-port) ) ) ) )


;; Parse an index line and return a menu item
;;
;; Returns:
;;   Ok with a menu item
;;   Error if the there is a problem
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
        (Ok (menu-item 'info line selector (server-hostname) (server-port) ) ) ) ) )
