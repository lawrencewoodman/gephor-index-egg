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
;;   The index file convert to a menu
;; Raises an error:
;;   If there was a problem
;; TODO: rename?
;; TODO: Update the return type
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
          (reverse plines)
          (let* ((line (car lines))
                 (item (parse-line line-num root-dir request-selector line)))
            (loop (cdr lines) (add1 line-num) (cons item plines) ) ) ) ) ) )


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


;; Return a menu item from a URL
;; TODO: Test the error from this
;;
;; Returns:
;;   The URL as a menu item
;; Raises an error:
;;   If the URL is invalid
(define (url-item line-num path username)
  (let ((item (menu-item-url username path)))
    (if item
        item
        (error* 'process-index
                "problem processing index, invalid URL, line: ~A"
                line-num) ) ) )


;; Return a file menu item
;; TODO: Test the error from this
;; Returns:
;;   A menu item pointing to the file
;; Raises an error:
;;   If the path on the => line is a directory and doesn't have a training /
;;   If the path on the => line doesn't exist or is an unknown type
;;   If the path on the => line isn't safe
;; TODO: Update return type
(: file-item (integer string string string string --> *))
(define (file-item line-num root-dir request-selector path username)
  (define (make-item full-path item-selector)
    (if (safe-path? root-dir full-path)
        (if (directory? full-path)
            (error* 'process-index
                    "problem processing index, path is a directory but link doesn't have a trailing '/', line: ~A"
                    line-num)
            (let ((item (menu-item-file full-path username item-selector)))
              (if item
                  item
                  (error* 'process-index
                          "problem processing index, path doesn't exist or unknown type, line: ~A"
                          line-num))))
        (error* 'process-index
                "problem processing index, path isn't safe, line: ~A"
                line-num) ) )

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
;;   A menu item pointing to the directory
(define (dir-item selector path username)
  (let ((item-selector (if (absolute-pathname? path)
                           (trim-path-selector path)
                           (make-pathname selector (string-chomp path "/")))))
    (menu-item 'menu username item-selector (server-hostname) (server-port) ) ) )


;; Parse an index line and return a menu item
;;
;; Returns:
;;   The line as a menu item
;; Raises an error:
;;   If the there is a problem
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

