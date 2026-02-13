;;; The route handler
;;;
;;; Definitions are exported in gephor-index.scm
;;; From this file the following are exported:
;;;   serve-index serve-path/index
;;;
;;; Copyright (C) 2025 Lawrence Woodman <https://lawrencewoodman.github.io/>
;;;
;;; Licensed under an MIT licence.  Please see LICENCE.md for details.
;;;

;; Exported Definitions ------------------------------------------------------

;; Tries the following handlers in turn until one returns
;; non-false or the last one fails:
;;   serve-index serve-path
;; Returns the value of the last handler tried.
;; TODO: Test
(: serve-path/index (string * -> (or string false)))
(define (serve-path/index root-dir request)
  (any (lambda (h) (h root-dir request))
       (list serve-index serve-path) ) )


;; If an 'index' file is in the directory formed from root-dir and request
;; process the index file and return the result.
;; See selector->local-path in gephor egg for more information about
;; selector requirements.
;; Returns #f if index file doesn't exist or can't be processed properly.
(: serve-index (string * -> (or string false)))
(define (serve-index root-dir request)
  (let ((selector (request-selector request))
        (client-address (request-client-address request)))
    ;; local-path is formed here rather than being passed in to ensure that it
    ;; is formed safely
    (and-let* ((local-path (selector->local-path root-dir selector)))
      (and (directory? local-path)
           (let ((index-path (make-pathname local-path "index")))
             (and (file-exists? index-path)
                  ;; TODO: max-response-size for safe-read-file
                  ;; TODO: doesn't make sense, replace this
                  (let* ((nex-index (safe-read-file (max-response-size)
                                                    root-dir
                                                    index-path))
                         (response (process-index root-dir selector nex-index)))
                    (apply log-info
                           "serving index"
                           (cons 'handler 'serve-index)
                           (cons 'index-path index-path)
                           (log-context))
                    (menu-render response) ) ) ) ) ) ) )

