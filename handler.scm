;;; The route handler
;;;
;;; Definitions are exported in gephor-index.scm
;;; From this file the following are exported:
;;;   serve-index serve-path/index
;;;
;;; Copyright (C) 2026 Lawrence Woodman <https://lawrencewoodman.github.io/>
;;;
;;; Licensed under an MIT licence.  Please see LICENCE.md for details.
;;;

;; Exported Definitions ------------------------------------------------------


;; Tries the following handlers in turn until one returns
;; successfully or the last one fails:
;;   serve-index serve-path
;;
;; See the documentation for each handler for more information.
;;
;; Returns:
;;   The value of the last successful handler
;;   #f if the the request can't be handled
(: serve-path/index (string * -> (or string false)))
(define (serve-path/index root-dir request)
  (or (serve-index root-dir request)
      (serve-path root-dir request) ) )


;; If an 'index' file is in the directory formed from root-dir and request
;; process the index file and return the result.
;; See selector->local-path in gephor egg for more information about
;; selector requirements.
;;
;; Returns:
;;   The index file turned into a rendered menu
;;   #f if the request can't be handled
;; Logs an error and raises an exception:
;;   If there are some problems
(: serve-index (string * -> (or string false)))
(define (serve-index root-dir request)
  ;; local-path is formed here rather than being passed in to ensure that it
  ;; is formed safely
  (let* ((selector (request-selector request))
         (client-address (request-client-address request))
         (local-path (selector->local-path root-dir selector)))
    (and local-path
         (directory? local-path)
           (let ((index-path (make-pathname local-path "index")))
             (and (file-exists? index-path)
                  ;; TODO: max-response-size for safe-read-file
                  ;; TODO: doesn't make sense, replace this
                  ;; TODO: This needs testing if either fail
                  (let* ((index (safe-read-file (max-response-size)
                                                root-dir
                                                index-path))
                         (menu (process-index root-dir selector index)))
                    (and menu
                         (menu-render menu) ) ) ) ) ) ) )


