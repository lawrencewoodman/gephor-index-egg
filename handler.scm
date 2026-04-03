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
(: serve-path/index (* -> (or string false)))
(define (serve-path/index request)
  (or (serve-index request)
      (serve-path request) ) )


;; If an 'index' file is in the directory formed from document-root and request,
;; process the index file and return the result.
;; See selector->local-path in gephor egg for more information about
;; selector requirements.
;;
;; Returns:
;;   The index file turned into a rendered menu
;;   #f if the request can't be handled
;; Raises an exception:
;;   If index file size > max-response-size
;;   If there are any other problems
(: serve-index (* -> (or string false)))
(define (serve-index request)
  ;; local-path is formed here rather than being passed in
  ;; to ensure that it is formed safely
  (let* ((selector (request-selector request))
         (client-address (request-client-address request))
         (local-path (selector->local-path selector)))
    (and local-path
         (directory? local-path)
           (let ((index-path (make-pathname local-path "index")))
             (and (file-exists? index-path)
                  (and-let* ((index (safe-read-file (max-response-size)
                                                    index-path))
                         (menu (process-index selector index)))
                    (and (not (null? menu))
                         (menu-render menu) ) ) ) ) ) ) )


