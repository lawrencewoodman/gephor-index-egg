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
;; Ok, Error or the last one returns Not-Applicable
;;   serve-index serve-path
;;
;; See the documentation for each handler for more information.
;;
;; Returns the value of the last handler tried, this will be:
;;   Ok if everything was ok
;;   Not-Applicable if neither of the handlers can handle the request
;;   Error if there was a problem
;; Returns the value of the last handler tried.
;; TODO: Test
(: serve-path/index (string * -> *))
(define (serve-path/index root-dir request)
  (let ((response (serve-index root-dir request)))
    (cases Result response
      (Not-Applicable () (serve-path root-dir request))
      (else response) ) ) )


;; If an 'index' file is in the directory formed from root-dir and request
;; process the index file and return the result.
;; See selector->local-path in gephor egg for more information about
;; selector requirements.
;;
;; Returns the value of the last handler tried, this will be:
;;   Ok if everything was ok this will contain the result of processing index
;;   Not-Applicable if their isn't an index file or the path isn't a directory
;;   Error if there was a problem
(: serve-index (string * -> *))
(define (serve-index root-dir request)
  ;; local-path is formed here rather than being passed in to ensure that it
  ;; is formed safely
  (let* ((selector (request-selector request))
         (client-address (request-client-address request))
         (rlocal-path (selector->local-path root-dir selector)))
    (cases Result rlocal-path
      (Ok (local-path)
        (if (directory? local-path)
            (let ((index-path (make-pathname local-path "index")))
              (if (file-exists? index-path)
                  ;; TODO: max-response-size for safe-read-file
                  ;; TODO: doesn't make sense, replace this
                  ;; TODO: This needs testing if either fail
                  (let ((rnex-index (safe-read-file (max-response-size)
                                                    root-dir
                                                    index-path)))
                    (cases Result rnex-index
                      (Ok (nex-index)
                        (let ((menu (process-index root-dir selector nex-index)))
                          (cases Result menu
                            (Ok (v) (Ok (menu-render v)))
                            (else menu))))
                      (else rnex-index)))
                  (Not-Applicable #t)))
            (Not-Applicable #t)))
      (else rlocal-path) ) ) )

