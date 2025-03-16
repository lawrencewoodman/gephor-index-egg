
(import scheme
        test
        (chicken base)
        (chicken bitwise)
        (chicken file)
        (chicken file posix)
        (chicken irregex)
        (chicken load)
        (chicken pathname)
        (chicken process-context)
        (chicken string)
        logfmt-logger
        gephor)

;; Import notes -------------------------------------------------------------
;; logfmt-logger  - Logger using logfmt
;; gephor         - The embeddable gopher server


(load-relative "../gephor-index.scm")
(import gephor-index)

(define dummy)


;; TODO: Test log output
(log-level 100)


;; The path of the fixtures directory
(define fixtures-dir
  (let loop ((dirs (list (current-directory) (make-pathname (current-directory) "tests"))))
    (if (null? dirs) (error "can't find fixtures directory"))
    (let ((try-path (make-pathname (car dirs) "fixtures")))
      (if (and (file-exists? try-path) (directory? try-path))
          try-path
          (loop (cdr dirs))))))


;; Check log timestamp (ts) field is in the expected ISO 8601 format
;; Returns the log entries with ts=#t if timestamp is valid
(define (confirm-log-entries-valid-timestamp entry)
  (irregex-replace/all "ts=\\d\\d\\d\\d-\\d\\d-\\d\\dT\\d\\d:\\d\\d:\\d\\d[+-]\\d\\d\\d\\d "
                       entry
                       "ts=#t ") )


;; Test each exported component
(include-relative "index.scm")
(include-relative "handler.scm")

(test-exit)
