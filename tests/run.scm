
(import scheme
        test
        (chicken base)
        (chicken bitwise)
        (chicken condition)
        (chicken file)
        (chicken file posix)
        (chicken format)
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

;; The path of the tests directory
(define tests-dir
  (let loop ((dirs (list (current-directory) (normalize-pathname (make-pathname (current-directory) "..")))))
    (if (null? dirs) (error "can't find tests directory"))
    (let ((try-path (make-pathname (car dirs) "tests")))
      (if (and (file-exists? try-path) (directory? try-path))
          try-path
          (loop (cdr dirs))))))


;; The path of the fixtures directory
(define fixtures-dir
  (let loop ((dirs (list (current-directory) tests-dir)))
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


;; Run expressions and return list with two values:
;;   The return value of the last expression in body
;;   Any entries logged which are at log level or above after
;;    running log-transform-proc on the log output
(define-syntax run/get-log-and-exn
  (syntax-rules ()
    ((run/get-log level log-transform-proc expr expr* ...)
      (parameterize ((log-level level)
                     (log-port (open-output-string)))
      (handle-exceptions ex
        (let ((log (log-transform-proc (get-output-string (log-port)))))
          (close-output-port (log-port))
          (list #f
                log
                (get-condition-property ex 'exn 'location)
                (get-condition-property ex 'exn 'message)))
          (let* ((ret (begin expr expr* ...))
                 (log (log-transform-proc (get-output-string (log-port)))))
            (close-output-port (log-port))
            (list ret log #f) ) ) ) ) ) )


;; Test each exported component
(include-relative "index.scm")
(include-relative "handler.scm")

(test-exit)
