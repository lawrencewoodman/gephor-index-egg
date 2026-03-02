
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
        gephor)

;; Import notes -------------------------------------------------------------
;; gephor         - The embeddable gopher server


(load-relative "../gephor-index.scm")
(import gephor-index)


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


;; Run expressions and returns the return value and any exception raised.
;; Returns a list with three values:
;;   The return value of the last expression in body or #f if didn't return
;;   The location of an exception or #f if no exception
;;   The message of an exception or #f if no exception
(define-syntax run/get-exn
  (syntax-rules ()
    ((run/get-exn expr expr* ...)
     (handle-exceptions ex
       (list #f
             (get-condition-property ex 'exn 'location)
             (get-condition-property ex 'exn 'message))
       (list (begin expr expr* ...) #f #f) ) ) ) )


;; Test each exported component
(include-relative "index.scm")
(include-relative "handler.scm")

(test-exit)
