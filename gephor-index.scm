;;; NEX style index handling for Gephor
;;; For Chicken Scheme.
;;;
;;; Copyright (C) 2025 Lawrence Woodman <https://lawrencewoodman.github.io/>
;;;
;;; Licensed under an MIT licence.  Please see LICENCE.md for details.
;;;


;; TODO: rename  exported functions to make consistent and more predictable?
(module gephor-index
  (process-index
   serve-path/index
   serve-index)


(import scheme
        (chicken base)
        (chicken file)
        (chicken file posix)
        (chicken irregex)
        (chicken pathname)
        (chicken string)
        (chicken type)
        srfi-1
        srfi-13
        srfi-14
        datatype
        logfmt-logger
        gephor)


;; Import notes -------------------------------------------------------------
;; srfi-1         - List procedures
;; srfi-13        - String library
;; srfi-14        - Character set library
;; datatype       - Variant records
;; logfmt-logger  - Logger using logfmt
;; gephor         - A Gopher server


;; Include rest of the code -------------------------------------------------

;; Procedures for processing index files
(include-relative "index.scm")

;; Route handling procedures
(include-relative "handler.scm")

)
