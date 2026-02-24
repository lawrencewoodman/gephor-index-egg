;;; Tests for the route handlers

(test-group "handlers"

  (parameterize ((server-hostname "localhost")
                 (server-port 70))

  ;; TODO: Add test for root-dir not existing in case specific incorrectly

  ;; TODO: Add Test for working through the three handlers in serve-path/index

  (test "serve-index supportes empty selector as Ok"
        (Ok "iA simple index file to check it is interpreted by serve-path\t\tlocalhost\t70\r\n.\r\n")
        (serve-index (make-pathname fixtures-dir "dir-b")
                                    (make-request "" "127.0.0.1") ) )


  (test "serve-index supportes subpath ('dir-b') selector"
        (Ok "iA simple index file to check it is interpreted by serve-path\tdir-b\tlocalhost\t70\r\n.\r\n")
        (serve-index fixtures-dir (make-request "dir-b" "127.0.0.1") ) )


  (test "serve-index processes 'index' files properly if present as Ok"
        ;; Whitespace is stripped at the beginning and end of file
        (Ok (string-intersperse '(
            "iA simple index file to check it is interpreted by serve-path\tdir-b\tlocalhost\t70"
            ".\r\n")
            "\r\n"))
        (serve-index fixtures-dir (make-request "dir-b" "127.0.0.1") ) )


  ;; TODO: Should this actually return Not-Applicable?
  (test "serve-index process empty 'index' files properly if present as Ok"
        ;; Whitespace is stripped at the beginning and end of file
        (Ok ".\r\n")
        (serve-index fixtures-dir (make-request "dir-index_empty_file"
                                                 "127.0.0.1") ) )


  (test "serve-index raises an error if an 'index' file isn't readable"
        (list 'safe-read-file
              (sprintf "can't read file, file is too big: ~A"
                        (make-pathname fixtures-dir "dir-b/index")))
        (handle-exceptions ex
          (list (get-condition-property ex 'exn 'location)
                (get-condition-property ex 'exn 'message))
          (parameterize ((max-response-size 5))
            (serve-index fixtures-dir (make-request "dir-b" "127.0.0.1") ) ) ) )


  (test "serve-path/index Not-Applicable if path doesn't exist"
        (Not-Applicable #t)
        (serve-path/index fixtures-dir (make-request "unknown" "127.0.0.1") ) )


) )

