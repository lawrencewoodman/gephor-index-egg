;;; Tests for the route handlers

(test-group "handlers"

  (parameterize ((server-hostname "localhost")
                 (server-port 70))

  ;; TODO: Add test for root-dir not existing in case specific incorrectly

  ;; TODO: Add Test for working through the three handlers in serve-path/index

  (test "serve-path/index returns result of serve-index if 'index' is present and can be processed properly"
        ;; Whitespace is stripped at the beginning and end of file
        "iA simple index file to check it is interpreted by serve-path\tdir-b\tlocalhost\t70\r\n.\r\n"
        (serve-path/index fixtures-dir (make-request "dir-b" "127.0.0.1") ) )


  (test "serve-path/index returns result of serve-path if 'index' isn't present"
        (conc "0aa.txt\tdir-a/aa.txt\tlocalhost\t70\r\n0ab.txt\tdir-a/ab.txt\tlocalhost\t70\r\n"
              "9ac.bin\tdir-a/ac.bin\tlocalhost\t70\r\n9empty.txt\tdir-a/empty.txt\tlocalhost\t70\r\n"
              ".\r\n")
        (serve-path/index fixtures-dir (make-request "dir-a" "127.0.0.1") ) )


  (test "serve-path/index returns #f if no handler can handle request"
        #f
        (serve-path/index fixtures-dir (make-request "dir-c" "127.0.0.1") ) )


  (test "serve-index supportes empty selector"
        "iA simple index file to check it is interpreted by serve-path\t\tlocalhost\t70\r\n.\r\n"
        (serve-index (make-pathname fixtures-dir "dir-b")
                                    (make-request "" "127.0.0.1") ) )


  (test "serve-index supportes subpath ('dir-b') selector"
        "iA simple index file to check it is interpreted by serve-path\tdir-b\tlocalhost\t70\r\n.\r\n"
        (serve-index fixtures-dir (make-request "dir-b" "127.0.0.1") ) )


  (test "serve-index processes 'index' files properly if present"
        ;; Whitespace is stripped at the beginning and end of file
        "iA simple index file to check it is interpreted by serve-path\tdir-b\tlocalhost\t70\r\n.\r\n"
        (serve-index fixtures-dir (make-request "dir-b" "127.0.0.1") ) )


  ;; TODO: Should this actually return Not-Applicable?
  (test "serve-index process empty 'index' files properly if present"
        ;; Whitespace is stripped at the beginning and end of file
        ".\r\n"
        (serve-index fixtures-dir (make-request "dir-index_empty_file"
                                                 "127.0.0.1") ) )


  (test "serve-index raises an error and logs an error if 'index' file has an unsafe link"
        '(#f "ts=#t level=error msg=\"problem processing index: path isn't safe\" line-num=3 client-address=127.0.0.1\n" serve-index "problem processing index")
        (parameterize ((log-context (list (cons 'client-address "127.0.0.1"))))
          (run/get-log-and-exn
            'info
            confirm-log-entries-valid-timestamp
            (serve-index fixtures-dir
                         (make-request "dir-index_error" "127.0.0.1") ) ) ) )


  (test "serve-index raises an error if an 'index' file isn't readable"
        (list 'safe-read-file
              (sprintf "can't read file, file is too big: ~A"
                        (make-pathname fixtures-dir "dir-b/index")))
        (handle-exceptions ex
          (list (get-condition-property ex 'exn 'location)
                (get-condition-property ex 'exn 'message))
          (parameterize ((max-response-size 5))
            (serve-index fixtures-dir (make-request "dir-b" "127.0.0.1") ) ) ) )


  (test "serve-path/index returns #f if path doesn't exist"
        #f
        (serve-path/index fixtures-dir (make-request "unknown" "127.0.0.1") ) )


) )

