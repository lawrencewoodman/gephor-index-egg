;;; Tests for the route handlers

(test-group "handlers"

  (parameterize ((server-hostname "localhost")
                 (server-port 70))

  ;; TODO: Add test for root-dir not existing in case specific incorrectly

  ;; TODO: Add Test for working through the three handlers in serve-path/index

  (test "serve-index supportes empty selector"
        "iA simple index file to check it is interpreted by serve-path\t\tlocalhost\t70\r\n.\r\n"
        (serve-index (make-pathname fixtures-dir "dir-b")
                     (make-request "" "127.0.0.1") ) )


  (test "serve-index supportes subpath ('dir-b') selector"
        "iA simple index file to check it is interpreted by serve-path\tdir-b\tlocalhost\t70\r\n.\r\n"
        (serve-index fixtures-dir (make-request "dir-b" "127.0.0.1") ) )


  (test "serve-index process 'index' files properly if present"
        ;; Whitespace is stripped at the beginning and end of file
        (string-intersperse '(
          "iA simple index file to check it is interpreted by serve-path\tdir-b\tlocalhost\t70"
          ".\r\n")
          "\r\n")
        (serve-index fixtures-dir (make-request "dir-b" "127.0.0.1") ) )


  (test "serve-index process empty 'index' files properly if present"
        ;; Whitespace is stripped at the beginning and end of file
        ".\r\n"
        (serve-index fixtures-dir
                    (make-request "dir-index_empty_file" "127.0.0.1") ) )


  (test "serve-index returns #f  if an 'index' file isn't world readable"
        (list (string-intersperse '(
                "iThis is used to test an index file that isn't world readable\t\tlocalhost\t70"
                ".\r\n")
                "\r\n")
              #f)
        (let* ((tmpdir (create-temporary-directory))
               (request (make-request "" "127.0.0.1")))
          (create-directory (make-pathname tmpdir "dir-a"))
          (create-directory (make-pathname tmpdir "dir-b"))
          (copy-file (make-pathname (list fixtures-dir "dir-index_world_readable") "index")
                     (make-pathname tmpdir "index"))
          (let ((response1 (serve-index tmpdir request))
                (response2
                  (begin
                    ;; Make tmpdir non world readable
                    (set-file-permissions! (make-pathname tmpdir "index")
                                           (bitwise-and (file-permissions tmpdir)
                                                        (bitwise-not perm/iroth)))
                    (serve-index tmpdir request))))
            (list response1 response2) ) ) )


  (test "serve-path returns false if path doesn't exist"
        #f
        (serve-path fixtures-dir (make-request "unknown" "127.0.0.1") ) )


) )

