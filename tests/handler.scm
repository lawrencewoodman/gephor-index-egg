;;; Tests for the route handlers

(test-group "handlers"

  (parameterize ((server-hostname "localhost")
                 (server-port 70))


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


  (test "serve-index returns #f if 'index' is empty or just whitespace"
        '(#f #f)
        (map (lambda (selector)
               (serve-index fixtures-dir
                            (make-request selector "127.0.0.1")))
             '("dir-index_empty_file" "dir-index_whitespace_file") ) )


  (test "serve-index returns #f if root-dir doesn't exist"
        #f
        (serve-index (make-pathname fixtures-dir "nothere")
                     (make-request "" "127.0.0.1") ) )


  (test "serve-index returns #f if 'index' can't be read if for example it isn't world readable"
        '("iThis is used to test an index file that isn't world readable\t\tlocalhost\t70\r\n.\r\n" #f)
        (let* ((tmpdir (create-temporary-directory))
               (tmpfile (make-pathname tmpdir "index")))
          (copy-file (make-pathname (list fixtures-dir "dir-index_world_readable")
                                    "index")
                     tmpfile)
          (let ((response1 (serve-index tmpdir (make-request "" "127.0.0.1")))
                (response2
                  (begin
                    ;; Make tmpfile non world readable
                    (set-file-permissions! tmpfile
                                           (bitwise-and (file-permissions tmpfile)
                                                        (bitwise-not perm/iroth)))
                    (serve-index tmpdir (make-request "" "127.0.0.1")))))
            (list response1 response2) ) ) )


  (test "serve-index raises an exception if root-dir is empty"
        '(#f safe-path? "root-dir must be an absolute directory: ")
        (run/get-exn (serve-index ""
                                  (make-request "dir-index_error" "127.0.0.1") ) ) )


  (test "serve-index raises an exception if 'index' file has an unsafe link"
        '(#f serve-index "problem processing index at line 3: path isn't safe")
        (run/get-exn (serve-index fixtures-dir
                                  (make-request "dir-index_error" "127.0.0.1") ) ) )


  (test "serve-index raises an exception if an 'index' file isn't readable"
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

