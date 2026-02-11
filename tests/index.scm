;;; Tests for the index procedures

(test-group "index"


  (test "process-index returns partially processed index and logs an error if file in 'index' doesn't exist"
        (list
          "ihello\tdir-a\tlocalhost\t70\r\n.\r\n"
          (sprintf "ts=#t level=error msg=\"problem processing index: file doesn't exist or unknown type\" line=2 path=nonexistent.txt local-path=~A connection-id=2\n"
                   (make-pathname fixtures-dir "dir-a/nonexistent.txt")))
        (let ((index "hello\n=> nonexistent.txt")
              (port (open-output-string)))
          (parameterize ((log-level 0)
                         (log-port port)
                         (log-context (list (cons 'connection-id 2))))
            (list (menu-render (process-index fixtures-dir "dir-a" index))
                  (confirm-log-entries-valid-timestamp (get-output-string port) ) ) ) ) )


  (test "process-index keeps processing after some errors and returns partially processed index"
        (list
          (string-intersperse '(
            "1/unknown\tunknown\tlocalhost\t70"
            "1unknown\tdir-a/unknown\tlocalhost\t70"
            ".\r\n")
            "\r\n")
          (sprintf "ts=#t level=error msg=\"problem processing index: file doesn't exist or unknown type\" line=2 path=\/fred.txt local-path=~A connection-id=2\n"
                   (make-pathname fixtures-dir "fred.txt")))
        (let ((index (string-intersperse '(
                       "=> /unknown/"
                       "=> /fred.txt"
                       "=> unknown/")
                       "\n"))
              (port (open-output-string)))
          (parameterize ((log-level 0)
                         (log-port port)
                         (log-context (list (cons 'connection-id 2))))
            (list (menu-render (process-index fixtures-dir "dir-a" index))
                  (confirm-log-entries-valid-timestamp (get-output-string port) ) ) ) ) )


  (test "process-index returns partially processed index and logs an error if an absolute link in 'index' is unsafe"
        (list '()
              "ts=#t level=error msg=\"problem processing index: path isn't safe\" line=1 path=/../run.scm connection-id=2\n")
        (let ((index (sprintf "=> /../run.scm An unsafe absolute link\n"))
              (port (open-output-string)))
          (parameterize ((log-level 0)
                         (log-port port)
                         (log-context (list (cons 'connection-id 2))))
            (list (process-index fixtures-dir "dir-a" index)
                  (confirm-log-entries-valid-timestamp (get-output-string port) ) ) ) ) )


  (test "process-index returns partially processed index and logs a error if a link to a directory doesn't have a trailing '/'"
        (list '()
              "ts=#t level=error msg=\"problem processing index: path is a directory but link doesn't have a trailing /\" line=1 path=dir-ba connection-id=2\n")
        (let ((index "=> dir-ba This is actually a directory")
              (port (open-output-string)))
          (parameterize ((log-level 0)
                         (log-port port)
                         (log-context (list (cons 'connection-id 2))))
            (list (process-index fixtures-dir "dir-b" index)
                  (confirm-log-entries-valid-timestamp (get-output-string port) ) ) ) ) )


  (test "process-index returns partially processed index and logs an error if a relative link in 'index' is unsafe"
        (list '()
              "ts=#t level=error msg=\"problem processing index: path isn't safe\" line=1 path=../run.scm connection-id=2\n")
        (let ((index "=> ../run.scm An unsafe relative link")
              (port (open-output-string)))
          (parameterize ((log-level 0)
                         (log-port port)
                         (log-context (list (cons 'connection-id 2))))
            (list (process-index fixtures-dir "dir-a" index)
                  (confirm-log-entries-valid-timestamp (get-output-string port) ) ) ) ) )


  (test "process-index returns partial processed index and logs an error if a URL is invalid"
        (list '()
              "ts=#t level=error msg=\"problem processing index: invalid URL\" line=1 username=\"telnet to example\" url=telnet://example.com/fred connection-id=2\n")
        (let ((index "=> telnet://example.com/fred telnet to example")
              (port (open-output-string)))
          (parameterize ((log-level 0)
                         (log-port port)
                         (log-context (list (cons 'connection-id 2))))
            (list (process-index fixtures-dir "dir-a" index)
                  (confirm-log-entries-valid-timestamp (get-output-string port) ) ) ) ) )


  (test "process-index removes blank lines at top and bottom of index"
        (string-intersperse '(
          "iThere are blank lines above and below this line that should be stripped\tdir-a\tlocalhost\t70"
          ".\r\n")
          "\r\n")
        (let ((index (string-intersperse '(
                       ""
                       ""
                       "There are blank lines above and below this line that should be stripped"
                       ""
                       "")
                       "\n")))
          (menu-render (process-index fixtures-dir "dir-a" index) ) ) )


  (test "process-index doesn't remove initial whitespace of first non blank line"
        (string-intersperse '(
          "i   This line has a few spaces at the start and two blanks lines before it\tdir-a\tlocalhost\t70"
          ".\r\n")
          "\r\n")
        (let ((index (string-intersperse '(
                       ""
                       ""
                       "   This line has a few spaces at the start and two blanks lines before it")
                       "\n")))
          (menu-render (process-index fixtures-dir "dir-a" index) ) ) )


  (test "process-index only recognizes links where => is at the beginning of the line"
        (string-intersperse '(
          "1A link with => starting at the beginning of the line\t\tlocalhost\t70"
          "i => / This isn't a link because => doesn't start at the beginning of the line\tdir-a\tlocalhost\t70"
          ".\r\n")
          "\r\n")
        (let ((index (string-intersperse '(
                       "=> / A link with => starting at the beginning of the line"
                       " => / This isn't a link because => doesn't start at the beginning of the line")
                       "\n")))
          (menu-render (process-index fixtures-dir "dir-a" index) ) ) )


  (test "process-index supports absolute links"
        (string-intersperse '(
          "1Back to the beginning\t\tlocalhost\t70"
          "1/dir-a\tdir-a\tlocalhost\t70"
          "0/a.txt\ta.txt\tlocalhost\t70"
          "1Lots of white space (will be removed)\tdir-a\tlocalhost\t70"
          "0Lots of white space (will be removed)\tb.txt\tlocalhost\t70"
          ".\r\n")
          "\r\n")
        (let ((index (string-intersperse '(
                       "=> / Back to the beginning"
                       "=> /dir-a/"
                       "=> /a.txt"
                       "=>     /dir-a/    Lots of white space (will be removed)    "
                       "=>     /b.txt     Lots of white space (will be removed)    ")
                       "\n")))
          (menu-render (process-index fixtures-dir "dir-a" index) ) ) )


  (test "process-index supports relative links"
        (string-intersperse '(
          "1dir-ba\tdir-b/dir-ba\tlocalhost\t70"
          "1The ba directory\tdir-b/dir-ba\tlocalhost\t70"
          "1The bb directory\tdir-b/dir-bb\tlocalhost\t70"
          "0dir-ba/baa.txt\tdir-b/dir-ba/baa.txt\tlocalhost\t70"
          "9dir-ba/bab.bin\tdir-b/dir-ba/bab.bin\tlocalhost\t70"
          "1Lots of white space (will be removed)\tdir-b/dir-ba\tlocalhost\t70"
          "0Lots of white space (will be removed)\tdir-b/dir-ba/baa.txt\tlocalhost\t70"
         ".\r\n")
          "\r\n")
        (let ((index (string-intersperse '(
                       "=> dir-ba/"
                       "=> dir-ba/ The ba directory"
                       "=> dir-bb/ The bb directory"
                       "=> dir-ba/baa.txt"
                       "=> dir-ba/bab.bin"
                       "=>     dir-ba/            Lots of white space (will be removed)    "
                       "=>     dir-ba/baa.txt     Lots of white space (will be removed)    ")
                       "\n")))

          (menu-render (process-index fixtures-dir "dir-b" index) ) ) )


  (test "process-index supports links to directories that don't exist as long as the link ends with '/'"
        (string-intersperse '(
          "1/unknown\tunknown\tlocalhost\t70"
          "1unknown\tdir-a/unknown\tlocalhost\t70"
          ".\r\n")
          "\r\n")
        (let ((index (string-intersperse '(
                       "=> /unknown/"
                       "=> unknown/")
                       "\n")))
          (menu-render (process-index fixtures-dir "dir-a" index) ) ) )


  (test "process-index uses '/' as the username for a link with this path that doesn't specify a username"
        (string-intersperse '(
          "1/\t\tlocalhost\t70"
          ".\r\n")
          "\r\n")
        (let ((index (string-intersperse '(
                       "=> /"
                       "\n"))))
          (menu-render (process-index fixtures-dir "" index) ) ) )


  (test "process-index supports URL links"
        (string-intersperse '(
          "hhttp://example.com\tURL:http://example.com\tlocalhost\t70"
          "hhttp://example.com/fred\tURL:http://example.com/fred\tlocalhost\t70"
          "hhttp://example.com/fred/\tURL:http://example.com/fred/\tlocalhost\t70"
          "hFred's things\tURL:http://example.com/fred\tlocalhost\t70"
          "hLots of white space (will be removed)\tURL:http://example.com/fred\tlocalhost\t70"
        ".\r\n")
          "\r\n")
        (let ((index (string-intersperse '(
                       "=> http://example.com"
                       "=> http://example.com/fred"
                       "=> http://example.com/fred/"
                       "=> http://example.com/fred Fred's things"
                       "=> http://example.com/fred      Lots of white space (will be removed)   ")
                       "\n")))
          (menu-render (process-index fixtures-dir "dir-b" index) ) ) )


  (test "process-index supports URL links with a ':' in path"
        (string-intersperse '(
          "hOld Example\tURL:https://example.com/http://old.example.com/\tlocalhost\t70"
          ".\r\n")
          "\r\n")
        (let ((index "=> https://example.com/http://old.example.com/ Old Example"))
          (menu-render (process-index fixtures-dir "dir-a" index) ) ) )



)
