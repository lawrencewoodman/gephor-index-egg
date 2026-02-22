;;; Tests for the index procedures

(test-group "index"


  (test "process-index returns Error if file path in 'index' doesn't exist"
        '("problem processing index: path doesn't exist or unknown type"
          ((line . 2) (username . "A missing file") (path . "nonexistent.txt")
           (local-path . #t)))
        (let ((index "hello\n=> nonexistent.txt A missing file"))
            (cases Result (process-index fixtures-dir "dir-a" index)
                   (Error (msg log-entries)
                          (list msg
                                (confirm-field-matches 'local-path
                                                       ".*?nonexistent.txt$"
                                                       log-entries)))
                   (else #f) ) ) )


  (test "process-index returns Error if an absolute path in 'index' is unsafe"
        '("problem processing index: path isn't safe"
          ((line . 1) (username . "An unsafe absolute link") (path . "/../run.scm")
           (local-path . #t)))
        (let ((index "=> /../run.scm An unsafe absolute link\n"))
            (cases Result (process-index fixtures-dir "dir-a" index)
                   (Error (msg log-entries)
                          (list msg
                                (confirm-field-matches 'local-path
                                                       ".*?run.scm$"
                                                       log-entries)))
                   (else #f) ) ) )


  (test "process-index returns Error if a link to a directory doesn't have a trailing '/'"
        '("problem processing index: path is a directory but link doesn't have a trailing '/'"
          ((line . 2) (username . "This is actually a directory") (path . "dir-ba")
           (local-path . #t)))
        (let ((index "before\n=> dir-ba This is actually a directory\nafter"))
            (cases Result (process-index fixtures-dir "dir-b" index)
                   (Error (msg log-entries)
                          (list msg
                                (confirm-field-matches 'local-path
                                                       ".*?dir-ba$"
                                                       log-entries)))
                   (else #f) ) ) )


  (test "process-index returns Error if a relative link in 'index' is unsafe"
        '("problem processing index: path isn't safe"
          ((line . 2) (username . "An unsafe relative link") (path . "../run.scm")
           (local-path . #t)))
        (let ((index "before\n=> ../run.scm An unsafe relative link\nafter"))
            (cases Result (process-index fixtures-dir "dir-b" index)
                   (Error (msg log-entries)
                          (list msg
                                (confirm-field-matches 'local-path
                                                       ".*?run.scm$"
                                                       log-entries)))
                   (else #f) ) ) )


  (test "process-index returns Error if a URL is invalid"
        '("problem processing index: invalid URL"
          ((line . 2) (username . "telnet to example") (url . "telnet://example.com/fred")))
        (let ((index "before\n=> telnet://example.com/fred telnet to example\nafter"))
            (cases Result (process-index fixtures-dir "dir-a" index)
                   (Error (msg log-entries) (list msg log-entries))
                   (else #f) ) ) )


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
          (cases Result (process-index fixtures-dir "dir-a" index)
                 (Ok (v) (menu-render v))
                 (else #f) ) ) )


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
          (cases Result (process-index fixtures-dir "dir-a" index)
                 (Ok (v) (menu-render v))
                 (else #f) ) ) )


  (test "process-index counts lines properly when there are initial blank lines if there is an error"
        '("problem processing index: invalid URL"
          ((line . 4) (username . "telnet to example") (url . "telnet://example.com/fred")))
        (let ((index "\n  \nbefore\n=> telnet://example.com/fred telnet to example\nafter"))
            (cases Result (process-index fixtures-dir "dir-a" index)
                   (Error (msg log-entries) (list msg log-entries))
                   (else #f) ) ) )


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
          (cases Result (process-index fixtures-dir "dir-a" index)
                 (Ok (v) (menu-render v))
                 (else #f) ) ) )


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
          (cases Result (process-index fixtures-dir "dir-a" index)
                 (Ok (v) (menu-render v))
                 (else #f) ) ) )


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

          (cases Result (process-index fixtures-dir "dir-b" index)
                 (Ok (v) (menu-render v))
                 (else #f) ) ) )


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
          (cases Result (process-index fixtures-dir "dir-a" index)
                 (Ok (v) (menu-render v))
                 (else #f) ) ) )


  (test "process-index uses '/' as the username for a link with this path that doesn't specify a username"
        (string-intersperse '(
          "1/\t\tlocalhost\t70"
          ".\r\n")
          "\r\n")
        (let ((index (string-intersperse '(
                       "=> /"
                       "\n"))))
          (cases Result (process-index fixtures-dir "" index)
                 (Ok (v) (menu-render v))
                 (else #f) ) ) )


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
          (cases Result (process-index fixtures-dir "dir-b" index)
                 (Ok (v) (menu-render v))
                 (else #f) ) ) )


  (test "process-index supports URL links with a ':' in path"
        (string-intersperse '(
          "hOld Example\tURL:https://example.com/http://old.example.com/\tlocalhost\t70"
          ".\r\n")
          "\r\n")
        (let ((index "=> https://example.com/http://old.example.com/ Old Example"))
          (cases Result (process-index fixtures-dir "dir-a" index)
                 (Ok (v) (menu-render v))
                 (else #f) ) ) )



)
