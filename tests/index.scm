;;; Tests for the index procedures

(test-group "index"


  (test "process-index raises an error if file path in 'index' doesn't exist"
        '(file-item "problem processing index on line: 2, path doesn't exist or unknown type: nonexistent.txt")
        (let ((index "hello\n=> nonexistent.txt"))
          (handle-exceptions ex
            (list (get-condition-property ex 'exn 'location)
                  (get-condition-property ex 'exn 'message))
            (menu-render (process-index fixtures-dir "dir-a" index) ) ) ) )


  (test "process-index raises an error if an absolute path in 'index' is unsafe"
        '(file-item "problem processing index on line: 1, path isn't safe: /../run.scm")
        (let ((index "=> /../run.scm An unsafe absolute link\n"))
          (handle-exceptions ex
            (list (get-condition-property ex 'exn 'location)
                  (get-condition-property ex 'exn 'message))
            (menu-render (process-index fixtures-dir "dir-a" index) ) ) ) )


  (test "process-index raises an error if a link to a directory doesn't have a trailing '/'"
        '(file-item "problem processing index on line: 1, path is a directory but link doesn't have a trailing '/': dir-ba")
        (let ((index "=> dir-ba This is actually a directory"))
          (handle-exceptions ex
            (list (get-condition-property ex 'exn 'location)
                  (get-condition-property ex 'exn 'message))
            (menu-render (process-index fixtures-dir "dir-b" index) ) ) ) )


  (test "process-index raises an an error if a relative link in 'index' is unsafe"
        '(file-item "problem processing index on line: 1, path isn't safe: ../run.scm")
        (let ((index "=> ../run.scm An unsafe relative link"))
          (handle-exceptions ex
            (list (get-condition-property ex 'exn 'location)
                  (get-condition-property ex 'exn 'message))
            (menu-render (process-index fixtures-dir "dir-a" index) ) ) ) )


  (test "process-index returns partial processed index and logs an error if a URL is invalid"
        '(file-item "problem processing index on line: 1, invalid URL: telnet://example.com/fred")
        (let ((index "=> telnet://example.com/fred telnet to example"))
          (handle-exceptions ex
            (list (get-condition-property ex 'exn 'location)
                  (get-condition-property ex 'exn 'message))
            (menu-render (process-index fixtures-dir "dir-a" index) ) ) ) )


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
