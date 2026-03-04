Gephor-index
============

This module provides [NEX](https://nightfall.city/nex/info/specification.txt) style index handling to [Gephor](https://github.com/lawrencewoodman/gephor-egg), the embeddable Gopher server written in [Chicken Scheme](https://call-cc.org/).


Requirements
------------
* Chicken Scheme 5.3+

The following eggs need to be installed:
* srfi-1
* srfi-13
* srfi-14
* [gephor](https://github.com/lawrencewoodman/gephor-egg)


NEX Style Index
---------------
An 'index' file is a standard text file that will be converted into a gophermap.  Links to files or menus can be created using the following at the start of a line:
```
=> Path/URL [Optional Text]
```

Paths on the local machine must end with a '/' if they point to a directory.  If a 'Path' is given rather than a full URL then it will be checked that it exists and an appropriate gopher itemtype will be assigned to it.  Paths can be absolute, based off of `root-dir` or relative to the path in which the 'index' file is located.


A URL can point to the schemes/protocols used by `make-item-url`, currently:
* gopher
* ssh
* http
* https

Any problems processing the index file will raise an exception.


Testing
-------
There is a testsuite in `tests/`.  To run it:

    $ csi tests/run


Licence
-------
Copyright (C) 2025-2026 Lawrence Woodman <https://lawrencewoodman.github.io/>

This software is licensed under an MIT Licence.  Please see the file, [LICENCE.md](https://github.com/lawrencewoodman/gephor-index-egg/blob/master/LICENCE.md), for details.
