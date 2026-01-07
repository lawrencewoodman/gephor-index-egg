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
* [logfmt-logger](https://github.com/lawrencewoodman/logfmt-logger-egg)
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

Any problems processing the index file, such as if a file doesn't exist, will be logged but the file will continue to be processed if possible.

If displayed text on a line is greater than 69 characters a warning will be logged.


Logging
-------
The module uses [logfmt-logger](https://github.com/lawrencewoodman/logfmt-logger-egg) which can be configured by the calling code to decide where the logger will output and what log level will be logged.


Testing
-------
There is a testsuite in `tests/`.  To run it:

    $ csi tests/run


Licence
-------
Copyright (C) 2025 Lawrence Woodman <https://lawrencewoodman.github.io/>

This software is licensed under an MIT Licence.  Please see the file, [LICENCE.md](https://github.com/lawrencewoodman/gephor-index-egg/blob/master/LICENCE.md), for details.
