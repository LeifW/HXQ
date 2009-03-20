
$Id: 00-README.txt,v 1.3 2003/08/01 19:59:00 joe Exp $

5 Mar 2002

Announcing HXML version 0.2, a non-validating XML parser written in Haskell.

HXML is available at:

    <URL: http://www.flightlab.com/~joe/hxml >

The current version is 0.2, and is pre-beta quality.

HXML has been tested with GHC 6.0, GHC 5.02, NHC 1.10, and 
various versions of Hugs 98.

Please contact Joe English <jenglish@flightlab.com> with
any questions, comments, or bug reports.

* * * KNOWN BUGS

    + The XML declaration is ignored.
    + Unicode support is only as good as that provided
      by the Haskell system (i.e., typically not very).
    + Does not do any well-formedness or validity checks.
    + Under Hugs 98 only, suffers a serious space fault.
    + Does not support XML Namespaces.

* * * USAGE

Documentation in XML format is available in the 'doc' subdirectory,
along with a Haskell program which converts it into HTML.
Run 'make html' in that directory to build the HTML docs
with Hugs.  doc/mkSite.hs also serves as an example of how
to use the library.

* * * INSTALLATION

Installation instructions depend on the Haskell system.
For Hugs, put the sources somewhere in the Hugs search path.
For NHC, just use 'hmake'.

For GHC, copy Makefile.dist to Makefile, edit as desired, and run
	make library
	make profiled-library		;# optional
Next, edit the file "hxml.conf.in" and replace @INSTDIR@
with the installation directory (i.e., wherever you extracted
the distribution).  Finally, run
    ghc-pkg --add hxml.conf.in
If all goes well, you may then use 'ghc -package hxml' to access
the library.

At some point, I'll add proper 'configure ; make ; make install' support.
Recommended procedures for doing this are still (Aug 2003) being hashed out
on various mailing lists.

* * * END.
