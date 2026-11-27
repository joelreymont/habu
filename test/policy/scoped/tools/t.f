\ A word from a file a SOURCE-ROOT:WITH body loaded: the help names the
\ require that loads it from here, not the body's spelling under its root.
require src/lib/fs.f
PATH-CAP 1 + BUFFER: ROOT-BUF
: ROOT$ ( -- str ) SOURCE-ROOT:CURRENT$ s" .." ROOT-BUF FS:JOIN-PATH ROOT-BUF swap >str ;
: LOAD-B ( -- ) s" src/b.f" required ;
: UNDER ( -- ) ROOT$ [: LOAD-B ;] SOURCE-ROOT:WITH ;
UNDER
: GREET ( -- ) UA:HELLO ;
GREET
