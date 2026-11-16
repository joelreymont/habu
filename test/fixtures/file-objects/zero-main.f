\ zero-main.f - a program whose MAIN throws a computed 0, built by
\ tools/hb-build.f.

: Z ( n -- ) throw ;
: MAIN ( -- ) s" " nip Z ;
