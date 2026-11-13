\ export-main.f - a program built by tools/hb-build.f with --repl, whose input
\ names FILE-OBJECTS-EXP's exports: a private word, another package's public
\ word, a constant and a does> definer.

package FILE-OBJECTS-EXP-SOURCE
public
: PUB ( -- n ) 5 ;
;package

package FILE-OBJECTS-EXP
private
: HIDDEN ( -- n ) 42 ;
3 constant THREE
: CELLED ( n -- ) create , does> ( -- n ) @ ;
public
EXPORT HIDDEN
EXPORT THREE
EXPORT CELLED
EXPORT FILE-OBJECTS-EXP-SOURCE:PUB
;package

: MAIN ( -- ) ;
