\ forth-card-doc-test.f - the worker card's size cap and forth.md pointer.
\ Run: bin/hb --load tools/forth-card-doc-test.f
\
\ docs/forth-card.md is read INSTEAD of docs/forth.md by every worker that
\ writes Habu, in this repo and in the applications. Its whole value is that it
\ stays small, so the cap is a gate rather than a note: a card that regrows past
\ 12 KB costs what forth.md costs and nobody notices until the tokens are spent.

require lib/errors.f
require lib/string.f
require lib/test.f
require lib/fs.f
require tools/lint/text.f

package FORTH-CARD-DOC

12288 constant CARD-CAP          \ 12 KB, about 3K tokens
512 constant HEAD-BYTES          \ the pointer sits at the top, not buried

: CARD$ ( -- ptr u8 n )
   s" docs/forth-card.md" ;

: REFERENCE$ ( -- ptr u8 n )
   s" docs/forth.md" ;

: POINTER$ ( -- ptr u8 n )
   s" Workers: read [docs/forth-card.md](forth-card.md) first; this file is the reference." ;

\ Only the head is searched, so the pointer cannot pass by sitting at the foot
\ of an 88 KB file. Splitting the whole reference into lines overflows the lint
\ line table, which is why this reads a bounded span instead.
: HEAD$ ( -- ptr u8 n )
   REFERENCE$ LINT-SOURCE:LOAD
   LINT-SOURCE:TEXT HEAD-BYTES min ;

: HEAD-HAS-POINTER? ( -- bool )
   HEAD$ POINTER$ LINT-CONTAINS? ;

: REPORT-OVERSIZE ( n -- ) {: size:n :}
   s" forth-card.md is " type size .
   s" bytes, cap " type CARD-CAP . cr ;

: TEST-CARD-EXISTS ( -- )
   CARD$ FILE? TTRUE ;

: TEST-CARD-FITS ( -- )
   CARD$ FILE-SIZE {: size:n :}
   size CARD-CAP > if size REPORT-OVERSIZE then
   size CARD-CAP <= TTRUE ;

: TEST-POINTER ( -- )
   HEAD-HAS-POINTER? TTRUE ;

: MAIN ( -- )
   T-RESET
   TEST-CARD-EXISTS
   TEST-CARD-FITS
   TEST-POINTER
   T-REPORT
   s" forth-card-doc: ok" type cr ;

MAIN

;package
