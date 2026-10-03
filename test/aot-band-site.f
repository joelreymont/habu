\ aot-band-site.f - the name half of the call-site audit.
\
\ A call site travels as a name the seed resolves, and a callee in a package's
\ public wordlist travels qualified, PACKAGE:TAIL, through the qualifier path a
\ compile uses. That path reads one colon, so a public tail holding a colon of its
\ own has no spelling the seed can resolve. MK: is a definer with one at its edge,
\ which its own package can call, and its does>-clause record is named MK:;does
\ (habu2.f DOES-REC). SEVEN, which MK: creates inside the window, ends in a branch
\ to that clause, so the capture would have to bake AOT-BAND-SITE:MK:;does. MAKE
\ is how the window reaches MK: at all: neither a qualified name nor a `using`
\ import resolves a tail with a colon in it.
\
\ The suite runs this under an empty band, so the call-band audit admits the
\ package as the target's and the site reaches the name audit.

require test/aot-band-lib.f

package AOT-BAND-SITE
public

: MK: ( n -- ) create , does> ( -- n ) @ ;
: MAKE ( n -- ) MK: ;

;package

\ The does> definer leaves the DATA cursor off a cell boundary, and a window's
\ DATA base must sit on one.
align

AOT-ARM:WINDOW-OPEN
7 AOT-BAND-SITE:MAKE SEVEN
AOT-ARM:WINDOW-CLOSE
AOT-BAND:GO
