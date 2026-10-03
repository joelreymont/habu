\ aot-band-export.f - the scope half of the call-site audit: a callee whose only
\ public name is an EXPORT.
\
\ HIDDEN is private to its package, so its own record sits in a wordlist no
\ qualifier reaches. `EXPORT HIDDEN` gives the same body a second record in the
\ public wordlist (habu2.f C-EXPORT), and that is the name CALLER is compiled
\ against. The body's FIRST record is still the private one, so a capture that
\ named a callee by its first record would refuse a call the loader accepted; the
\ site has to travel as AOT-BAND-EXPORT:HIDDEN, the name the export made.
\
\ The suite runs this under an empty band, so the call-band audit admits the
\ package as the target's and the site reaches the scope audit.

require test/aot-band-lib.f

package AOT-BAND-EXPORT

\ Long enough that a call to it stays a call rather than an inlined copy.
: HIDDEN ( n -- n ) {: v:n :}
   v 1 +  v 2 * +  v 3 * +  v 5 * +  v 7 * +  v 11 * +  v 13 * +
   v 17 * +  v 19 * +  v 23 * +  v 29 * +  v 31 * + ;

public
EXPORT HIDDEN
;package

AOT-ARM:WINDOW-OPEN
: CALLER ( n -- n ) AOT-BAND-EXPORT:HIDDEN AOT-BAND-EXPORT:HIDDEN ;
AOT-ARM:WINDOW-CLOSE
AOT-BAND:GO
