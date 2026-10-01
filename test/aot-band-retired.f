\ aot-band-retired.f - the call-site scope refusal: a callee no live name reaches.
\
\ CALLER is compiled against GONE, then `undefine` retires that record and a
\ second GONE takes the name, the way a word is replaced. CALLER still calls the
\ first body, and no record in any wordlist the seed can search carries it: the
\ live GONE is another body. So the capture refuses the site by scope, where
\ baking the spelling GONE would boot an engine that calls the wrong word.
\
\ The suite runs this under an empty band, so the call-band audit admits the
\ retired record as the target's and the site reaches the scope audit.

require test/aot-band-lib.f

\ Long enough that a call to it stays a call rather than an inlined copy.
: GONE ( n -- n ) {: v:n :}
   v 1 +  v 2 * +  v 3 * +  v 5 * +  v 7 * +  v 11 * +  v 13 * +
   v 17 * +  v 19 * +  v 23 * +  v 29 * +  v 31 * + ;

AOT-ARM:WINDOW-OPEN
: CALLER ( n -- n ) GONE GONE ;
undefine GONE
: GONE ( n -- n ) 3 + ;
AOT-ARM:WINDOW-CLOSE
AOT-BAND:GO
