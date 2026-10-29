\ code-bytes.f - the one bounded way a checked body reads the bytes at a code
\ address. An engine code address is a number (XREF-START, an xt, a span walk);
\ AT is the only place it becomes a pointer, through the private cast N>U8, and
\ it refuses a span IN-CODE? does not admit. Requires nothing, so a tool that must
\ not disturb its own measurement (tools/tier-census.f, tools/tier-dump.f) loads it.

package CODE-BYTES

private

CAST: N>U8 ( n -- ptr u8 )

\ Code lives in two disjoint bands and a span lies wholly inside one: a span
\ straddling the gap, landing among the dictionary records or control-flow stack
\ between them, or running past what is emitted, is not code.
\ [rbase, dbase@) - the engine's baked text. dbase@ is a true upper bound: the
\ image's read-write tail lies between the text and the region, so it admits
\ wrongly only addresses in the process's own data. The exact text size is
\ per target (src/os/linux/layout.f LINUX-TEXT-SIZE and its macos twin).
: TEXT-LO ( -- n ) rbase ;
: TEXT-HI ( -- n ) dbase@ ;

\ [dbase@ + DICT-SIZE, cp@) - the region's code band, up to what is emitted.
: BAND-LO ( -- n ) dbase@ DICT-SIZE + ;
: BAND-HI ( -- n ) cp@ ;

: WITHIN? ( n n n n -- bool ) {: at:n bytes:n lo:n hi:n :}
   at lo < if false exit then
   bytes hi at - > if false exit then
   true ;

public

\ REGION bounds the byte count: no code span is larger than the region that
\ holds it. WITHIN? compares the count with the room between the address and
\ the band top, so an address near the maximum cell cannot wrap a sum back
\ under the top.
: IN-CODE? ( n n -- bool ) {: at:n bytes:n :}
   at 0 <= if false exit then
   bytes 0 < bytes REGION > or if false exit then
   at bytes TEXT-LO TEXT-HI WITHIN? if true exit then
   at bytes BAND-LO BAND-HI WITHIN? ;

: AT ( n n -- ptr u8 n ) {: at:n bytes:n :}
   at bytes IN-CODE? 0= if
      s" hb: span outside the code region" 74 die
   then
   at N>U8 bytes ;

;package
