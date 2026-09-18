\ code-bytes.f - the one bounded way a checked body reads the bytes at a code
\ address.
\
\ An engine-supplied code address arrives as a number: XREF-START reads one out
\ of a dictionary record, an execution token is one, and a walk over a span
\ counts in them. A body that wants the machine instructions there needs a byte
\ pointer, and every reader used to mint its own by storing the number in an
\ undeclared cell and fetching it back through ptr-field - one unchecked pun,
\ copied per tool, unnamed and unbounded. AT replaces them: it is the only place
\ the number becomes a pointer, and it refuses an address that is not code.
\
\ WHY TWO BANDS. Code lives in two disjoint places and a record may point into
\ either. The engine's baked text sits at `rbase`, below the region mapping that
\ the dictionary and the emitted code share; everything compiled since boot sits
\ in that region's code band, which starts DICT-SIZE above `dbase@` and ends at
\ the code pointer. A span must lie wholly inside ONE of them: one that
\ straddles the gap, or lands among the dictionary records or the control-flow
\ stack between them, or runs past what has actually been emitted, is not code.
\
\ WHY THE TEXT BAND'S TOP IS dbase@ AND NOT THE IMAGE'S TEXT SIZE. The exact top
\ is the executable's own text-segment size, whose name differs per target
\ (src/os/linux/layout.f LINUX-TEXT-SIZE and its macos twin); reading it here
\ would make this file target-specific for a bound that is already sound without
\ it: the image's read-write tail lies between the text and the region, so
\ `dbase@` is a true upper bound, and the only addresses it admits wrongly are
\ ones in the process's own data. Every absurd address - a wordlist id read as a
\ start, a negative, a number past the emitted code - is refused either way.
\
\ The pointer itself is minted by src/habu/xref.f's XREF-N>U8, the engine's one
\ declared n -> ptr u8 refinement. This file adds no TRUST row of its own; it
\ adds the bound that row never had.
\
\ This file requires nothing, so a tool that must not disturb its own
\ measurement (tools/tier-census.f, tools/tier-dump.f) can load it.

package CODE-BYTES

private

\ [rbase, dbase@) - the engine's baked text.
: TEXT-LO ( -- n ) rbase ;
: TEXT-HI ( -- n ) dbase@ ;

\ [dbase@ + DICT-SIZE, cp@) - the region's code band, up to what is emitted.
: BAND-LO ( -- n ) dbase@ DICT-SIZE + ;
: BAND-HI ( -- n ) cp@ ;

: WITHIN? ( n n n n -- bool ) {: at:n bytes:n lo:n hi:n :}
   at lo < if false exit then
   at bytes + hi > if false exit then
   true ;

public

\ REGION bounds the byte count before it is added to the address, so the sum
\ below cannot wrap: no code span is larger than the region that holds it.
: IN-CODE? ( n n -- bool ) {: at:n bytes:n :}
   at 0 <= if false exit then
   bytes 0 < bytes REGION > or if false exit then
   at bytes TEXT-LO TEXT-HI WITHIN? if true exit then
   at bytes BAND-LO BAND-HI WITHIN? ;

: AT ( n n -- ptr u8 n ) {: at:n bytes:n :}
   at bytes IN-CODE? 0= if
      s" hb: span outside the code region" 74 die
   then
   at XREF-N>U8 bytes ;

;package
