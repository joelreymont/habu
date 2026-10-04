\ id.f - persistent identities, never-reused counters and error classes for
\ the browser runtime (docs/browser-runtime.md §2.1, §5.2).
\
\ STORAGE CLASS. CALLER-OWNED: the package keeps no state. A counter's state
\ lives in the caller's cells, COUNTER-CELLS of them (`n TYPED-BUFFER C n`), so
\ any number of tasks use their own counters at once. The state is a private
\ record: COUNTER writes it into zeroed cells and answers the counter, an
\ opaque token for them. A counter cell COUNTER has not filled holds the zero
\ token, which NEXT and CLOSE refuse before reading a cell.
\
\ An Id128 is an opaque 16-byte value, two cells holding the bytes big-endian,
\ so the lexicographic order of the bytes is the unsigned order of the high
\ cell, then the low one. A DEFTYPE token is one cell, so id128 is a public
\ STRUCTURE whose cells are the private type id-half. Its generated
\ RT--ID-ID128:MAKE and UNMAKE are public, but outside the package no word
\ makes an id-half or reads one and no cast names one, so the pair only takes
\ Id128s apart and puts their halves back together, two Id128s' halves crossed
\ included.
\
\ BOUNDARY. Outside the package no typed route leads from an Id128 to its
\ cells or from a counter to its state, and none from a number to either:
\ their converters and the state's constructor are private, a cast from a
\ number to a counter is E-CAST-OWNER, and an Id128's bytes are its route, in
\ through BYTES>ID128 and out through ID128>BYTES. Raw storage passes both: a
\ private pointer mint (docs/forth.md, CAST:), such as any package's own
\ `CAST: ( ptr RT-ID:id128 -- ptr n )`, reads and writes the two cells of an
\ Id128 held in storage as numbers, one from a counter to `ptr n` reaches its
\ state, and the public byte and cell views over a cell that holds a counter
\ store any number there. A counter's cells also stay the caller's buffer,
\ which the package does not guard: writing them can rewind a counter or make
\ a counter of them again. Each counter owns its cells exclusively: no two
\ counters, and no counter and table, share a cell, and COUNTER is never handed
\ cells a live or closed counter or a table uses. What follows holds while
\ only this package's words write a counter's cells and each counter's cells
\ are its own.
\
\ A counter issues 1, 2, ... up to 2^64 - 1 (the cell -1) and then throws
\ E-RT-ID-EXHAUSTED instead of wrapping, so a value it issued is never issued
\ again. Every copy of a counter, on the stack or stored and restored, names
\ the one state that NEXT advances, so no copy issues a value again. CLOSE
\ ends that state for every copy and keeps it, so its cells are never made a
\ counter again. ComponentInstanceId and PlacementId each take theirs from
\ their own counter; the package that owns the identity mints its nominal from
\ the value.
\
\ error-class is §5.2's list of failure classes, and a failure keeps the full
\ throw code beside its class; the wire's ErrorClass numbering is the browser
\ layer's.

require lib/errors.f
require lib/type/deftype.f

package RT-ID

private

DEFTYPE ID-HALF

public

-9500 constant E-RT-ID-FIRST
-9509 constant E-RT-ID-LAST
-9500 constant E-RT-ID-LENGTH     \ an Id128 span that is not 16 bytes at a non-null address
-9501 constant E-RT-ID-EXHAUSTED  \ NEXT of a counter that has issued its last value, 2^64 - 1
-9502 constant E-RT-ID-CLOSED     \ NEXT of a counter CLOSE has ended
-9503 constant E-RT-ID-HELD       \ COUNTER over cells that already hold a counter, closed or not
-9504 constant E-RT-ID-NULL       \ COUNTER over null cells, or NEXT or CLOSE of the zero counter token

STRUCTURE id128 0
   FIELD hi id-half
   FIELD lo id-half
;STRUCTURE

DEFTYPE COUNTER
undefine >COUNTER
undefine COUNTER>N

ENUM error-class DERIVE eq domain validation conflict permission unsupported cancelled timeout unresolved quota oom stale platform protocol fatal ;ENUM

STRUCTURE failure 0
   FIELD class error-class
   FIELD code n
;STRUCTURE

private

\ last: the value last issued, 0 before the first; mode: 0 in zeroed cells,
\ then LIVE once COUNTER fills them and CLOSED once CLOSE ends the counter.
STRUCTURE state 0 DERIVE addr
   FIELD last n
   FIELD mode n
;STRUCTURE

public

STATE-CELLS constant COUNTER-CELLS

private

CAST: >COUNTER ( ptr n -- counter )
CAST: COUNTER>STATE ( counter -- ptr state )

16 constant ID128-BYTES
8 constant HALF-BYTES
$8000000000000000 constant SIGN-BIT
-1 constant LAST-ISSUE             \ 2^64 - 1, the last value a counter issues
1 constant LIVE
2 constant CLOSED

: >ID128 ( n n -- id128 )
   {: hi:n lo:n :}
   hi >ID-HALF lo >ID-HALF RT--ID-ID128:MAKE ;

: ID128>N ( id128 -- n n )
   RT--ID-ID128:UNMAKE {: hi:id-half lo:id-half :}
   hi ID-HALF>N lo ID-HALF>N ;

\ The eight bytes at the address as one big-endian cell.
: BE-HALF ( ptr u8 -- n )
   {: a:ptr :}
   0 HALF-BYTES 0 do 8 lshift a i + c@ or loop ;

\ The cell as eight big-endian bytes at the address.
: BE-HALF! ( n ptr u8 -- )
   {: v:n a:ptr :}
   HALF-BYTES 0 do v HALF-BYTES 1 - i - 8 * rshift a i + c! loop ;

\ Refuse an Id128 span that is not 16 bytes at a non-null address.
: ID128-SPAN ( ptr u8 n -- )
   {: a:ptr u:n :}
   u ID128-BYTES <> if E-RT-ID-LENGTH throw then
   a 0= if E-RT-ID-LENGTH throw then ;

\ -1, 0 or 1 as the first cell is below, equal to or above the second,
\ both read unsigned.
: UCOMPARE ( n n -- n )
   {: a:n b:n :}
   a b = if 0 exit then
   a SIGN-BIT xor b SIGN-BIT xor < if -1 exit then
   1 ;

\ The counter's state, refusing the zero token before anything reads through
\ it.
: STATE-OF ( counter -- ptr state )
   COUNTER>STATE {: st:ptr :}
   st 0= if E-RT-ID-NULL throw then
   st ;

public

: BYTES>ID128 ( ptr u8 n -- id128 )
   {: a:ptr u:n :}
   a u ID128-SPAN
   a BE-HALF a HALF-BYTES + BE-HALF >ID128 ;

\ The Id128's sixteen bytes at the address, as BYTES>ID128 reads them.
: ID128>BYTES ( id128 ptr u8 n -- )
   {: x:id128 a:ptr u:n :}
   a u ID128-SPAN
   x ID128>N {: hi:n lo:n :}
   hi a BE-HALF!
   lo a HALF-BYTES + BE-HALF! ;

\ -1, 0 or 1 as the first Id128's bytes sort before, equal or after the
\ second's, byte by byte and unsigned.
: COMPARE ( id128 id128 -- n )
   {: x:id128 y:id128 :}
   x ID128>N {: xh:n xl:n :}
   y ID128>N {: yh:n yl:n :}
   xh yh <> if xh yh UCOMPARE exit then
   xl yl UCOMPARE ;

: EQUAL? ( id128 id128 -- bool )
   COMPARE 0= ;

: ZERO? ( id128 -- bool )
   ID128>N {: hi:n lo:n :}
   hi 0= lo 0= and ;

\ A counter that has issued nothing, its state the COUNTER-CELLS zeroed cells
\ at the address.
: COUNTER ( ptr n -- counter )
   {: at:ptr :}
   at 0= if E-RT-ID-NULL throw then
   at >COUNTER {: c:counter :}
   c COUNTER>STATE {: st:ptr :}
   st STATE-MODE @ 0<> if E-RT-ID-HELD throw then
   0 LIVE STATE-MAKE st !
   c ;

\ The counter's next value, written to its state before it is answered. A
\ counter that has issued 2^64 - 1 has nothing left to issue.
: NEXT ( counter -- n )
   STATE-OF {: st:ptr :}
   st @ STATE-UNMAKE {: last:n mode:n :}
   mode LIVE <> if E-RT-ID-CLOSED throw then
   last LAST-ISSUE = if E-RT-ID-EXHAUSTED throw then
   last 1 + {: v:n :}
   v st STATE-LAST !
   v ;

\ End the counter for every copy. Its state stays, so its cells are never made
\ a counter again.
: CLOSE ( counter -- )
   STATE-OF {: st:ptr :}
   CLOSED st STATE-MODE ! ;

;package
