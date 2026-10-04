\ id-test.f - Id128s, counters, handles and handle tables through the RT-ID and
\ RT-HANDLE load path, with the typed routes each type refuses: from a number,
\ back to a counter's earlier state and to a table or counter its maker did
\ not answer. The private pointer mint that id.f's and handle.f's headers name
\ passes the first.
\ Run: bin/hb --load lib/runtime/id-test.f

require lib/errors.f
require lib/string.f
require lib/test.f
require lib/test/outcome.f
require lib/runtime/id.f
require lib/runtime/handle.f

package RIDT

16 BUFFER: ID-BYTES
16 BUFFER: OUT-BYTES
8 BUFFER: WIRE

\ ---- Id128 ------------------------------------------------------------------

\ An Id128 whose bytes are zero but for byte `at`, which holds `v`.
: ID-AT ( n n -- RT-ID:id128 )
   {: at:n v:n :}
   16 0 do 0 ID-BYTES i + c! loop
   v ID-BYTES at + c!
   ID-BYTES 16 RT-ID:BYTES>ID128 ;

: ORDER ( -- )
   s" a higher last byte sorts after" T-LABEL
   15 0 ID-AT 15 1 ID-AT RT-ID:COMPARE -1 T=
   15 1 ID-AT 15 0 ID-AT RT-ID:COMPARE 1 T=
   s" the first byte outranks the last" T-LABEL
   0 1 ID-AT 15 $FF ID-AT RT-ID:COMPARE 1 T=
   s" byte 7 outranks byte 8 across the two cells" T-LABEL
   7 1 ID-AT 8 $FF ID-AT RT-ID:COMPARE 1 T=
   s" bytes compare unsigned in the high cell" T-LABEL
   0 $80 ID-AT 0 $7F ID-AT RT-ID:COMPARE 1 T=
   s" and in the low cell" T-LABEL
   8 $80 ID-AT 8 $7F ID-AT RT-ID:COMPARE 1 T=
   s" equal bytes compare equal" T-LABEL
   9 $5A ID-AT 9 $5A ID-AT RT-ID:COMPARE 0 T= ;

: EQUALITY ( -- )
   s" two Id128s from the same bytes are equal" T-LABEL
   3 7 ID-AT 3 7 ID-AT RT-ID:EQUAL? TTRUE
   s" the same byte at another place is not" T-LABEL
   3 7 ID-AT 4 7 ID-AT RT-ID:EQUAL? TFALSE
   s" sixteen zero bytes are the zero Id128" T-LABEL
   0 0 ID-AT RT-ID:ZERO? TTRUE
   s" one byte set in either cell is not" T-LABEL
   0 $80 ID-AT RT-ID:ZERO? TFALSE
   15 1 ID-AT RT-ID:ZERO? TFALSE ;

: ID-SPANS ( -- )
   s" an Id128 is exactly 16 bytes" T-LABEL
   [: ID-BYTES 15 RT-ID:BYTES>ID128 RT-ID:ZERO? drop ;] RT-ID:E-RT-ID-LENGTH TTHROWSQ
   [: ID-BYTES 17 RT-ID:BYTES>ID128 RT-ID:ZERO? drop ;] RT-ID:E-RT-ID-LENGTH TTHROWSQ
   [: ID-BYTES -1 RT-ID:BYTES>ID128 RT-ID:ZERO? drop ;] RT-ID:E-RT-ID-LENGTH TTHROWSQ
   s" at an address" T-LABEL
   [: NULL-PTR 16 RT-ID:BYTES>ID128 RT-ID:ZERO? drop ;] RT-ID:E-RT-ID-LENGTH TTHROWSQ
   s" and is written to exactly 16 bytes at an address" T-LABEL
   [: 0 0 ID-AT OUT-BYTES 15 RT-ID:ID128>BYTES ;] RT-ID:E-RT-ID-LENGTH TTHROWSQ
   [: 0 0 ID-AT OUT-BYTES 17 RT-ID:ID128>BYTES ;] RT-ID:E-RT-ID-LENGTH TTHROWSQ
   [: 0 0 ID-AT NULL-PTR 16 RT-ID:ID128>BYTES ;] RT-ID:E-RT-ID-LENGTH TTHROWSQ ;

\ Sixteen distinct bytes, the high bit set in some of each cell's.
: SPREAD ( -- RT-ID:id128 )
   16 0 do i 37 * 11 + ID-BYTES i + c! loop
   ID-BYTES 16 RT-ID:BYTES>ID128 ;

: ROUND-TRIP ( -- )
   s" an Id128 writes back the bytes it was read from" T-LABEL
   SPREAD OUT-BYTES 16 RT-ID:ID128>BYTES
   OUT-BYTES 16 ID-BYTES 16 T$=
   s" and those bytes read back as the same Id128" T-LABEL
   OUT-BYTES 16 RT-ID:BYTES>ID128 SPREAD RT-ID:EQUAL? TTRUE
   s" a one-byte Id128 writes its byte in place, either cell" T-LABEL
   7 $80 ID-AT OUT-BYTES 16 RT-ID:ID128>BYTES
   OUT-BYTES 16 ID-BYTES 16 T$=
   8 $80 ID-AT OUT-BYTES 16 RT-ID:ID128>BYTES
   OUT-BYTES 16 ID-BYTES 16 T$= ;

\ ---- counters ---------------------------------------------------------------

RT-ID:COUNTER-CELLS TYPED-BUFFER CELLS-A n
RT-ID:COUNTER-CELLS TYPED-BUFFER CELLS-B n
RT-ID:COUNTER-CELLS TYPED-BUFFER CELLS-C n
RT-ID:COUNTER-CELLS TYPED-BUFFER CELLS-D n
TYPED-VARIABLE C RT-ID:counter
TYPED-VARIABLE SAVED RT-ID:counter
TYPED-VARIABLE C-Z RT-ID:counter

: FRESH-COUNTER ( -- )
   0 CELLS-A RT-ID:COUNTER C !
   s" a fresh counter issues 1, then 2" T-LABEL
   C @ RT-ID:NEXT 1 T=
   C @ RT-ID:NEXT 2 T=
   s" cells that hold a live counter are not made a counter again" T-LABEL
   [: 0 CELLS-A RT-ID:COUNTER drop ;] RT-ID:E-RT-ID-HELD TTHROWSQ
   s" and the counter there goes on" T-LABEL
   C @ RT-ID:NEXT 3 T= ;

\ Restoring a counter saved before an issue would issue that value twice if the
\ copy held the counter's state; every copy names the one state instead.
: REWIND ( -- )
   0 CELLS-B RT-ID:COUNTER C !
   s" a counter saved and restored after an issue goes on from that issue" T-LABEL
   C @ SAVED !
   C @ RT-ID:NEXT 1 T=
   SAVED @ C !
   C @ RT-ID:NEXT 2 T=
   s" and so does a copy on the stack" T-LABEL
   0 CELLS-C RT-ID:COUNTER dup RT-ID:NEXT 1 T= RT-ID:NEXT 2 T= ;

: CLOSED ( -- )
   0 CELLS-D RT-ID:COUNTER C !
   C @ SAVED !
   C @ RT-ID:NEXT 1 T=
   SAVED @ RT-ID:CLOSE
   s" a counter closed through one copy refuses NEXT through every copy" T-LABEL
   [: C @ RT-ID:NEXT drop ;] RT-ID:E-RT-ID-CLOSED TTHROWSQ
   [: SAVED @ RT-ID:NEXT drop ;] RT-ID:E-RT-ID-CLOSED TTHROWSQ
   s" a second close changes nothing" T-LABEL
   C @ RT-ID:CLOSE
   [: C @ RT-ID:NEXT drop ;] RT-ID:E-RT-ID-CLOSED TTHROWSQ
   s" and its cells are never made a counter again" T-LABEL
   [: 0 CELLS-D RT-ID:COUNTER drop ;] RT-ID:E-RT-ID-HELD TTHROWSQ ;

\ C-Z is never filled: it holds the zero token of its zeroed image.
: UNMADE ( -- )
   s" a counter cell COUNTER has not filled is refused before it is read" T-LABEL
   [: C-Z @ RT-ID:NEXT drop ;] RT-ID:E-RT-ID-NULL TTHROWSQ
   [: C-Z @ RT-ID:CLOSE ;] RT-ID:E-RT-ID-NULL TTHROWSQ
   s" and so are null cells" T-LABEL
   [: NULL-PTR RT-ID:COUNTER drop ;] RT-ID:E-RT-ID-NULL TTHROWSQ ;

\ ---- handles ----------------------------------------------------------------

\ The wire's eight bytes of a handle: the slot u32, then the generation u32,
\ each little-endian.
: WIRE$ ( n n -- ptr u8 n )
   {: slot:n gen:n :}
   4 0 do slot i 8 * rshift $FF and WIRE i + c! loop
   4 0 do gen i 8 * rshift $FF and WIRE 4 + i + c! loop
   WIRE 8 ;

: >WIRE-HANDLE ( n n -- RT-HANDLE:handle )
   WIRE$ RT-HANDLE:BYTES>HANDLE ;

: WIRE-FORMS ( -- )
   s" eight zero bytes are the null handle" T-LABEL
   0 0 >WIRE-HANDLE RT-HANDLE:NULL? TTRUE
   1 1 >WIRE-HANDLE RT-HANDLE:NULL? TFALSE
   s" a handle with only its slot zero is refused" T-LABEL
   [: 0 1 >WIRE-HANDLE drop ;] RT-HANDLE:E-RT-HANDLE-HALF-NULL TTHROWSQ
   [: 0 $FF000000 >WIRE-HANDLE drop ;] RT-HANDLE:E-RT-HANDLE-HALF-NULL TTHROWSQ
   s" and one with only its generation zero" T-LABEL
   [: 1 0 >WIRE-HANDLE drop ;] RT-HANDLE:E-RT-HANDLE-HALF-NULL TTHROWSQ
   [: $FF000000 0 >WIRE-HANDLE drop ;] RT-HANDLE:E-RT-HANDLE-HALF-NULL TTHROWSQ
   s" a handle is exactly 8 bytes at an address" T-LABEL
   [: WIRE 7 RT-HANDLE:BYTES>HANDLE drop ;] RT-HANDLE:E-RT-HANDLE-LENGTH TTHROWSQ
   [: WIRE 9 RT-HANDLE:BYTES>HANDLE drop ;] RT-HANDLE:E-RT-HANDLE-LENGTH TTHROWSQ
   [: NULL-PTR 8 RT-HANDLE:BYTES>HANDLE drop ;] RT-HANDLE:E-RT-HANDLE-LENGTH TTHROWSQ ;

2 TYPED-BUFFER SLOTS-A n
2 TYPED-BUFFER SLOTS-B n
1 TYPED-BUFFER SLOTS-C n
2 TYPED-BUFFER SLOTS-D n
1 TYPED-BUFFER SLOTS-E n
RT-HANDLE:HEADER-CELLS TYPED-BUFFER HEADER-A n
RT-HANDLE:HEADER-CELLS TYPED-BUFFER HEADER-B n
RT-HANDLE:HEADER-CELLS TYPED-BUFFER HEADER-C n
RT-HANDLE:HEADER-CELLS TYPED-BUFFER HEADER-D n
RT-HANDLE:HEADER-CELLS TYPED-BUFFER HEADER-E n
TYPED-VARIABLE TABLE-A RT-HANDLE:table
TYPED-VARIABLE TABLE-B RT-HANDLE:table
TYPED-VARIABLE TABLE-C RT-HANDLE:table
TYPED-VARIABLE TABLE-D RT-HANDLE:table
TYPED-VARIABLE TABLE-E RT-HANDLE:table
TYPED-VARIABLE TABLE-Z RT-HANDLE:table
TYPED-VARIABLE H-A RT-HANDLE:handle
TYPED-VARIABLE H-B RT-HANDLE:handle
TYPED-VARIABLE H-C RT-HANDLE:handle
TYPED-VARIABLE H-D RT-HANDLE:handle
TYPED-VARIABLE H-E RT-HANDLE:handle

: STALE ( -- n )
   RT-HANDLE:E-RT-HANDLE-STALE ;

: FOREIGN ( -- n )
   RT-HANDLE:E-RT-HANDLE-FOREIGN ;

: RANGE ( -- n )
   RT-HANDLE:E-RT-HANDLE-RANGE ;

\ TABLE-A owns slots 10 and 11.
: ISSUE-AND-RELEASE ( -- )
   0 SLOTS-A 2 10 0 HEADER-A RT-HANDLE:OPEN TABLE-A !
   TABLE-A @ RT-HANDLE:ISSUE H-A !
   TABLE-A @ RT-HANDLE:ISSUE H-B !
   s" a table issues its slots in order" T-LABEL
   H-A @ TABLE-A @ RT-HANDLE:INDEX 0 T=
   H-B @ TABLE-A @ RT-HANDLE:INDEX 1 T=
   s" an issued handle is slot 10 at generation 1 on the wire" T-LABEL
   10 1 >WIRE-HANDLE TABLE-A @ RT-HANDLE:INDEX 0 T=
   s" with every slot live the table is full" T-LABEL
   [: TABLE-A @ RT-HANDLE:ISSUE drop ;] RT-HANDLE:E-RT-HANDLE-FULL TTHROWSQ
   H-A @ TABLE-A @ RT-HANDLE:RELEASE
   s" a released handle is stale" T-LABEL
   [: H-A @ TABLE-A @ RT-HANDLE:INDEX drop ;] STALE TTHROWSQ
   s" and is not released twice" T-LABEL
   [: H-A @ TABLE-A @ RT-HANDLE:RELEASE ;] STALE TTHROWSQ
   s" its slot comes back at the next generation" T-LABEL
   TABLE-A @ RT-HANDLE:ISSUE H-C !
   H-C @ TABLE-A @ RT-HANDLE:INDEX 0 T=
   10 2 >WIRE-HANDLE TABLE-A @ RT-HANDLE:INDEX 0 T=
   s" while the handle it replaced stays stale" T-LABEL
   [: H-A @ TABLE-A @ RT-HANDLE:INDEX drop ;] STALE TTHROWSQ ;

\ Opening TABLE-A's header again would reset it, and its next issue would be
\ slot 10 at generation 1 again: the released H-A, live once more.
: REOPEN ( -- )
   H-C @ TABLE-A @ RT-HANDLE:RELEASE
   s" an open header is not opened again" T-LABEL
   [: 0 SLOTS-A 2 10 0 HEADER-A RT-HANDLE:OPEN drop ;] RT-HANDLE:E-RT-HANDLE-OPEN TTHROWSQ
   s" so its next issue is slot 10's next generation, reviving no handle" T-LABEL
   TABLE-A @ RT-HANDLE:ISSUE drop
   10 3 >WIRE-HANDLE TABLE-A @ RT-HANDLE:INDEX 0 T=
   [: H-A @ TABLE-A @ RT-HANDLE:INDEX drop ;] STALE TTHROWSQ ;

\ TABLE-B owns slots 12 and 13, beside TABLE-A's.
: FOREIGN-HANDLES ( -- )
   0 SLOTS-B 2 12 0 HEADER-B RT-HANDLE:OPEN TABLE-B !
   TABLE-B @ RT-HANDLE:ISSUE H-D !
   s" a handle from a table below the range is foreign" T-LABEL
   [: H-B @ TABLE-B @ RT-HANDLE:INDEX drop ;] FOREIGN TTHROWSQ
   [: H-B @ TABLE-B @ RT-HANDLE:RELEASE ;] FOREIGN TTHROWSQ
   s" and left live in its own table" T-LABEL
   H-B @ TABLE-A @ RT-HANDLE:INDEX 1 T=
   s" a handle from a table above the range is foreign" T-LABEL
   [: H-D @ TABLE-A @ RT-HANDLE:INDEX drop ;] FOREIGN TTHROWSQ
   s" the null handle is foreign to every table" T-LABEL
   [: RT-HANDLE:NULL TABLE-A @ RT-HANDLE:INDEX drop ;] FOREIGN TTHROWSQ
   s" a slot of the table never issued is stale" T-LABEL
   [: 13 1 >WIRE-HANDLE TABLE-B @ RT-HANDLE:INDEX drop ;] STALE TTHROWSQ ;

\ The generations a slot runs through are driven to its last by writing the
\ free slot's cell, the caller's storage: generation 2^32 - 2 in its low half
\ and no next free slot.
$FFFFFFFE constant NEXT-TO-LAST

\ TABLE-C owns slot 20 alone.
: EXHAUSTION ( -- )
   0 SLOTS-C 1 20 0 HEADER-C RT-HANDLE:OPEN TABLE-C !
   TABLE-C @ RT-HANDLE:ISSUE TABLE-C @ RT-HANDLE:RELEASE
   NEXT-TO-LAST 0 SLOTS-C !
   TABLE-C @ RT-HANDLE:ISSUE H-E !
   s" a slot's last generation is 2^32 - 1" T-LABEL
   20 $FFFFFFFF >WIRE-HANDLE TABLE-C @ RT-HANDLE:INDEX 0 T=
   H-E @ TABLE-C @ RT-HANDLE:RELEASE
   s" releasing it retires the slot instead of wrapping" T-LABEL
   [: TABLE-C @ RT-HANDLE:ISSUE drop ;] RT-HANDLE:E-RT-HANDLE-FULL TTHROWSQ
   s" and its last handle is stale" T-LABEL
   [: H-E @ TABLE-C @ RT-HANDLE:INDEX drop ;] STALE TTHROWSQ
   [: 20 1 >WIRE-HANDLE TABLE-C @ RT-HANDLE:INDEX drop ;] STALE TTHROWSQ ;

\ TABLE-D owns slots 30 and 31: the first retires while the second is live.
: RETIRED-BESIDE-LIVE ( -- )
   0 SLOTS-D 2 30 0 HEADER-D RT-HANDLE:OPEN TABLE-D !
   TABLE-D @ RT-HANDLE:ISSUE H-A !
   TABLE-D @ RT-HANDLE:ISSUE H-B !
   H-A @ TABLE-D @ RT-HANDLE:RELEASE
   NEXT-TO-LAST 0 SLOTS-D !
   TABLE-D @ RT-HANDLE:ISSUE TABLE-D @ RT-HANDLE:RELEASE
   s" a retired slot beside a live one leaves the table full" T-LABEL
   [: TABLE-D @ RT-HANDLE:ISSUE drop ;] RT-HANDLE:E-RT-HANDLE-FULL TTHROWSQ
   H-B @ TABLE-D @ RT-HANDLE:RELEASE
   s" and the live one's slot is issued again, the retired one never" T-LABEL
   TABLE-D @ RT-HANDLE:ISSUE TABLE-D @ RT-HANDLE:INDEX 1 T=
   31 2 >WIRE-HANDLE TABLE-D @ RT-HANDLE:INDEX 1 T= ;

\ Every refused OPEN leaves HEADER-E zeroed, so the last one opens it.
: OPEN-RANGES ( -- )
   s" a table needs a slot, from slot 1, within 2^32 - 1" T-LABEL
   [: 0 SLOTS-E 0 1 0 HEADER-E RT-HANDLE:OPEN drop ;] RANGE TTHROWSQ
   [: 0 SLOTS-E 1 0 0 HEADER-E RT-HANDLE:OPEN drop ;] RANGE TTHROWSQ
   [: 0 SLOTS-E 2 $FFFFFFFF 0 HEADER-E RT-HANDLE:OPEN drop ;] RANGE TTHROWSQ
   [: 0 SLOTS-E 1 $100000000 0 HEADER-E RT-HANDLE:OPEN drop ;] RANGE TTHROWSQ
   s" however far past it the range starts" T-LABEL
   [: 0 SLOTS-E 2 $7FFFFFFFFFFFFFFF 0 HEADER-E RT-HANDLE:OPEN drop ;] RANGE TTHROWSQ
   s" no more than 2^31 - 1 slots, over storage" T-LABEL
   [: 0 SLOTS-E $80000000 1 0 HEADER-E RT-HANDLE:OPEN drop ;] RANGE TTHROWSQ
   [: NULL-PTR 1 1 0 HEADER-E RT-HANDLE:OPEN drop ;] RANGE TTHROWSQ
   [: 0 SLOTS-E 1 1 NULL-PTR RT-HANDLE:OPEN drop ;] RANGE TTHROWSQ
   s" slot 2^32 - 1 is a table's last slot" T-LABEL
   0 SLOTS-E 1 $FFFFFFFF 0 HEADER-E RT-HANDLE:OPEN TABLE-E !
   TABLE-E @ RT-HANDLE:ISSUE TABLE-E @ RT-HANDLE:INDEX 0 T=
   $FFFFFFFF 1 >WIRE-HANDLE TABLE-E @ RT-HANDLE:INDEX 0 T= ;

\ TABLE-Z is never opened: it holds the zero token of its zeroed image.
: UNOPENED ( -- )
   s" a table cell OPEN has not filled is refused before it is read" T-LABEL
   [: TABLE-Z @ RT-HANDLE:ISSUE drop ;] RT-HANDLE:E-RT-HANDLE-UNOPENED TTHROWSQ
   [: RT-HANDLE:NULL TABLE-Z @ RT-HANDLE:INDEX drop ;] RT-HANDLE:E-RT-HANDLE-UNOPENED TTHROWSQ
   [: RT-HANDLE:NULL TABLE-Z @ RT-HANDLE:RELEASE ;] RT-HANDLE:E-RT-HANDLE-UNOPENED TTHROWSQ ;

\ ---- routes from a number ---------------------------------------------------

4096 BUFFER: DIAG

\ The checker refuses the candidate, and its diagnostic names the types.
: REFUSED ( ptr u8 n ptr u8 n -- )
   {: src:ptr srcu:n want:ptr wantu:n :}
   DIAG 4096 DIAG-BUFFER!
   src srcu CHECK-CANDIDATE! 0 T=
   DIAG-BUFFER$ want wantu CONTAINS? TTRUE
   DIAG-BUFFER-OFF ;

: CERTIFIED ( ptr u8 n -- )
   CHECK-QUIET-CANDIDATE! -1 T= ;

: CHECKER ( -- )
   s" an Id128 where a handle is expected is refused" T-LABEL
   s" RIDT-ID-AS-HANDLE ( RT-ID:id128 RT-HANDLE:table -- n ) RT-HANDLE:INDEX"
   s" expected: rt-handle:handle rt-handle:table actual: rt-id:id128 rt-handle:table" REFUSED
   s" while a handle there certifies" T-LABEL
   s" RIDT-HANDLE-AS-HANDLE ( RT-HANDLE:handle RT-HANDLE:table -- n ) RT-HANDLE:INDEX" CERTIFIED
   s" a number where a handle is expected is refused" T-LABEL
   s" RIDT-N-AS-HANDLE ( n RT-HANDLE:table -- n ) RT-HANDLE:INDEX"
   s" actual: n rt-handle:table" REFUSED
   s" a handle where an Id128 is expected is refused" T-LABEL
   s" RIDT-HANDLE-AS-ID ( RT-HANDLE:handle -- bool ) RT-ID:ZERO?"
   s" expected: rt-id:id128 actual: rt-handle:handle" REFUSED
   s" an Id128's MAKE takes no numbers and its UNMAKE gives none" T-LABEL
   s" RIDT-MAKE-ID ( n n -- RT-ID:id128 ) RT--ID-ID128:MAKE"
   s" actual: n n" REFUSED
   s" RIDT-UNMAKE-ID ( RT-ID:id128 -- n n ) RT--ID-ID128:UNMAKE"
   s" expected: n n" REFUSED ;

\ A cast is declared at top level, where the text runs.
: CASTS ( -- )
   s" no cast turns a number into a handle or a table outside its package" T-LABEL
   s" CAST: RIDT-FORGE-HANDLE ( n -- RT-HANDLE:handle )" TEST-EVAL:RC E-CAST-OWNER T=
   s" CAST: RIDT-FORGE-TABLE ( n -- RT-HANDLE:table )" TEST-EVAL:RC E-CAST-OWNER T=
   s" no cast turns a number or cells into a counter outside its package" T-LABEL
   s" CAST: RIDT-FORGE-COUNTER ( n -- RT-ID:counter )" TEST-EVAL:RC E-CAST-OWNER T=
   s" CAST: RIDT-COUNTER-AT ( ptr n -- RT-ID:counter )" TEST-EVAL:RC E-CAST-OWNER T=
   s" no cast turns a number into an Id128 or a half, or one back" T-LABEL
   s" CAST: RIDT-FORGE-ID ( n -- RT-ID:id128 )" TEST-EVAL:RC E-CAST-ARITY T=
   s" CAST: RIDT-READ-ID ( RT-ID:id128 -- n )" TEST-EVAL:RC E-CAST-ARITY T=
   s" CAST: RIDT-FORGE-HALF ( n -- RT-ID:id-half )" TEST-EVAL:RC E-CAST-FAM T=
   s" CAST: RIDT-READ-HALF ( RT-ID:id-half -- n )" TEST-EVAL:RC E-CAST-FAM T= ;

$400 constant CHILD-CAP
10000 constant CHILD-MS
CHILD-CAP BUFFER: CHILD-OUT
CHILD-CAP BUFFER: CHILD-ERR

\ A forked child of this image loads the text and exits 70, having named the
\ word undefined.
: UNDEFINED ( ptr u8 n ptr u8 n -- )
   {: src:ptr srcu:n want:ptr wantu:n :}
   src srcu CHILD-OUT CHILD-CAP >LEN CHILD-ERR CHILD-CAP >LEN CHILD-MS >MS SUBJECT:RUN
   {: outu:len erru:len oc :}
   src srcu CHILD-OUT outu LEN>N CHILD-ERR erru LEN>N oc CHECKER-REJECT-RC T-OUTCOME-EXITED=
   CHILD-ERR erru LEN>N want wantu CONTAINS? TTRUE ;

: CONVERTERS ( -- )
   s" an Id128's converters are undefined outside RT-ID" T-LABEL
   s" : RIDT-U1 ( n n -- RT-ID:id128 ) RT-ID:>ID128 ;" s" E-UNDEFINED: RT-ID:>ID128" UNDEFINED
   s" : RIDT-U2 ( RT-ID:id128 -- n n ) RT-ID:ID128>N ;" s" E-UNDEFINED: RT-ID:ID128>N" UNDEFINED
   s" : RIDT-U3 ( n -- n ) RT-ID:>ID-HALF ;" s" E-UNDEFINED: RT-ID:>ID-HALF" UNDEFINED
   s" a counter's outside RT-ID" T-LABEL
   s" : RIDT-U4 ( ptr n -- RT-ID:counter ) RT-ID:>COUNTER ;" s" E-UNDEFINED: RT-ID:>COUNTER" UNDEFINED
   s" : RIDT-U5 ( RT-ID:counter -- n ) RT-ID:COUNTER>N ;" s" E-UNDEFINED: RT-ID:COUNTER>N" UNDEFINED
   s" a handle's and a table's outside RT-HANDLE" T-LABEL
   s" : RIDT-U6 ( n -- RT-HANDLE:handle ) RT-HANDLE:>HANDLE ;" s" E-UNDEFINED: RT-HANDLE:>HANDLE" UNDEFINED
   s" : RIDT-U7 ( RT-HANDLE:handle -- n ) RT-HANDLE:HANDLE>N ;" s" E-UNDEFINED: RT-HANDLE:HANDLE>N" UNDEFINED
   s" : RIDT-U8 ( ptr n -- RT-HANDLE:table ) RT-HANDLE:>TABLE ;" s" E-UNDEFINED: RT-HANDLE:>TABLE" UNDEFINED
   s" no constructor builds a table's header outside RT-HANDLE" T-LABEL
   s" : RIDT-U9 ( ptr n -- RT-HANDLE:table ) 0 1 0 0 0 RT--HANDLE-TABLE:MAKE ;" s" E-UNDEFINED: RT--HANDLE-TABLE:MAKE" UNDEFINED
   s" : RIDT-U10 ( ptr n -- ) 0 1 0 0 RT--HANDLE-HEADER:MAKE drop ;" s" E-UNDEFINED: RT--HANDLE-HEADER:MAKE" UNDEFINED
   s" nor a counter's state outside RT-ID" T-LABEL
   s" : RIDT-U11 ( -- ) 0 1 RT--ID-STATE:MAKE drop ;" s" E-UNDEFINED: RT--ID-STATE:MAKE" UNDEFINED ;

: MAIN ( -- )
   T-RESET
   ORDER
   EQUALITY
   ID-SPANS
   ROUND-TRIP
   FRESH-COUNTER
   REWIND
   CLOSED
   UNMADE
   WIRE-FORMS
   ISSUE-AND-RELEASE
   REOPEN
   FOREIGN-HANDLES
   EXHAUSTION
   RETIRED-BESIDE-LIVE
   OPEN-RANGES
   UNOPENED
   CHECKER
   CASTS
   CONVERTERS ;

MAIN

;package

\ A counter's last values: white-box, since only RT-ID writes a counter's state.
package RT-ID

COUNTER-CELLS TYPED-BUFFER RIDT-CELLS n
TYPED-VARIABLE RIDT-C counter

: RIDT-LAST! ( n -- )
   RIDT-C @ STATE-OF STATE-LAST ! ;

: RIDT-EXHAUSTION ( -- )
   0 RIDT-CELLS COUNTER RIDT-C !
   s" past the largest signed cell a counter goes on, unsigned" T-LABEL
   $7FFFFFFFFFFFFFFF RIDT-LAST!
   RIDT-C @ NEXT $8000000000000000 T=
   s" its last value is 2^64 - 1" T-LABEL
   -2 RIDT-LAST!
   RIDT-C @ NEXT -1 T=
   s" then it refuses instead of wrapping, each time it is asked" T-LABEL
   [: RIDT-C @ NEXT drop ;] E-RT-ID-EXHAUSTED TTHROWSQ
   [: RIDT-C @ NEXT drop ;] E-RT-ID-EXHAUSTED TTHROWSQ ;

RIDT-EXHAUSTION

;package

T-REPORT
