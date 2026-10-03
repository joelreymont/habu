\ room-left-test.f - append guards that measure a length against the room left.
\
\ DTC+ and VRDEF-APP (src/core/roles.f) are engine words any source file can
\ call, and AOT-CAPTURE:BOOTRUN+ (src/habu/aot-capture.f) and AOT-IDENT:PATH+
\ (src/habu/aot-ident.f) are public entries of the capture and its closure
\ table, so each takes its length from the caller. A guard that adds that length
\ to what the buffer already holds lets a length near the maximum cell wrap the
\ sum back under the capacity, and one that only bounds it from above lets a
\ negative one through. Each is driven at the exact fill, one past it, -1 and
\ the maximum cell. A refusal ends the process, so the refused cases run in a
\ forked child.
\
\ CHECKER-DEFRECORD and CHECKER-DEFER (src/core/checker.f) are checker words any
\ source can call with a name. The checker's string pools grow, so a name of
\ exactly the room left and one past it are both stored. A name is at most the
\ ceiling `:` defines, BODYBUF-CAP - 1 bytes, so each pool is first filled with
\ shorter names until its room is under that; test/name-length-test.f drives the
\ lengths no name has.
\
\ Run: bin/hb --load test/room-left-test.f

require lib/errors.f
require lib/memory.f
require lib/string.f
require lib/test.f
require lib/test/outcome.f
require lib/test/subject.f
require src/arch/arm64/asm.f
require src/arch/arm64/icode.f
require src/habu/layout.f
require src/habu/aot-decl.f
require src/habu/aot-arm.f
require src/habu/aot-capture.f
require src/habu/aot-ident.f

using AOT-BUF
package ROOM-LEFT-TEST

$400 constant CAPTURE-CAP
10000 constant TIMEOUT-MS
-1 1 rshift constant MAX-CELL
BODYBUF-CAP 1 - constant CEIL      \ the longest name `:` defines
VRDEF-CAP constant SRC-CAP         \ the largest buffer filled from SRC
create SRC SRC-CAP allot
create OUT CAPTURE-CAP allot
create ERR CAPTURE-CAP allot

\ die writes its message as one line, so every want below ends in one newline.
: DIES ( ptr u8 n n ptr u8 n -- ) {: source:ptr sourceu:n rc:n want:ptr wantu:n :}
   source sourceu OUT CAPTURE-CAP >LEN ERR CAPTURE-CAP >LEN TIMEOUT-MS >MS
   SUBJECT:RUN {: outu:len erru:len oc :}
   source sourceu OUT outu LEN>N ERR erru LEN>N oc rc T-OUTCOME-EXITED=
   outu LEN>N 0 T=
   ERR erru LEN>N want wantu T$= ;

: DTC-FILL ( -- ) DTC-BEGIN SRC DTC-CAP DTC+ ;
: DTC-OVER ( -- ) DTC-BEGIN SRC DTC-CAP 1- DTC+ SRC 2 DTC+ ;
: DTC-NEG ( -- ) DTC-BEGIN SRC 1 DTC+ SRC -1 DTC+ ;
: DTC-MAX ( -- ) DTC-BEGIN SRC 1 DTC+ SRC MAX-CELL DTC+ ;

: TEST-DTC ( -- )
   s" converter text fills its buffer exactly" T-LABEL
   DTC-FILL DTC-NAME-END DTC-NAME$ nip DTC-CAP T=
   s" converter text one past, -1 and the maximum cell are refused" T-LABEL
   s" DTC-OVER" 70 S\" nominal: converter text too long\n" DIES
   s" DTC-NEG" 70 S\" nominal: converter text too long\n" DIES
   s" DTC-MAX" 70 S\" nominal: converter text too long\n" DIES ;

: VRDEF-FILL ( -- ) VRDEF-CLEAR SRC VRDEF-CAP VRDEF-APP ;
: VRDEF-OVER ( -- ) VRDEF-CLEAR SRC VRDEF-CAP 1- VRDEF-APP SRC 2 VRDEF-APP ;
: VRDEF-NEG ( -- ) VRDEF-CLEAR SRC 1 VRDEF-APP SRC -1 VRDEF-APP ;
: VRDEF-MAX ( -- ) VRDEF-CLEAR SRC 1 VRDEF-APP SRC MAX-CELL VRDEF-APP ;

: TEST-VRDEF ( -- )
   s" a value-record field list fills its buffer exactly" T-LABEL
   VRDEF-FILL VRDEF-U @ VRDEF-CAP T=
   s" a field list one past, -1 and the maximum cell are refused" T-LABEL
   s" VRDEF-OVER" 70 S\" value-record: field list too long\n" DIES
   s" VRDEF-NEG" 70 S\" value-record: field list too long\n" DIES
   s" VRDEF-MAX" 70 S\" value-record: field list too long\n" DIES ;

\ Rows of 255 bytes, then the one that ends exactly at the cap: a row takes its
\ length byte and its bytes, and the live terminator takes one more.
: BOOTRUN-FILL ( -- )
   0 AOT-BOOTRUN-LEN !
   begin AOT-BOOTRUN-LEN @ 257 + AOT-BOOTRUN-CAP <= while
      SRC 255 AOT-CAPTURE:BOOTRUN+
   repeat
   SRC AOT-BOOTRUN-CAP 2 - AOT-BOOTRUN-LEN @ - AOT-CAPTURE:BOOTRUN+ ;
: BOOTRUN-OVER ( -- ) BOOTRUN-FILL SRC 0 AOT-CAPTURE:BOOTRUN+ ;
: BOOTRUN-NEG ( -- ) 0 AOT-BOOTRUN-LEN ! SRC -1 AOT-CAPTURE:BOOTRUN+ ;
: BOOTRUN-MAX ( -- ) 0 AOT-BOOTRUN-LEN ! SRC MAX-CELL AOT-CAPTURE:BOOTRUN+ ;

: TEST-BOOTRUN ( -- )
   s" boot-run rows fill the list exactly" T-LABEL
   BOOTRUN-FILL AOT-BOOTRUN-LEN @ AOT-BOOTRUN-CAP 1- T=
   0 AOT-BOOTRUN-LEN !
   s" a boot-run row one past the list or of negative length is refused" T-LABEL
   s" BOOTRUN-OVER" 74 S\" aot-capture: boot-run overflow\n" DIES
   s" BOOTRUN-NEG" 74 S\" aot-capture: boot-run overflow\n" DIES
   s" a boot-run row of the maximum cell is too long" T-LABEL
   s" BOOTRUN-MAX" 74 S\" aot-capture: boot-run name too long\n" DIES ;

\ A closure path is copied whole into a slot of PATH-CAP bytes.
: PATH-FILL ( -- ) AOT-IDENT:RESET SRC PATH-CAP AOT-IDENT:PATH+ ;
: PATH-OVER ( -- ) AOT-IDENT:RESET SRC PATH-CAP 1+ AOT-IDENT:PATH+ ;
: PATH-NEG ( -- ) AOT-IDENT:RESET SRC -1 AOT-IDENT:PATH+ ;
: PATH-MAX ( -- ) AOT-IDENT:RESET SRC MAX-CELL AOT-IDENT:PATH+ ;

: TEST-PATH ( -- )
   s" a closure path fills its slot exactly" T-LABEL
   PATH-FILL 0 AOT-IDENT:PATH$ nip PATH-CAP T=
   s" a closure path one past, -1 and the maximum cell are refused" T-LABEL
   s" PATH-OVER" 74 S\" aot-ident: closure path longer than the path cap\n" DIES
   s" PATH-NEG" 74 S\" aot-ident: closure path longer than the path cap\n" DIES
   s" PATH-MAX" 74 S\" aot-ident: closure path longer than the path cap\n" DIES ;

\ A name of `u` bytes of byte c in a fresh mapping: one letter repeated names no
\ type or word the checker knows, and two of them differ by length alone.
: LETTERS ( n n -- ptr u8 n )
   {: u:n c:n :}
   u MEM-ALLOC-BYTES drop {: p:ptr :}
   u 0 ?do c p i + c! loop
   p u ;

\ The names that fill a pool: r, each one byte shorter than the last, so no two
\ are the same name.
variable FILL-U
CEIL FILL-U !
: FILLER ( -- ptr u8 n )
   FILL-U @ 114 LETTERS
   FILL-U @ 1 - FILL-U ! ;

: VREC-ROOM ( -- n ) VREC-STR-CAP-V @ VREC-STR-U @ - ;

\ Declare records until the value-record pool has at most `n` bytes of room.
: VREC-FILL ( n -- )
   {: want:n :}
   begin VREC-ROOM want > while FILLER s" f n" CHECKER-DEFRECORD repeat ;

\ Define a record whose name is `u` bytes of c: the checker knows it afterwards.
: REC-NAMED ( n n -- )
   LETTERS {: a:ptr au:n :}
   a au s" f n" CHECKER-DEFRECORD
   a au TYPE-RESERVED? TTRUE ;

\ The value-record pool takes the record's name first, so a name of exactly
\ the room left fills it before the field's copy of the name grows it.
: TEST-TYPE-NAME ( -- )
   s" a record name one past the value-record pool's room is stored" T-LABEL
   CEIL 1 - VREC-FILL
   VREC-ROOM 1+ 113 REC-NAMED
   s" a record name of exactly the pool's room is stored" T-LABEL
   CEIL VREC-FILL
   VREC-ROOM 112 REC-NAMED ;

: SYM-ROOM ( -- n ) SYM-STR-CAP-V @ SYM-STR-U @ - ;

\ Defer names until the symbol pool has at most `n` bytes of room.
: SYM-FILL ( n -- )
   {: want:n :}
   begin SYM-ROOM want > while FILLER CHECKER-DEFER repeat ;

\ A deferred name lands in the symbol pool alone, so the pool's own fill shows.
: SYM-PAST ( -- )
   CEIL 1 - SYM-FILL
   SYM-STR-CAP-V @ {: cap:n :}
   SYM-ROOM 1+ 113 LETTERS CHECKER-DEFER
   SYM-STR-U @ cap 1+ T=
   SYM-STR-CAP-V @ cap > TTRUE ;

: SYM-EXACT ( -- )
   CEIL SYM-FILL
   SYM-STR-CAP-V @ {: cap:n :}
   SYM-ROOM 112 LETTERS CHECKER-DEFER
   SYM-STR-U @ cap T=
   SYM-STR-CAP-V @ cap T= ;

: TEST-SYM ( -- )
   s" a deferred name one past the symbol pool's room grows the pool" T-LABEL
   SYM-PAST
   s" a deferred name of exactly the pool's room fills it" T-LABEL
   SYM-EXACT ;

: MAIN ( -- )
   T-RESET
   TEST-DTC
   TEST-VRDEF
   TEST-BOOTRUN
   TEST-PATH
   TEST-TYPE-NAME
   TEST-SYM
   T-REPORT
   s" room-left-test: ok" type cr ;

MAIN

;package
;using
