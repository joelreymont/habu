\ linker.f - LINKER:WITH opens a build's data space, runs the body in it and
\ closes it on every exit: the space's words and strings live there, and after
\ the close hb's dictionary, code, data and header are as before. One process:
\ the boundary under test is in-process.
\ Every build defines words of its own names: the host checker keeps a symbol
\ for each, so a second build of one name is E-DUPLICATE-DEFINITION, 78.

require src/lib/test.f
require src/lib/string.f
require src/lib/memory.f
require src/habu/layout.f
require src/habu/boot-cells.f
require src/habu/xref.f
require src/compiler/native/abi.f
require src/compiler/native/shadow.f
require src/habu/linker.f
require src/habu/link-band.f

package LINKER-TEST
private

CAST: STRING-XT ( n -- [ -- ptr u8 n ] )

77 constant BODY-CODE                \ the code case e's body throws

\ ---- the host before a build ---------------------------------------------------
variable ND0
variable CP0
variable HERE0
variable DPHW0
variable HEADER0                     \ hb's DATA header, its distance from NULL-PTR

: DATA-CELL ( n -- ptr n )
   {: off:n :}
   data-base BYTE-VIEW off + CELL-VIEW ;

: COPY-CELL ( n -- ptr n )
   {: off:n :}
   NULL-PTR BYTE-VIEW HEADER0 @ + off + CELL-VIEW ;

: HERE-N ( -- n ) here NULL-PTR - ;

: HEADER-ALLOC ( -- )
   DATA-START MEM:BYTES-ALLOC-LEN MEM:ALLOC-BYTES drop NULL-PTR - HEADER0 ! ;

: TAKE-HOST ( -- )
   ndict@ ND0 !  cp@ CP0 !  HERE-N HERE0 !
   data-base DPHW-CELL + @ DPHW0 !
   DATA-START 0 ?do i DATA-CELL @ i COPY-CELL ! CELL +loop ;

\ Sixteen hex digits, most significant first.
: .HEX ( n -- )
   {: v:n :}
   [char] $ emit
   16 0 ?do
      v 60 i 4 * - rshift 15 and {: d:n :}
      d 10 < if d 48 + else d 55 + then emit
   loop ;

\ off in [lo, hi).
: IN? ( n n n -- bool )
   {: off:n lo:n hi:n :}
   off lo >= off hi < and ;

\ The close's forget keeps NDICT-HW-CELL and HIDX's stale claims (layout.f); a
\ build's wordlists raise WIDN-CELL.
: CLOSE-LEFT? ( n -- bool )
   {: off:n :}
   off WIDN-CELL = off NDICT-HW-CELL = or off HIDX:CLAIMS = or ;

\ Scratch any host definition, evaluation or number printing writes: name and
\ token cells, the body buffer, its source rows, EVALERR and fmt/string scratch.
: SCRATCH? ( n -- bool )
   {: off:n :}
   off BODYLEN-CELL = off DEF-TKA-CELL = or off DEF-TKL-CELL = or
   off TKA-CELL = or off TKL-CELL = or off PENDTKA-CELL = or
   off EVALERR-CELL = or
   off TOKBODY-CELL TOKFRF-CELL CELL + IN? or
   off BODYBUF-OFF BODYBUF-OFF BODYBUF-CAP + IN? or
   off BODY-SOURCE:OFF BODY-SOURCE:END IN? or
   off FMT-ABI:START STRING-ABI:END IN? or ;

: CHANGES? ( n -- bool )
   {: off:n :}
   off CLOSE-LEFT? off SCRATCH? or 0= ;

\ The header cells that differ from the copy, each printed with before and after.
: HEADER-DIFFS ( -- n )
   0
   DATA-START 0 ?do
      i COPY-CELL @ {: was:n :}
      i DATA-CELL @ {: now:n :}
      was now <> i CHANGES? and if
         s" header cell " type i .HEX s"  before " type was .HEX
         s"  after " type now .HEX cr
         1+
      then
   CELL +loop ;

: HOST-CHECK ( -- )
   ndict@ ND0 @ T=
   cp@ CP0 @ T=
   HERE-N HERE0 @ T=
   data-base DPHW-CELL + @ DPHW0 @ T=
   NSHADOW:OPEN? TFALSE
   HEADER-DIFFS 0 T= ;

: BUILD ( [ -- ] -- ) s" test" rot LINKER:WITH ;

: SAY ( ptr u8 n -- ) s" linker: " type type cr ;

\ ---- inside a build ------------------------------------------------------------
variable IMAGE                       \ BOOT:IMAGE-BASE-CELL inside the body
variable FOUND
variable FOUND-WL
variable SPACE-WL
variable SPACE-NS
TYPED-VARIABLE NATIVE bool
variable AT1
variable AT2
variable LEN1
variable IMAGE-C                     \ case c's image base
variable HERE-D
variable AT-D
TYPED-VARIABLE BYTES-OK bool
variable REFUSED
variable REWOUND
variable LOW-DP

: TAKE-IMAGE ( -- ) data-base BOOT:IMAGE-BASE-CELL + @ IMAGE ! ;

: LOOKUP ( ptr u8 n -- n ) XREF-FIND-INDEX ;

\ The address a string word of the space answers, read with its length cell
\ and bytes while the space is mapped.
: CALL$ ( ptr u8 n -- ptr u8 n ) LOOKUP XREF-REC XREF-START STRING-XT execute ;

: LEN-AT ( n -- ptr n ) NULL-PTR BYTE-VIEW swap + CELL - CELL-VIEW ;

\ ---- the cases -----------------------------------------------------------------
: MAIN$ ( -- ptr u8 n ) s" MAIN" ;

: BODY-A ( -- )
   s" : MAIN ( -- ) 72 emit 105 emit cr ;" evaluate-closed
   s" create LK-A1 1 , 2 ," evaluate-closed
   MAIN$ LOOKUP FOUND !
   FOUND @ 0 >= if FOUND @ XREF-REC XREF-WORDLIST FOUND-WL ! then
   BOOT:GLOBAL-WL SPACE-WL !
   BOOT:NAMESPACE-WL SPACE-NS !
   NSHADOW:NATIVE? NATIVE ! ;

: CASE-A ( -- )
   -1 FOUND !  false NATIVE !
   [: BODY-A ;] BUILD
   FOUND @ ND0 @ >= TTRUE
   FOUND-WL @ SPACE-WL @ T=
   SPACE-WL @ BOOT:GLOBAL-WL T<>
   SPACE-NS @ BOOT:NAMESPACE-WL T<>
   NATIVE @ TTRUE
   s" a: MAIN is defined in the space's wordlist under the native shadow" SAY ;

: CASE-B ( -- )
   MAIN$ LOOKUP -1 T=
   HOST-CHECK
   s" b: after the close hb's dictionary, code, data and header are as before" SAY ;

: C1$ ( -- ptr u8 n ) s" LK-C1" ;
: C2$ ( -- ptr u8 n ) s" LK-C2" ;

: BODY-C ( -- )
   s\" : LK-C1 ( -- ptr u8 n ) s\" hello\" ;" evaluate-closed
   s\" : LK-C2 ( -- ptr u8 n ) s\" hello\" ;" evaluate-closed
   TAKE-IMAGE
   C1$ CALL$ drop NULL-PTR - AT1 !
   C2$ CALL$ drop NULL-PTR - AT2 !
   AT1 @ LEN-AT @ LEN1 ! ;

: CASE-C ( -- )
   [: BODY-C ;] BUILD
   IMAGE @ IMAGE-C !
   AT1 @ AT2 @ T=
   AT1 @ IMAGE @ DATA-START + >= TTRUE
   AT1 @ IMAGE @ DATA-SIZE + < TTRUE
   LEN1 @ 5 T=
   HOST-CHECK
   s" c: two words' equal strings share one address in the space" SAY ;

: D1$ ( -- ptr u8 n ) s" LK-D1" ;

\ A decoy where case c's string sat: a table the last build left would find it.
: DECOY ( -- )
   AT1 @ 5 + HERE-N - allot
   5 AT1 @ LEN-AT !
   s" hello" drop  NULL-PTR BYTE-VIEW AT1 @ +  5 BYTE-COPY ;

: BODY-D ( -- )
   TAKE-IMAGE
   IMAGE @ IMAGE-C @ = if DECOY then
   HERE-N HERE-D !
   s\" : LK-D1 ( -- ptr u8 n ) s\" hello\" ;" evaluate-closed
   D1$ CALL$ s" hello" STR= BYTES-OK !
   D1$ CALL$ drop NULL-PTR - {: at:n :}
   at AT-D !
   at LEN-AT @ LEN1 ! ;

: CASE-D ( -- )
   false BYTES-OK !  0 AT-D !
   [: BODY-D ;] BUILD
   IMAGE @ IMAGE-C @ T=
   AT-D @ HERE-D @ >= TTRUE
   BYTES-OK @ TTRUE
   LEN1 @ 5 T=
   HOST-CHECK
   s" d: a second space at the first one's address gets a string of its own" SAY ;

: BODY-E ( -- )
   s" : LK-E1 ( -- ) ;" evaluate-closed
   BODY-CODE throw ;

: CASE-E ( -- )
   [: [: BODY-E ;] BUILD ;] BODY-CODE TTHROWSQ
   HOST-CHECK
   s" e: a throw in the body is rethrown after the close" SAY ;

: REFUSE ( -- ) s\" : LK-F1 ( -- n ) s\" gone\" ;" evaluate-closed ;
: REWIND ( -- ) s\" : LK-F2 ( -- ptr u8 n ) s\" new\" ;" evaluate-closed ;

: BODY-F ( -- )
   TAKE-IMAGE
   [: REFUSE ;] catch REFUSED !
   HERE-N LOW-DP !
   [: REWIND ;] catch REWOUND ! ;

: CASE-F ( -- )
   0 REFUSED !  0 REWOUND !
   [: BODY-F ;] BUILD
   REFUSED @ CHECKER-REJECT-RC T=
   REWOUND @ E-INTERN-REWIND T=
   LOW-DP @ IMAGE @ DATA-START + >= TTRUE
   HOST-CHECK
   s" f: after a rollback in the space a new string is refused" SAY ;

: G1$ ( -- ptr u8 n ) s" LK-G1" ;

: INNER ( -- ) [: ;] s" engine" rot LINKER:WITH ;

: BODY-G ( -- )
   [: INNER ;] E-COMP-STATE TTHROWSQ
   s" : LK-G1 ( -- ) ;" evaluate-closed
   G1$ LOOKUP FOUND ! ;

: CASE-G ( -- )
   -1 FOUND !
   [: BODY-G ;] BUILD
   FOUND @ ND0 @ >= TTRUE
   G1$ LOOKUP -1 T=
   HOST-CHECK
   s" g: a build inside a build is refused and the outer one goes on" SAY ;

: CASE-H ( -- )
   NABI:BINDING NSHADOW:OPEN-NATIVE
   [: [: ;] BUILD ;] E-NSHADOW-STATE TTHROWSQ
   NSHADOW:OPEN? TTRUE
   NSHADOW:CLOSE
   HOST-CHECK
   s" h: a refused open step leaves the caller's shadow open" SAY ;

variable HOST-AT
\ hb's last created word when the builds run: a does> patches LASTC's record.
create LK-HOST 7 ,

: HOST-ADDR ( -- n ) ['] LK-HOST execute NULL-PTR - ;

: BODY-I ( -- )
   s" : LK-I1 ( -- ) does> ( -- n ) @ ;" evaluate-closed
   [: s" LK-I1" evaluate-closed ;] catch REFUSED ! ;

: CASE-I ( -- )
   0 REFUSED !
   HOST-ADDR HOST-AT !
   [: BODY-I ;] BUILD
   REFUSED @ CHECKER-REJECT-RC T=
   HOST-ADDR HOST-AT @ T=
   HOST-CHECK
   s" i: a does> before the space's first create patches no word of hb's" SAY ;

variable FLAGS0

: NEWEST-FLAGS ( -- n ) ndict@ 1- XREF-REC XREF-FLAGS ;

: BODY-J ( -- ) s" immediate" evaluate-closed ;

: CASE-J ( -- )
   NEWEST-FLAGS FLAGS0 !
   [: BODY-J ;] BUILD
   NEWEST-FLAGS FLAGS0 @ T=
   HOST-CHECK
   s" j: the close clears an immediate a build set on hb's newest word" SAY ;

public

: RUN ( -- )
   T-RESET
   HEADER-ALLOC
   \ The first build installs the typed loop for good; the host is taken after it.
   [: ;] BUILD
   TAKE-HOST
   CASE-A CASE-B CASE-C CASE-D CASE-E CASE-F CASE-G CASE-H CASE-I CASE-J
   T-REPORT ;

;package

LINKER-TEST:RUN
