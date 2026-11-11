\ linker.f - LINKER:WITH opens a build's data space, runs the body in it and
\ closes it on every exit: the space's words and strings live there, and after
\ the close hb's dictionary, code, data and header are as before. One process:
\ the boundary under test is in-process.
\ Each build's checker starts from the primitive rows in the stores CHECK
\ registers, and the close puts them back as hb had them.

require src/lib/test.f
require src/lib/string.f
require src/lib/memory.f
require src/habu/layout.f
require src/habu/boot-cells.f
require src/habu/xref.f
require src/compiler/native/abi.f
require src/compiler/native/shadow.f
require src/compiler/target/model.f
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
\ token cells, the body text's handle, EVALERR and fmt/string scratch.
: SCRATCH? ( n -- bool )
   {: off:n :}
   off BODYLEN-CELL = off DEF-TKA-CELL = or off DEF-TKL-CELL = or
   off TKA-CELL = or off TKL-CELL = or off PENDTKA-CELL = or
   off EVALERR-CELL = or
   off TOKBODY-CELL TOKVSP-CELL CELL + IN? or
   off BODY-TEXT:BASE-CELL BODY-TEXT:BASE-CELL 4 cells + IN? or
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

: BUILD ( [ -- ] -- )
   {: body :}
   s" test" RTARGET:HOST-TARGET body LINKER:WITH ;

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

: INNER ( -- ) s" engine" RTARGET:HOST-TARGET [: ;] LINKER:WITH ;

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

variable RC-1                        \ a build line's throw code
variable RC-2

: M1 ( -- ) s" : LK-M1 ( -- n ) 1 ;" evaluate-closed ;
: M2 ( -- ) s" : LK-M1 ( -- n n ) 1 2 ;" evaluate-closed ;
: BODY-M1 ( -- ) [: M1 ;] catch RC-1 ! ;
: BODY-M2 ( -- ) [: M2 ;] catch RC-2 ! ;

: CASE-M ( -- )
   TAKE-HOST
   -1 RC-1 !  -1 RC-2 !
   [: BODY-M1 ;] BUILD
   [: BODY-M2 ;] BUILD
   HOST-CHECK
   RC-1 @ 0 T=
   RC-2 @ 0 T=
   s" m: two builds may define one name with different effects" SAY ;

: N1 ( -- ) s" : STR= ( n -- n ) ;" evaluate-closed ;
: N2 ( -- ) s" : LK-N1 ( -- n ) 5 STR= ;" evaluate-closed ;

: BODY-N ( -- )
   [: N1 ;] catch RC-1 !
   [: N2 ;] catch RC-2 ! ;

: CASE-N ( -- )
   TAKE-HOST
   -1 RC-1 !  -1 RC-2 !
   [: BODY-N ;] BUILD
   HOST-CHECK
   RC-1 @ 0 T=
   RC-2 @ 0 T=
   s\" : LK-N2 ( -- bool ) s\" a\" s\" a\" STR= ;" TEST-EVAL:RC 0 T=
   s" : LK-N3 ( -- n ) 5 STR= ;" TEST-EVAL:RC CHECKER-REJECT-RC T=
   s" n: a build may redefine an hb word; hb's effect stays" SAY ;

: O1 ( -- )
   s" package WL public : GLOBAL? ( -- n ) 1 ; ;package" evaluate-closed ;

: O2 ( -- ) s" : LK-O1 ( -- n ) WL:GLOBAL? ;" evaluate-closed ;

: BODY-O ( -- )
   [: O1 ;] catch RC-1 !
   [: O2 ;] catch RC-2 ! ;

: CASE-O ( -- )
   TAKE-HOST
   -1 RC-1 !  -1 RC-2 !
   [: BODY-O ;] BUILD
   HOST-CHECK
   RC-1 @ 0 T=
   RC-2 @ 0 T=
   s" : LK-O2 ( -- bool ) 0 WL:GLOBAL? ;" TEST-EVAL:RC 0 T=
   s" : LK-O3 ( -- n ) WL:GLOBAL? ;" TEST-EVAL:RC CHECKER-REJECT-RC T=
   s" o: a build may define a package hb has; hb's stays" SAY ;

: CASE-P ( -- )
   s" : LK-M1 ( -- n n ) 1 2 ;" TEST-EVAL:RC 0 T=
   s" p: after the close hb's checker has none of the builds' names" SAY ;

variable Q0-ORD                      \ LK-Q0's dictionary ordinal
: LK-Q0 ( n -- n ) 1 + ;
ndict@ 1- Q0-ORD !

CAST: CONTRACT-ACTION ( n -- [ n -- ptr u8 n bool ] )

\ hb's checker owner record's contract for the word at an ordinal, read as the
\ verifier calls the owner (test/checker-decl-locs-lib.f SLOT).
: OWNER-CONTRACT ( n -- ptr u8 n bool )
   data-base NCOMP-DISPATCH:DECL-CELL + 0 ptr-field @
   CHECKER-OWNER-ABI:RECORD-CONTRACT-OFF CELL + CHECKER-OWNER-GUARD:VALIDATE
   CHECKER-OWNER-ABI:RECORD-CONTRACT-OFF + CELL-VIEW @ CONTRACT-ACTION
   execute ;

\ Whether it links the word.
: LINKED? ( n -- bool ) OWNER-CONTRACT nip nip ;

: Q1 ( -- ) s" : LK-QA ( -- ) ;" evaluate-closed ;

: BODY-Q ( -- )
   [: Q1 ;] catch RC-1 !
   [: REFUSE ;] catch RC-2 ! ;

: CASE-Q ( -- )
   TAKE-HOST
   -1 RC-1 !  -1 RC-2 !
   [: BODY-Q ;] BUILD
   HOST-CHECK
   RC-1 @ 0 T=
   RC-2 @ CHECKER-REJECT-RC T=
   Q0-ORD @ LINKED? TTRUE
   s" q: a build's definitions leave hb's records linked" SAY ;

public

: RUN ( -- )
   T-RESET
   HEADER-ALLOC
   \ The first build installs the typed loop for good; the host is taken after it.
   [: ;] BUILD
   TAKE-HOST
   Q0-ORD @ LINKED? TTRUE
   CASE-A CASE-B CASE-C CASE-D CASE-E CASE-F CASE-G CASE-H CASE-I CASE-J
   CASE-M CASE-N CASE-O CASE-P CASE-Q ;

;package

LINKER-TEST:RUN

\ Case k runs at top level with LINKER-TEST open, as a caller's build does.
package LINKER-TEST
private

variable PKG-K                       \ PKG-PUB-CELL inside the body
variable RC-K

: BODY-K ( -- )
   s" variable LK-K1  1 LK-K1 !" evaluate-closed
   PKG-PUB-CELL DATA-CELL @ PKG-K ! ;

: CASE-K ( -- )
   TAKE-HOST
   -1 PKG-K !
   [: [: BODY-K ;] BUILD ;] catch RC-K !
   RC-K @ 0 T=
   PKG-K @ 0 T=
   HOST-CHECK
   s" k: a build run inside a host package sees no package open, and the close reopens it" SAY ;

CASE-K

variable RAN-L
\ The package it opens writes 0 into PKG-OUTSIDE-CELL; WITHOUT-PACKAGE restores the caller's.
: OPEN-L ( -- ) [: s" package LK-PX" evaluate-closed ;] INTERPRETER:WITHOUT-PACKAGE ;

: CASE-L ( -- )
   [: [: s" : LK-L1 ( -- ) ;" evaluate-closed ;] INTERPRETER:WITHOUT-PACKAGE ;] catch 0 T=
   s" LK-L1" LOOKUP -1 T<>
   s" package LK-PB ;package" TEST-EVAL:RC 75 T=
   0 RAN-L !
   s" OPEN-L -1 RAN-L ! package LK-PB ;package" TEST-EVAL:RC 75 T=
   RAN-L @ -1 T=
   s" l: WITHOUT-PACKAGE defines at top level and keeps the caller's package outside" SAY ;

CASE-L

variable R1-ORD                      \ LK-R1's dictionary ordinal
\ The grid flag is the eighth cell RECORD-CONTRACT-BUILD writes.
7 cells constant CONTRACT-GRID

\ Whether hb's checker owner holds a current contract with a storage grid for
\ the word at an ordinal.
: RECORD-GRID? ( n -- bool )
   OWNER-CONTRACT {: p:ptr u:n ok:bool :}
   ok 0= if false exit then
   p BYTE-VIEW CONTRACT-GRID + CELL-VIEW @ 0 <> ;

: BODY-R ( -- ) s" create LK-RB 1 ," evaluate-closed ;

: BUILD-R ( -- )
   TAKE-HOST
   [: BODY-R ;] BUILD
   HOST-CHECK ;

: CASE-R ( -- )
   R1-ORD @ RECORD-GRID? TTRUE
   s" r: a created body pending across a build keeps its grid" SAY ;

\ Case r: LK-R1's body pends across a build that creates its own, as a build
\ driver's top level does, and LK-R2 then closes it.
create LK-R1 7 , 8 ,
ndict@ 1- R1-ORD !
BUILD-R
create LK-R2
CASE-R

TYPED-VARIABLE SEALED-S bool

: S$ ( -- ptr u8 n ) s" LK-S" ;

\ LK-S's row in the namespace wordlist BOOT:NAMESPACE-WL names, the build's
\ inside a body.
: S-ROW ( -- ptr n ) S$ BOOT:NAMESPACE-WL XREF-FIND-WL ;

: BODY-S ( -- )
   s" package LK-S seal-package ;package" evaluate-closed
   S-ROW {: row:ptr :}
   row XREF-FOUND? 0= if exit then
   row XREF:SEALED? SEALED-S ! ;

: CASE-S ( -- )
   TAKE-HOST
   false SEALED-S !
   [: BODY-S ;] BUILD
   SEALED-S @ TTRUE
   S-ROW XREF-FOUND? TFALSE
   HOST-CHECK
   s" s: seal-package in a build marks its package's record sealed" SAY ;

CASE-S

;package

T-REPORT
