\ linker.f - LINKER:WITH opens a build's data space, runs the body in it and
\ closes it on every exit: the space's words and strings live there, and after
\ the close hb's dictionary, code and data are as before. One process:
\ the boundary under test is in-process.
\ Each build's checker starts from the primitive rows in the stores CHECK
\ registers, and the close puts them back as hb had them.

require src/lib/test.f
require src/lib/string.f
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

CAST: STRING-XT ( n -- [ -- str ] )

77 constant BODY-CODE

variable ND0
variable CP0
variable HERE0

: HERE-N ( -- n ) here NULL-PTR - ;

: TAKE-HOST ( -- )
   ndict@ ND0 !  cp@ CP0 !  HERE-N HERE0 ! ;

: HOST-CHECK ( -- )
   ndict@ ND0 @ T=
   cp@ CP0 @ T=
   HERE-N HERE0 @ T=
   NSHADOW:OPEN? TFALSE ;

: BUILD ( [ -- renders nominal roots ] -- )
   {: body :}
   s" test" RTARGET:HOST-TARGET body LINKER:WITH ;

: SAY ( str -- ) s" linker: " type type cr ;

variable FOUND
variable FOUND-WL
variable SPACE-WL
variable SPACE-NS
TYPED-VARIABLE NATIVE bool
variable AT1
variable AT2
variable LEN1
variable HERE-D
variable AT-D
TYPED-VARIABLE BYTES-OK bool
variable REFUSED
variable REWOUND

\ The address a string word of the space answers, read with its length cell
\ and bytes while the space is mapped.
: CALL$ ( str -- str ) XREF-FIND XREF-START STRING-XT execute ;

: LEN-AT ( n -- ^n ) NULL-PTR BYTE-VIEW swap + CELL - CELL-VIEW ;

: MAIN$ ( -- str ) s" MAIN" ;

: BODY-A ( -- )
   s" : MAIN ( -- ) 72 emit 105 emit cr ;" evaluate-closed
   s" create LK-A1 1 , 2 ," evaluate-closed
   MAIN$ XREF-FIND-CURRENT-INDEX FOUND !
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
   MAIN$ XREF-FIND XREF-FOUND? TFALSE
   HOST-CHECK
   s" b: after the close hb's dictionary, code and data are as before" SAY ;

: C1$ ( -- str ) s" LK-C1" ;
: C2$ ( -- str ) s" LK-C2" ;

: BODY-C ( -- )
   s\" : LK-C1 ( -- str ) s\" hello\" ;" evaluate-closed
   s\" : LK-C2 ( -- str ) s\" hello\" ;" evaluate-closed
   C1$ CALL$ STR:DATA NULL-PTR - AT1 !
   C2$ CALL$ STR:DATA NULL-PTR - AT2 !
   AT1 @ LEN-AT @ LEN1 ! ;

: CASE-C ( -- )
   [: BODY-C ;] BUILD
   AT1 @ AT2 @ T=
   LEN1 @ 5 T=
   HOST-CHECK
   s" c: two words' equal strings share one address in the space" SAY ;

: D1$ ( -- str ) s" LK-D1" ;

\ A decoy where case c's string sat, if this space has not passed it: a table
\ the last build left would find it.
: DECOY ( -- )
   AT1 @ 5 + HERE-N - allot
   5 AT1 @ LEN-AT !
   s" hello" STR:DATA  NULL-PTR BYTE-VIEW AT1 @ +  5 BYTE-COPY ;

: BODY-D ( -- )
   AT1 @ CELL - HERE-N >= if DECOY then
   HERE-N HERE-D !
   s\" : LK-D1 ( -- str ) s\" hello\" ;" evaluate-closed
   D1$ CALL$ s" hello" STR:= BYTES-OK !
   D1$ CALL$ STR:DATA NULL-PTR - {: at:n :}
   at AT-D !
   at LEN-AT @ LEN1 ! ;

: CASE-D ( -- )
   false BYTES-OK !  0 AT-D !
   [: BODY-D ;] BUILD
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

: REFUSE ( -- ) s\" 16 allot : LK-F1 ( -- n ) s\" gone\" ;" evaluate-closed ;
: REWIND ( -- ) s\" : LK-F2 ( -- str ) s\" new\" ;" evaluate-closed ;

: BODY-F ( -- )
   [: REFUSE ;] catch REFUSED !
   16 allot  -16 allot
   [: REWIND ;] catch REWOUND ! ;

: CASE-F ( -- )
   0 REFUSED !  0 REWOUND !
   [: BODY-F ;] BUILD
   REFUSED @ 70 T=
   REWOUND @ E-INTERN-REWIND T=
   HOST-CHECK
   s" f: after DP moves back in the space a new string is rejected" SAY ;

: G1$ ( -- str ) s" LK-G1" ;

: INNER ( -- ) s" engine" RTARGET:HOST-TARGET [: ;] LINKER:WITH ;

: BODY-G ( -- )
   [: INNER ;] E-COMP-STATE TTHROWSQ
   s" : LK-G1 ( -- ) ;" evaluate-closed
   G1$ XREF-FIND-CURRENT-INDEX FOUND ! ;

: CASE-G ( -- )
   -1 FOUND !
   [: BODY-G ;] BUILD
   FOUND @ ND0 @ >= TTRUE
   G1$ XREF-FIND XREF-FOUND? TFALSE
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
\ hb's last created word when the builds run.
create LK-HOST 7 ,

: HOST-ADDR ( -- n ) ['] LK-HOST execute NULL-PTR - ;

: BODY-I ( -- )
   [: s" : LK-I1 ( -- ) does> ( -- n ) @ ;" evaluate-closed ;] catch REFUSED ! ;

: CASE-I ( -- )
   0 REFUSED !
   HOST-ADDR HOST-AT !
   [: BODY-I ;] BUILD
   REFUSED @ 70 T=
   HOST-ADDR HOST-AT @ T=
   HOST-CHECK
   s" i: a does> word in a build that creates no word is rejected, and hb's created word stays" SAY ;

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

: N1 ( -- ) s" : >U8 ( -- n ) 5 ;" evaluate-closed ;
: N2 ( -- ) s" : LK-N1 ( -- n ) >U8 ;" evaluate-closed ;

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
   s" : LK-N2 ( -- n ) 65 >U8 ;" TEST-EVAL:RC 0 T=
   s" : LK-N3 ( -- n ) >U8 ;" TEST-EVAL:RC 70 T=
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
   s" : LK-O3 ( -- n ) WL:GLOBAL? ;" TEST-EVAL:RC 70 T=
   s" o: a build may define a package hb has; hb's stays" SAY ;

: CASE-P ( -- )
   s" : LK-M1 ( -- n n ) 1 2 ;" TEST-EVAL:RC 0 T=
   s" p: after the close hb's checker has none of the builds' names" SAY ;

\ RUN and its cases run with no package open, so a text that calls LK-Q0
\ reopens LINKER-TEST.
: LK-Q0 ( n -- n ) 1 + ;

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
   RC-2 @ 70 T=
   s" package LINKER-TEST : LK-Q2 ( -- n ) 1 LK-Q0 ; ;package"
      TEST-EVAL:RC 0 T=
   s" q: a build's definitions leave hb's records linked" SAY ;

public

: RUN ( -- )
   T-RESET
   \ The first build installs the typed loop for good; the host is taken after it.
   [: ;] BUILD
   s" package LINKER-TEST : LK-Q1 ( -- n ) 1 LK-Q0 ; ;package"
      TEST-EVAL:RC 0 T=
   TAKE-HOST
   CASE-A CASE-B CASE-C CASE-D CASE-E CASE-F CASE-G CASE-H CASE-I CASE-J
   CASE-M CASE-N CASE-O CASE-P CASE-Q ;

;package

LINKER-TEST:RUN

\ Case k runs at top level with LINKER-TEST open, as a caller's build does.
package LINKER-TEST
private

variable WL-K                        \ LK-K1's wordlist, read inside the body
variable GLOBAL-K                    \ the build's global wordlist, read there too
variable RC-K

: BODY-K ( -- )
   s" variable LK-K1  1 LK-K1 !" evaluate-closed
   s" LK-K1" XREF-FIND
   dup XREF-FOUND? if XREF-WORDLIST else drop -1 then WL-K !
   BOOT:GLOBAL-WL GLOBAL-K ! ;

: CASE-K ( -- )
   TAKE-HOST
   -1 WL-K !  0 GLOBAL-K !
   [: [: BODY-K ;] BUILD ;] catch RC-K !
   RC-K @ 0 T=
   WL-K @ GLOBAL-K @ T=
   HOST-CHECK
   s" k: a build run inside a host package sees no package open, and the close reopens it" SAY ;

CASE-K

variable RAN-L
\ The package it opens writes 0 into PKG-OUTSIDE-CELL; WITHOUT-PACKAGE restores the caller's.
: OPEN-L ( -- ) [: s" package LK-PX" evaluate-closed ;] INTERPRETER:WITHOUT-PACKAGE ;

: CASE-L ( -- )
   [: [: s" : LK-L1 ( -- ) ;" evaluate-closed ;] INTERPRETER:WITHOUT-PACKAGE ;] catch 0 T=
   s" LK-L1" XREF-FIND XREF-FOUND? TTRUE
   s" package LK-PB ;package" TEST-EVAL:RC 70 T=
   0 RAN-L !
   s" OPEN-L -1 RAN-L ! package LK-PB ;package" TEST-EVAL:RC 70 T=
   RAN-L @ -1 T=
   s" l: WITHOUT-PACKAGE defines at top level and keeps the caller's package outside" SAY ;

CASE-L

: BODY-R ( -- ) s" create LK-RB 1 ," evaluate-closed ;

: BUILD-R ( -- )
   TAKE-HOST
   [: BODY-R ;] BUILD
   HOST-CHECK ;

: CASE-R ( -- )
   s" : LK-R3 ( -- n ) LK-R1 @ ;" TEST-EVAL:RC 0 T=
   s" LK-R3" TEST-EVAL:N 7 T=
   s" r: a created body pending across a build keeps its first cell" SAY ;

\ Case r: LK-R1's body pends across a build that creates its own, as a build
\ driver's top level does, and LK-R2 then closes it.
create LK-R1 7 , 8 ,
BUILD-R
create LK-R2
CASE-R

TYPED-VARIABLE SEALED-S bool

: S$ ( -- str ) s" LK-S" ;

\ LK-S's row in the namespace wordlist BOOT:NAMESPACE-WL names, the build's
\ inside a body.
: S-ROW ( -- ^n ) S$ BOOT:NAMESPACE-WL XREF-FIND-WL ;

: BODY-S ( -- )
   s" package LK-S seal-package ;package" evaluate-closed
   S-ROW {: row:^ :}
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

variable T-AT                        \ LK-T0's record index, below the build's first
: LK-T0 ( -- n ) 7 ;

\ The body retires it as a program's `undefine` of an hb word does.
: BODY-T ( -- ) T-AT @ XREF-REC XREF:RETIRE ;

: CASE-T ( -- )
   TAKE-HOST
   s" LK-T0" XREF-FIND-CURRENT-INDEX T-AT !
   [: BODY-T ;] BUILD
   HOST-CHECK
   s" : LK-T1 ( -- n ) LK-T0 ;" TEST-EVAL:RC 0 T=
   s" LK-T1" TEST-EVAL:N 7 T=
   s" t: an hb word a build retires is found again after the close" SAY ;

CASE-T

\ From a fresh table one build cannot reach HIDX:LOAD-MAX, which lies above
\ DICT-CAP; a close keeps its build's claims, so builds add them up until one more
\ can. U-MANY stops short of LOAD-MAX, so the compaction falls in U-RETIRE's build.
variable U-SEQ
variable U-LAST
TYPED-VARIABLE U-COMPACTED bool

\ The main task's DATA sits at DATA-VA.
: U-CLAIMS ( -- n ) NULL-PTR DATA-VA + HIDX:CLAIMS + CELL-VIEW @ ;
: U-LETTERS ( n -- ) begin dup 26 mod 65 + SB-APPEND-C 26 / dup 0= until drop ;
: U-ONE ( -- ) SB-RESET s" create LK-U" SB-APPEND U-SEQ @ U-LETTERS 1 U-SEQ +! SB$ evaluate-closed ;
: U-ROOM ( -- n ) DICT-CAP ndict@ - 256 - ;
: U-MANY ( -- ) U-ROOM  HIDX:LOAD-MAX U-CLAIMS - 1-  min 0 ?do U-ONE loop ;

: U-RETIRE ( -- )
   T-AT @ XREF-REC XREF:RETIRE
   false U-COMPACTED !
   U-ROOM 0 ?do
      U-CLAIMS U-LAST !  U-ONE
      U-CLAIMS U-LAST @ < if true U-COMPACTED ! leave then
   loop ;

: CASE-U ( -- )
   s" LK-T0" XREF-FIND-CURRENT-INDEX T-AT !
   U-ROOM 0 > TTRUE
   begin HIDX:LOAD-MAX U-CLAIMS - U-ROOM >= while [: U-MANY ;] BUILD repeat
   [: U-RETIRE ;] BUILD
   U-COMPACTED @ TTRUE
   s" : LK-U1 ( -- n ) LK-T0 ;" TEST-EVAL:RC 0 T=
   s" LK-U1" TEST-EVAL:N 7 T=
   s" u: a revived hb word is found after a compaction in its build" SAY ;

CASE-U

;package

T-REPORT
