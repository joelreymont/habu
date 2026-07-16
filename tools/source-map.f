\ source-map.f - authoritative composed-source map reader and origin lookup.

require lib/errors.f
require lib/string.f
require lib/memory.f
require lib/fs.f
require lib/source.f
require lib/vector.f
require lib/content-key.f

package SOURCE-MAP
private

16 constant VEC-CAP
INCLUDE-MAX-DEPTH 1+ constant CHAIN-CAP
32 constant DIGEST-U
$09 constant TAB
$0A constant LF
$46 constant ROW-FILE
$43 constant ROW-CHAIN
$4D constant ROW-MAP

create FILE-A-V VEC-HEADER-CELLS cells allot
create FILE-U-V VEC-HEADER-CELLS cells allot
create CHAIN-OFF-V VEC-HEADER-CELLS cells allot
create CHAIN-U-V VEC-HEADER-CELLS cells allot
create CHAIN-FILE-V VEC-HEADER-CELLS cells allot
create ROW-OUT-V VEC-HEADER-CELLS cells allot
create ROW-U-V VEC-HEADER-CELLS cells allot
create ROW-SRC-V VEC-HEADER-CELLS cells allot
create ROW-LINE-V VEC-HEADER-CELLS cells allot
create ROW-COL-V VEC-HEADER-CELLS cells allot
create ROW-FILE-V VEC-HEADER-CELLS cells allot
create ROW-CHAIN-V VEC-HEADER-CELLS cells allot

PTR-VARIABLE MAP-A
variable MAP-U
PTR-VARIABLE SRC-A
variable SRC-U

variable SCAN-I
PTR-VARIABLE LINE-A
variable LINE-U
variable FIELD-I
variable LAST-OUT-END
variable ROW-PHASE
variable EXPECT-SRC-U
variable EXPECT-FILE-N
variable EXPECT-CHAIN-N
variable EXPECT-ROW-N

create SRC-DIGEST-BUF DIGEST-U allot
create MAP-DIGEST-BUF DIGEST-U allot

variable LOC-ROW
variable LOC-OUT
variable LOC-BYTE
variable LOC-LINE
variable LOC-COL
variable LOC-FILE
variable LOC-CHAIN
variable SCAN-LINE
variable SCAN-COL
variable CHECK-LINE
variable CHECK-COL

variable SPAN-END
variable SPAN-POS
variable SPAN-ROW
variable SPAN-FILE
variable SPAN-CHAIN
variable SPAN-BYTE

: TRUE ( -- bool )
   0 0= ;

: FALSE ( -- bool )
   TRUE 0= ;

: INIT-V ( ptr a -- )
   VEC-CAP >COUNT VEC-INIT ;

: CLEAR-V ( ptr a -- )
   VEC-CLEAR ;

: PUSH-N ( n ptr a -- )
   VEC-PUSH-N drop ;

: PUSH-A ( ptr u8 ptr a -- )
   VEC-PUSH-A drop ;

: N@ ( ptr a n -- n ) {: v:ptr idx:n :}
   v idx >IDX VEC-N@ ;

: A@ ( ptr a n -- ptr u8 ) {: v:ptr idx:n :}
   v idx >IDX VEC-A@ ;

: FILE-N ( -- n )
   FILE-U-V VEC-LEN@ LEN>N ;

: CHAIN-N ( -- n )
   CHAIN-U-V VEC-LEN@ LEN>N ;

: CHAIN-FILE-N ( -- n )
   CHAIN-FILE-V VEC-LEN@ LEN>N ;

: ROW-N ( -- n )
   ROW-U-V VEC-LEN@ LEN>N ;

: VECTORS-INIT ( -- )
   FILE-A-V INIT-V  FILE-U-V INIT-V
   CHAIN-OFF-V INIT-V  CHAIN-U-V INIT-V  CHAIN-FILE-V INIT-V
   ROW-OUT-V INIT-V  ROW-U-V INIT-V  ROW-SRC-V INIT-V
   ROW-LINE-V INIT-V  ROW-COL-V INIT-V  ROW-FILE-V INIT-V
   ROW-CHAIN-V INIT-V ;

: VECTORS-CLEAR ( -- )
   FILE-A-V CLEAR-V  FILE-U-V CLEAR-V
   CHAIN-OFF-V CLEAR-V  CHAIN-U-V CLEAR-V  CHAIN-FILE-V CLEAR-V
   ROW-OUT-V CLEAR-V  ROW-U-V CLEAR-V  ROW-SRC-V CLEAR-V
   ROW-LINE-V CLEAR-V  ROW-COL-V CLEAR-V  ROW-FILE-V CLEAR-V
   ROW-CHAIN-V CLEAR-V ;

: ENSURE-VECTORS ( -- )
   FILE-U-V VEC-CAP@ COUNT>N 0= if VECTORS-INIT else VECTORS-CLEAR then ;

: MAP-A-FIELD ( -- ptr ptr u8 )
   MAP-A 0 ptr-field ;

: MAP-A@ ( -- ptr u8 )
   MAP-A-FIELD @ ;

: MAP-A! ( ptr u8 -- )
   MAP-A-FIELD ! ;

: SRC-A-FIELD ( -- ptr ptr u8 )
   SRC-A 0 ptr-field ;

: SRC-A@ ( -- ptr u8 )
   SRC-A-FIELD @ ;

: SRC-A! ( ptr u8 -- )
   SRC-A-FIELD ! ;

: FAIL-SCHEMA ( -- )
   E-DIAG-SCHEMA throw ;

: FAIL-ORIGIN ( -- )
   E-DIAG-ORIGIN throw ;

: LINE$ ( -- ptr u8 n )
   LINE-A @ LINE-U @ ;

: LINE-LF? ( -- bool )
   SCAN-I @ begin dup MAP-U @ < while
      MAP-A@ over + c@ LF = if drop TRUE exit then
      1+
   repeat drop FALSE ;

: LINE-CHECK ( -- )
   LINE-LF? 0= if FAIL-SCHEMA then ;

: NEXT-LINE? ( -- bool )
   SCAN-I @ MAP-U @ >= if FALSE exit then
   LINE-CHECK
   SCAN-I @ {: start:n :}
   begin SCAN-I @ MAP-U @ < while
      MAP-A@ SCAN-I @ + c@ LF = if
         MAP-A@ start + LINE-A !
         SCAN-I @ start - LINE-U !
         SCAN-I @ 1+ SCAN-I !
         TRUE exit
      then
      SCAN-I @ 1+ SCAN-I !
   repeat
   FALSE ;

: FIELD-RESET ( -- )
   2 FIELD-I ! ;

: FIELD-TAB? ( -- bool )
   FIELD-I @ begin dup LINE-U @ < while
      LINE-A @ over + c@ TAB = if drop TRUE exit then
      1+
   repeat drop FALSE ;

: FIELD-CHECK ( -- )
   FIELD-I @ LINE-U @ >= if FAIL-SCHEMA then
   FIELD-TAB? 0= if FAIL-SCHEMA then ;

: FIELD$ ( -- ptr u8 n )
   FIELD-CHECK
   FIELD-I @ {: start:n :}
   begin FIELD-I @ LINE-U @ < while
      LINE-A @ FIELD-I @ + c@ TAB = if
         LINE-A @ start + FIELD-I @ start -
         FIELD-I @ 1+ FIELD-I !
         exit
      then
      FIELD-I @ 1+ FIELD-I !
   repeat
   LINE-A @ 0 ;

: LAST-FIELD-CHECK ( -- )
   FIELD-I @ LINE-U @ >= if FAIL-SCHEMA then
   FIELD-TAB? if FAIL-SCHEMA then ;

: LAST-FIELD$ ( -- ptr u8 n )
   LAST-FIELD-CHECK
   LINE-A @ FIELD-I @ + LINE-U @ FIELD-I @ -
   LINE-U @ FIELD-I ! ;

: N$-CHECK ( ptr u8 n -- ) {: a:ptr u:n :}
   u 0= if FAIL-SCHEMA then
   u 1 > if a c@ $30 = if FAIL-SCHEMA then then
   0 begin dup u < while
      a over + c@ dup $30 < swap $39 > or if drop FAIL-SCHEMA then
      1+
   repeat drop
   a u STR>NUMBER? MATCH option
      none OF FAIL-SCHEMA ENDOF
      some OF dup 0 < if drop FAIL-SCHEMA else drop then ENDOF
   ;MATCH ;

: N$>N ( ptr u8 n -- n )
   2dup N$-CHECK
   STR>NUMBER? MATCH option
      none OF 0 ENDOF
      some OF ENDOF
   ;MATCH ;

: FIELD-N ( -- n )
   FIELD$ N$>N ;

: LAST-N ( -- n )
   LAST-FIELD$ N$>N ;

: HEX? ( n -- bool ) {: c:n :}
   c $30 >= c $39 <= and if TRUE exit then
   c $41 >= c $46 <= and ;

: HEX-CHECK ( n -- )
   HEX? 0= if FAIL-SCHEMA then ;

: HEX>N ( n -- n ) {: c:n :}
   c HEX-CHECK
   c $30 >= c $39 <= and if c $30 - exit then
   c $41 >= c $46 <= and if c $37 - exit then
   c $57 - ;

: PATH-HEX>BYTES ( ptr u8 n -- ptr u8 n ) {: hex:ptr hexu:n :}
   hexu 2 mod 0 <> if FAIL-SCHEMA then
   hexu 2 / {: u:n :}
   u FS-PATH-CAP > if FAIL-SCHEMA then
   u 1 max MEM:BYTES-ALLOC-LEN MEM:ALLOC-BYTES drop {: dst:ptr :}
   u 0 ?do
      hex i 2 * + c@ HEX>N 4 lshift
      hex i 2 * 1+ + c@ HEX>N or
      dst i + c!
   loop
   dst u ;

: NUL? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   0 begin dup u < while
      a over + c@ 0= if drop TRUE exit then
      1+
   repeat drop FALSE ;

: FILE-DUP? ( ptr u8 n -- bool ) {: path:ptr pathu:n :}
   0 begin dup FILE-N < while
      dup FILE-A-V swap A@
      over FILE-U-V swap N@
      path pathu STR= if drop TRUE exit then
      1+
   repeat drop FALSE ;

: CHAIN-FILE-AT ( n n -- n ) {: chain:n idx:n :}
   CHAIN-OFF-V chain N@ idx + CHAIN-FILE-V swap N@ ;

: CHAIN-SAME? ( n n -- bool ) {: left:n right:n :}
   CHAIN-U-V left N@ CHAIN-U-V right N@ <> if FALSE exit then
   0 begin dup CHAIN-U-V left N@ < while
      dup left swap CHAIN-FILE-AT
      over right swap CHAIN-FILE-AT <> if drop FALSE exit then
      1+
   repeat drop TRUE ;

: CHAIN-DUP? ( n -- bool ) {: chain:n :}
   0 begin dup chain < while
      dup chain CHAIN-SAME? if drop TRUE exit then
      1+
   repeat drop FALSE ;

: CHAIN-LAST-FILE ( n -- n ) {: chain:n :}
   chain CHAIN-U-V chain N@ 1- CHAIN-FILE-AT ;

: CHAIN-PREFIX-HAS? ( n n n -- bool ) {: chain:n end:n file:n :}
   0 begin dup end < while
      chain over CHAIN-FILE-AT file = if drop TRUE exit then
      1+
   repeat drop FALSE ;

: CHAIN-SHAPE-CHECK ( n -- ) {: chain:n :}
   CHAIN-U-V chain N@ {: depth:n :}
   depth 0= depth CHAIN-CAP > or if FAIL-SCHEMA then
   chain 0 CHAIN-FILE-AT 0 <> if FAIL-SCHEMA then
   0 begin dup depth < while
      chain over CHAIN-FILE-AT {: file:n :}
      chain over file CHAIN-PREFIX-HAS? if drop FAIL-SCHEMA then
      1+
   repeat drop ;

: ROW-PREFIX ( -- n )
   LINE-U @ 2 < if FAIL-SCHEMA then
   LINE-A @ 1+ c@ TAB <> if FAIL-SCHEMA then
   LINE-A @ c@ ;

: PARSE-FILE ( -- )
   ROW-PHASE @ 0 <> if FAIL-SCHEMA then
   FIELD-RESET
   FIELD-N FILE-N <> if FAIL-SCHEMA then
   LAST-FIELD$ PATH-HEX>BYTES {: path:ptr pathu:n :}
   pathu 0= if FAIL-SCHEMA then
   path pathu NUL? if FAIL-SCHEMA then
   path pathu FILE-DUP? if FAIL-SCHEMA then
   path FILE-A-V PUSH-A
   pathu FILE-U-V PUSH-N ;

: PARSE-CHAIN-FILES ( n -- ) {: depth:n :}
   0 begin dup depth < while
      dup 1+ depth = if LAST-N else FIELD-N then {: file:n :}
      file FILE-N >= if FAIL-SCHEMA then
      file CHAIN-FILE-V PUSH-N
      1+
   repeat drop ;

: PARSE-CHAIN ( -- )
   ROW-PHASE @ 2 = if FAIL-SCHEMA then
   FILE-N EXPECT-FILE-N @ <> if FAIL-SCHEMA then
   1 ROW-PHASE !
   FIELD-RESET
   FIELD-N CHAIN-N <> if FAIL-SCHEMA then
   FIELD-N {: depth:n :}
   depth 0= if FAIL-SCHEMA then
   CHAIN-FILE-N CHAIN-OFF-V PUSH-N
   depth CHAIN-U-V PUSH-N
   depth PARSE-CHAIN-FILES
   CHAIN-N 1- {: chain:n :}
   chain CHAIN-DUP? if FAIL-SCHEMA then
   chain CHAIN-SHAPE-CHECK ;

: ROW-RANGE-CHECK ( n n -- ) {: out:n u:n :}
   u 0= if FAIL-SCHEMA then
   out u + out < if FAIL-SCHEMA then
   out LAST-OUT-END @ <> if FAIL-SCHEMA then
   out u + LAST-OUT-END ! ;

: ORIGIN-RANGE-CHECK ( n n n n -- ) {: src:n u:n line:n col:n :}
   src u + src < if FAIL-SCHEMA then
   line 1- col 1- + dup line 1- < if drop FAIL-SCHEMA then
   src > if FAIL-SCHEMA then ;

: PARSE-MAP ( -- )
   FILE-N EXPECT-FILE-N @ <> if FAIL-SCHEMA then
   CHAIN-N EXPECT-CHAIN-N @ <> if FAIL-SCHEMA then
   2 ROW-PHASE !
   FIELD-RESET
   FIELD-N {: out:n :}
   FIELD-N {: u:n :}
   out u ROW-RANGE-CHECK
   out ROW-OUT-V PUSH-N
   u ROW-U-V PUSH-N
   FIELD-N {: src:n :}
   FIELD-N dup 1 < if drop FAIL-SCHEMA then {: line:n :}
   FIELD-N dup 1 < if drop FAIL-SCHEMA then {: col:n :}
   src u line col ORIGIN-RANGE-CHECK
   src ROW-SRC-V PUSH-N
   line ROW-LINE-V PUSH-N
   col ROW-COL-V PUSH-N
   FIELD-N {: file:n :}
   file FILE-N >= if FAIL-SCHEMA then
   file ROW-FILE-V PUSH-N
   LAST-N {: chain:n :}
   chain CHAIN-N >= if FAIL-SCHEMA then
   chain CHAIN-LAST-FILE file <> if FAIL-SCHEMA then
   chain ROW-CHAIN-V PUSH-N ;

: PARSE-ROW ( -- )
   ROW-PREFIX
   dup ROW-FILE = if drop PARSE-FILE exit then
   dup ROW-CHAIN = if drop PARSE-CHAIN exit then
   ROW-MAP = if PARSE-MAP exit then
   FAIL-SCHEMA ;

: DIGEST-HEX-CHECK ( ptr u8 n -- ) {: hex:ptr hexu:n :}
   hexu DIGEST-U 2 * <> if FAIL-SCHEMA then
   DIGEST-U 0 ?do
      hex i 2 * + c@ dup HEX-CHECK HEX>N 4 lshift
      hex i 2 * 1+ + c@ dup HEX-CHECK HEX>N or
      SRC-DIGEST-BUF i + c@ <> if FAIL-SCHEMA then
   loop ;

: PARSE-HEADER ( -- )
   NEXT-LINE? 0= if FAIL-SCHEMA then
   LINE-U @ 10 < if FAIL-SCHEMA then
   LINE-A @ 8 s" HABUMAP2" STR= 0= if FAIL-SCHEMA then
   LINE-A @ 8 + c@ TAB <> if FAIL-SCHEMA then
   9 FIELD-I !
   FIELD-N EXPECT-SRC-U !
   FIELD-N EXPECT-FILE-N !
   FIELD-N EXPECT-CHAIN-N !
   FIELD-N EXPECT-ROW-N !
   LAST-FIELD$ DIGEST-HEX-CHECK ;

: PARSE-ROWS ( -- )
   begin NEXT-LINE? while
      LINE-U @ 0= if FAIL-SCHEMA then
      PARSE-ROW
   repeat ;

: SAME-ORIGIN? ( n n -- bool ) {: a:n b:n :}
   ROW-FILE-V a N@ ROW-FILE-V b N@ =
   ROW-CHAIN-V a N@ ROW-CHAIN-V b N@ = and ;

: PREV-ORIGIN-ROW ( n -- n ) {: row:n :}
   row 1- begin dup 0 >= while
      dup row SAME-ORIGIN? if exit then
      1-
   repeat drop -1 ;

: ROW-END-LOCATION ( n -- ) {: row:n :}
   ROW-LINE-V row N@ CHECK-LINE !
   ROW-COL-V row N@ CHECK-COL !
   ROW-OUT-V row N@ {: start:n :}
   start ROW-U-V row N@ + {: end:n :}
   start begin dup end < while
      SRC-A@ over + c@ LF = if
         CHECK-LINE @ 1+ CHECK-LINE !
         1 CHECK-COL !
      else
         CHECK-COL @ 1+ CHECK-COL !
      then
      1+
   repeat drop ;

: ROW-START-CHECK ( n -- ) {: row:n :}
   ROW-SRC-V row N@ 0= if
      ROW-LINE-V row N@ 1 <> ROW-COL-V row N@ 1 <> or if FAIL-SCHEMA then
      exit
   then
   row PREV-ORIGIN-ROW dup 0 < if drop FAIL-SCHEMA then {: prev:n :}
   ROW-SRC-V prev N@ ROW-U-V prev N@ + ROW-SRC-V row N@ <> if FAIL-SCHEMA then
   prev ROW-END-LOCATION
   CHECK-LINE @ ROW-LINE-V row N@ <> if FAIL-SCHEMA then
   CHECK-COL @ ROW-COL-V row N@ <> if FAIL-SCHEMA then ;

: VALIDATE-ORIGINS ( -- )
   0 begin dup ROW-N < while
      dup ROW-START-CHECK
      1+
   repeat drop ;

: VALIDATE-ROWS ( -- )
   EXPECT-SRC-U @ SRC-U @ <> if FAIL-SCHEMA then
   FILE-N EXPECT-FILE-N @ <> if FAIL-SCHEMA then
   CHAIN-N EXPECT-CHAIN-N @ <> if FAIL-SCHEMA then
   ROW-N EXPECT-ROW-N @ <> if FAIL-SCHEMA then
   ROW-N 0= if FAIL-SCHEMA then
   LAST-OUT-END @ SRC-U @ <> if FAIL-SCHEMA then
   VALIDATE-ORIGINS ;

: RESET ( -- )
   ENSURE-VECTORS
   0 SCAN-I !
   0 LAST-OUT-END !
   0 ROW-PHASE !
   -1 LOC-ROW ! ;

: AUTH-CHECK ( ptr u8 n ptr u8 n -- ) {: src-dg:ptr src-dgu:n map-dg:ptr map-dgu:n :}
   src-dgu DIGEST-U <> map-dgu DIGEST-U <> or if FAIL-SCHEMA then
   SRC-A@ SRC-U @ SRC-DIGEST-BUF SHA256
   SRC-DIGEST-BUF DIGEST-U src-dg src-dgu STR= 0= if FAIL-SCHEMA then
   MAP-A@ MAP-U @ MAP-DIGEST-BUF SHA256
   MAP-DIGEST-BUF DIGEST-U map-dg map-dgu STR= 0= if FAIL-SCHEMA then ;

: LOAD ( ptr u8 n ptr u8 n ptr u8 n ptr u8 n -- )
   {: map:ptr mapu:n src:ptr srcu:n src-dg:ptr src-dgu:n map-dg:ptr map-dgu:n :}
   map MAP-A!  mapu MAP-U !
   src SRC-A!  srcu SRC-U !
   RESET
   src-dg src-dgu map-dg map-dgu AUTH-CHECK
   PARSE-HEADER
   PARSE-ROWS
   VALIDATE-ROWS ;

: ROW-CONTAINS? ( n n -- bool ) {: row:n byte:n :}
   ROW-OUT-V row N@ {: out:n :}
   byte out >= byte out - ROW-U-V row N@ < and ;

: FIND-ROW ( n -- n ) {: byte:n :}
   0 begin dup ROW-N < while
      dup byte ROW-CONTAINS? if exit then
      1+
   repeat drop -1 ;

: ADVANCE-LOC ( n n -- ) {: start:n end:n :}
   start begin dup end < while
      SRC-A@ over + c@ LF = if
         LOC-LINE @ 1+ LOC-LINE !
         1 LOC-COL !
      else
         LOC-COL @ 1+ LOC-COL !
      then
      1+
   repeat drop ;

: LOCATE-ROW ( n n -- ) {: byte:n row:n :}
   ROW-OUT-V row N@ {: out:n :}
   row LOC-ROW !
   byte LOC-OUT !
   ROW-SRC-V row N@ {: src:n :}
   src byte out - + dup src < if drop FAIL-ORIGIN then LOC-BYTE !
   ROW-LINE-V row N@ LOC-LINE !
   ROW-COL-V row N@ LOC-COL !
   ROW-FILE-V row N@ LOC-FILE !
   ROW-CHAIN-V row N@ LOC-CHAIN !
   out byte ADVANCE-LOC ;

: LOCATE ( n -- ) {: byte:n :}
   byte 0 < if FAIL-ORIGIN then
   byte FIND-ROW dup 0 < if drop FAIL-ORIGIN then {: row:n :}
   byte row LOCATE-ROW ;

: LOCATE-END ( -- )
   ROW-N 0= if FAIL-ORIGIN then
   ROW-N 1- {: row:n :}
   ROW-OUT-V row N@ ROW-U-V row N@ + SRC-U @ <> if FAIL-ORIGIN then
   SRC-U @ row LOCATE-ROW ;

: SPAN-SEG-END ( n -- n ) {: row:n :}
   ROW-OUT-V row N@ ROW-U-V row N@ + SPAN-END @ min ;

: SPAN-ROW-CHECK ( -- )
   SPAN-ROW @ ROW-N >= if FAIL-ORIGIN then
   SPAN-ROW @ {: row:n :}
   ROW-OUT-V row N@ {: out:n :}
   SPAN-POS @ out < SPAN-POS @ out - ROW-U-V row N@ >= or if FAIL-ORIGIN then
   ROW-FILE-V row N@ SPAN-FILE @ <> if FAIL-ORIGIN then
   ROW-CHAIN-V row N@ SPAN-CHAIN @ <> if FAIL-ORIGIN then
   ROW-SRC-V row N@ SPAN-POS @ out - + SPAN-BYTE @ <> if FAIL-ORIGIN then ;

: SPAN-ADVANCE ( -- )
   SPAN-ROW @ SPAN-SEG-END {: end:n :}
   end SPAN-POS @ - SPAN-BYTE @ + dup SPAN-BYTE @ < if drop FAIL-ORIGIN then
   SPAN-BYTE !
   end SPAN-POS !
   SPAN-ROW @ 1+ SPAN-ROW ! ;

: LOCATE-SPAN ( n n -- ) {: start:n end:n :}
   end start < if FAIL-ORIGIN then
   start end = start SRC-U @ = and if LOCATE-END exit then
   start LOCATE
   end start = if exit then
   end SRC-U @ > if FAIL-ORIGIN then
   end SPAN-END !
   start SPAN-POS !
   LOC-ROW @ SPAN-ROW !
   LOC-FILE @ SPAN-FILE !
   LOC-CHAIN @ SPAN-CHAIN !
   LOC-BYTE @ SPAN-BYTE !
   begin SPAN-POS @ SPAN-END @ < while
      SPAN-ROW-CHECK
      SPAN-ADVANCE
   repeat
   start LOCATE ;

: FILE$ ( n -- ptr u8 n ) {: file:n :}
   file 0 < file FILE-N >= or if FAIL-SCHEMA then
   FILE-A-V file A@ FILE-U-V file N@ ;

: CHAIN-FILE@ ( n n -- n ) {: chain:n idx:n :}
   chain 0 < chain CHAIN-N >= or if FAIL-SCHEMA then
   idx 0 < idx CHAIN-U-V chain N@ >= or if FAIL-SCHEMA then
   chain idx CHAIN-FILE-AT ;

: LINE-COL-FIND? ( n n -- bool ) {: line:n col:n :}
   1 SCAN-LINE !
   1 SCAN-COL !
   0 begin dup SRC-U @ < while
      SCAN-LINE @ line = SCAN-COL @ col = and if
         LOC-OUT !
         TRUE exit
      then
      SRC-A@ over + c@ LF = if
         SCAN-LINE @ 1+ SCAN-LINE !
         1 SCAN-COL !
      else
         SCAN-COL @ 1+ SCAN-COL !
      then
      1+
   repeat drop FALSE ;

: LOCATE-LINE-COL ( n n -- ) {: line:n col:n :}
   line 1 < col 1 < or if FAIL-ORIGIN then
   line col LINE-COL-FIND? 0= if FAIL-ORIGIN then
   LOC-OUT @ LOCATE ;

public

: OPEN ( ptr u8 n ptr u8 n ptr u8 n ptr u8 n -- )
   LOAD ;

: ORIGIN! ( n -- )
   LOCATE ;

: ORIGIN-LINE-COLUMN! ( n n -- )
   LOCATE-LINE-COL ;

: ORIGIN-SPAN! ( n n -- )
   LOCATE-SPAN ;

: ORIGIN-OUTPUT-BYTE ( -- n )
   LOC-OUT @ ;

: ORIGIN-FILE-ID ( -- n )
   LOC-FILE @ ;

: ORIGIN-CHAIN-ID ( -- n )
   LOC-CHAIN @ ;

: ORIGIN-FILE$ ( -- ptr u8 n )
   LOC-FILE @ FILE$ ;

: ORIGIN-LINE ( -- n )
   LOC-LINE @ ;

: ORIGIN-COLUMN ( -- n )
   LOC-COL @ ;

: ORIGIN-BYTE ( -- n )
   LOC-BYTE @ ;

: ORIGIN-CHAIN-N ( -- n )
   CHAIN-U-V LOC-CHAIN @ N@ ;

: ORIGIN-CHAIN-FILE$ ( n -- ptr u8 n )
   LOC-CHAIN @ swap CHAIN-FILE@ FILE$ ;

;package
