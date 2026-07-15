\ source-compose.f - exact frozen source composition for native builds.

require lib/errors.f
require lib/string.f
require lib/memory.f
require lib/fs.f
require lib/vector.f
require lib/source.f
require lib/content-key.f
require lib/json-write.f
require tools/lint/text.f
require tools/lint/token.f
require tools/lint/lib.f
require tools/lint/source-lex.f

package SOURCE-COMPOSE
private

SOURCE-ARENA-CAP constant OUT-CAP
SOURCE-ARENA-CAP constant MAP-CAP
SOURCE-ARENA-CAP constant EVENT-CAP
16 constant VEC-INIT-CAP
$20 constant DIGEST-U
$40 constant HEX-U
$20 constant SP
$0A constant LF
$0D constant CR
$09 constant TAB
$22 constant DQ
$5C constant BSLASH

ENUM action include require provide ;ENUM
ENUM registry-state fresh known ;ENUM

create FILE-PATH-V VEC-HEADER-CELLS cells allot
create FILE-U-V VEC-HEADER-CELLS cells allot
create FILE-SRC-V VEC-HEADER-CELLS cells allot
create FILE-SRC-U-V VEC-HEADER-CELLS cells allot
create FILE-DG-V VEC-HEADER-CELLS cells allot
create FILE-TOK-V VEC-HEADER-CELLS cells allot
create FILE-TOK-N-V VEC-HEADER-CELLS cells allot
create FILE-ACTIVE-V VEC-HEADER-CELLS cells allot

create TOK-FILE-V VEC-HEADER-CELLS cells allot
create TOK-KIND-V VEC-HEADER-CELLS cells allot
create TOK-START-V VEC-HEADER-CELLS cells allot
create TOK-END-V VEC-HEADER-CELLS cells allot
create TOK-LINE-V VEC-HEADER-CELLS cells allot
create TOK-COL-V VEC-HEADER-CELLS cells allot
create TOK-A-V VEC-HEADER-CELLS cells allot
create TOK-U-V VEC-HEADER-CELLS cells allot

create REG-PATH-V VEC-HEADER-CELLS cells allot
create REG-U-V VEC-HEADER-CELLS cells allot

create MAP-OUT-V VEC-HEADER-CELLS cells allot
create MAP-U-V VEC-HEADER-CELLS cells allot
create MAP-FILE-V VEC-HEADER-CELLS cells allot
create MAP-SRC-V VEC-HEADER-CELLS cells allot
create MAP-LINE-V VEC-HEADER-CELLS cells allot
create MAP-COL-V VEC-HEADER-CELLS cells allot

create FRAME-FILE INCLUDE-MAX-DEPTH cells allot
create FRAME-TOK INCLUDE-MAX-DEPTH cells allot
create FRAME-END INCLUDE-MAX-DEPTH cells allot
create FRAME-SRC INCLUDE-MAX-DEPTH cells allot
create FRAME-LINE INCLUDE-MAX-DEPTH cells allot
create FRAME-COL INCLUDE-MAX-DEPTH cells allot

create PATH-BUF FS-PATH-CAP allot
create FAIL-TARGET-BUF FS-PATH-CAP allot
create NUM-BUF 32 allot
create DIGEST-BUF DIGEST-U allot
create MAP-DIGEST-BUF DIGEST-U allot
create PLAN-DIGEST-BUF DIGEST-U allot
create INT-BUF 8 allot

PTR-VARIABLE OUT-A
variable OUT-U
PTR-VARIABLE MAP-A
variable MAP-U
PTR-VARIABLE EVENT-A
variable EVENT-U
PTR-VARIABLE FREEZE-A
variable FREEZE-U
PTR-VARIABLE READ-PATH-A
variable READ-PATH-U
variable DEPTH
variable COMPILING
variable DEF-OPEN
variable QUOTE-DEPTH
variable NAME-NEXT
variable PATH-U
variable DEC-I
variable DEC-END
variable MAP-I
variable DIGEST-I
variable WALK-I

variable FAIL-CODE
variable FAIL-FILE
variable FAIL-LINE
variable FAIL-COL
variable FAIL-BYTE
variable FAIL-TARGET-U

: TRUE ( -- bool )
   0 0= ;

: FALSE ( -- bool )
   TRUE 0= ;

: INIT-V ( ptr a -- )
   VEC-INIT-CAP >COUNT VEC-INIT ;

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

: N! ( n ptr a n -- ) {: value:n v:ptr idx:n :}
   value v idx >IDX VEC-N! ;

: A! ( ptr u8 ptr a n -- ) {: value:ptr v:ptr idx:n :}
   value v idx >IDX VEC-A! ;

: FILE-N ( -- n )
   FILE-U-V VEC-LEN@ LEN>N ;

: TOK-N ( -- n )
   TOK-U-V VEC-LEN@ LEN>N ;

: MAP-N ( -- n )
   MAP-U-V VEC-LEN@ LEN>N ;

: VECTORS-INIT ( -- )
   FILE-PATH-V INIT-V  FILE-U-V INIT-V  FILE-SRC-V INIT-V
   FILE-SRC-U-V INIT-V  FILE-DG-V INIT-V  FILE-TOK-V INIT-V
   FILE-TOK-N-V INIT-V  FILE-ACTIVE-V INIT-V
   TOK-FILE-V INIT-V  TOK-KIND-V INIT-V  TOK-START-V INIT-V
   TOK-END-V INIT-V  TOK-LINE-V INIT-V  TOK-COL-V INIT-V
   TOK-A-V INIT-V  TOK-U-V INIT-V
   REG-PATH-V INIT-V  REG-U-V INIT-V
   MAP-OUT-V INIT-V  MAP-U-V INIT-V  MAP-FILE-V INIT-V
   MAP-SRC-V INIT-V  MAP-LINE-V INIT-V  MAP-COL-V INIT-V ;

: VECTORS-CLEAR ( -- )
   FILE-PATH-V CLEAR-V  FILE-U-V CLEAR-V  FILE-SRC-V CLEAR-V
   FILE-SRC-U-V CLEAR-V  FILE-DG-V CLEAR-V  FILE-TOK-V CLEAR-V
   FILE-TOK-N-V CLEAR-V  FILE-ACTIVE-V CLEAR-V
   TOK-FILE-V CLEAR-V  TOK-KIND-V CLEAR-V  TOK-START-V CLEAR-V
   TOK-END-V CLEAR-V  TOK-LINE-V CLEAR-V  TOK-COL-V CLEAR-V
   TOK-A-V CLEAR-V  TOK-U-V CLEAR-V
   REG-PATH-V CLEAR-V  REG-U-V CLEAR-V
   MAP-OUT-V CLEAR-V  MAP-U-V CLEAR-V  MAP-FILE-V CLEAR-V
   MAP-SRC-V CLEAR-V  MAP-LINE-V CLEAR-V  MAP-COL-V CLEAR-V ;

: ENSURE-VECTORS ( -- )
   FILE-U-V VEC-CAP@ COUNT>N 0= if VECTORS-INIT else VECTORS-CLEAR then ;

: ALLOC ( n -- ptr u8 ) {: u:n :}
   u 1 max MEM:BYTES-ALLOC-LEN MEM:ALLOC-BYTES drop ;

: BYTES-COPY ( ptr u8 n -- ptr u8 ) {: a:ptr u:n :}
   u ALLOC {: dst:ptr :}
   a dst u BYTE-COPY
   dst ;

: NUL? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   0 begin dup u < while
      a over + c@ 0= if drop TRUE exit then
      1+
   repeat drop FALSE ;

: FAIL! ( n n n n n -- ) {: code:n file:n line:n col:n byte:n :}
   code FAIL-CODE !
   file FAIL-FILE !
   line FAIL-LINE !
   col FAIL-COL !
   byte FAIL-BYTE !
   code throw ;

: FAIL-TOK ( n n -- ) {: code:n tok:n :}
   code TOK-FILE-V tok N@ TOK-LINE-V tok N@ TOK-COL-V tok N@
   TOK-START-V tok N@ FAIL! ;

: FAIL-FILE0 ( n n -- ) {: code:n file:n :}
   code file 1 1 0 FAIL! ;

: PATH-FAIL ( n n n -- ) {: code:n file:n tok:n :}
   tok 0 >= if code tok FAIL-TOK then
   code file FAIL-FILE0 ;

: PATH-CHECK ( ptr u8 n n n -- ) {: a:ptr u:n file:n tok:n :}
   u 0 <= if E-DISC-MALFORMED file tok PATH-FAIL then
   u FS-PATH-CAP > if E-DISC-CAPACITY file tok PATH-FAIL then
   a u NUL? if E-DISC-NUL-PATH file tok PATH-FAIL then ;

: FAIL-TARGET! ( ptr u8 n -- ) {: a:ptr u:n :}
   a FAIL-TARGET-BUF u BYTE-COPY
   u FAIL-TARGET-U ! ;

: FILE-PATH$ ( n -- ptr u8 n ) {: file:n :}
   FILE-PATH-V file A@ FILE-U-V file N@ ;

: FILE-SOURCE$ ( n -- ptr u8 n ) {: file:n :}
   FILE-SRC-V file A@ FILE-SRC-U-V file N@ ;

: TOK$ ( n -- ptr u8 n ) {: tok:n :}
   TOK-A-V tok A@ TOK-U-V tok N@ ;

: TOK=? ( n ptr u8 n -- bool ) {: tok:n a:ptr u:n :}
   tok TOK$ a u LINT-STR=CI ;

: WORD? ( n -- bool )
   TOK-KIND-V swap N@ L-WORD = ;

: ANY-STRING? ( n -- bool ) {: tok:n :}
   tok TOK$ 2dup LINT-NORMAL-STRING-OPENER? if 2drop TRUE exit then
   LINT-ESC-STRING-OPENER? ;

: STRING? ( n -- bool ) {: tok:n :}
   tok ANY-STRING? 0= if FALSE exit then
   tok TOK$ drop c@ LINT-FOLD $73 = ;

: ESCAPED? ( n -- bool )
   TOK$ LINT-ESC-STRING-OPENER? ;

: HEX? ( n -- bool ) {: c:n :}
   c $30 >= c $39 <= and if TRUE exit then
   c $41 >= c $46 <= and if TRUE exit then
   c $61 >= c $66 <= and ;

: HEX>N ( n -- n ) {: c:n :}
   c $30 >= c $39 <= and if c $30 - exit then
   c $41 >= c $46 <= and if c $37 - exit then
   c $61 >= c $66 <= and if c $57 - exit then
   E-DISC-MALFORMED FAIL-FILE @ FAIL-LINE @ FAIL-COL @ FAIL-BYTE @ FAIL! ;

: PATH-C+ ( n n -- ) {: c:n tok:n :}
   PATH-U @ FS-PATH-CAP >= if E-DISC-CAPACITY tok FAIL-TOK then
   c PATH-BUF PATH-U @ + c!
   PATH-U @ 1+ PATH-U ! ;

: SIMPLE-ESC ( n n -- n ) {: c:n tok:n :}
   c DQ = c $71 = or if DQ exit then
   c BSLASH = if BSLASH exit then
   c $61 = if 7 exit then
   c $62 = if 8 exit then
   c $65 = if $1B exit then
   c $6C = if LF exit then
   c $66 = if $0C exit then
   c $6E = if LF exit then
   c $72 = if CR exit then
   c $74 = if TAB exit then
   c $76 = if $0B exit then
   c $7A = if 0 exit then
   E-DISC-MALFORMED tok FAIL-TOK ;

: DECODE-ESC ( ptr u8 n n -- ) {: src:ptr rawu:n tok:n :}
   0 DEC-I !
   begin DEC-I @ rawu < while
      src DEC-I @ + c@ dup BSLASH <> if
         tok PATH-C+
         DEC-I @ 1+ DEC-I !
      else
         drop
         DEC-I @ 1+ rawu >= if E-DISC-MALFORMED tok FAIL-TOK then
         src DEC-I @ 1+ + c@ {: esc:n :}
         esc $78 = esc $58 = or if
            DEC-I @ 3 + rawu >= if E-DISC-MALFORMED tok FAIL-TOK then
            src DEC-I @ 2 + + c@ dup HEX? 0= if drop E-DISC-MALFORMED tok FAIL-TOK then HEX>N 4 lshift
            src DEC-I @ 3 + + c@ dup HEX? 0= if drop E-DISC-MALFORMED tok FAIL-TOK then HEX>N or
            tok PATH-C+
            DEC-I @ 4 + DEC-I !
         else
            esc tok SIMPLE-ESC tok PATH-C+
            DEC-I @ 2 + DEC-I !
         then
      then
   repeat ;

: DECODE-PATH ( n -- ptr u8 n ) {: tok:n :}
   0 PATH-U !
   TOK-START-V tok N@ TOK-U-V tok N@ + 1+ {: start:n :}
   TOK-END-V tok N@ 1- {: end:n :}
   end start < if E-DISC-MALFORMED tok FAIL-TOK then
   TOK-FILE-V tok N@ FILE-SOURCE$ drop start + {: src:ptr :}
   end start - {: rawu:n :}
   tok ESCAPED? if src rawu tok DECODE-ESC else src PATH-BUF rawu BYTE-COPY rawu PATH-U ! then
   PATH-BUF PATH-U @ TOK-FILE-V tok N@ tok PATH-CHECK
   PATH-BUF PATH-U @ ;

: FILE-FIND ( ptr u8 n -- n ) {: a:ptr u:n :}
   0 begin dup FILE-N < while
      dup FILE-PATH$ a u LINT-STR= if exit then
      1+
   repeat drop -1 ;

: LEX-BOUNDARY ( n n -- n ) {: file:n lex:n :}
   lex 1+ L# @ < if lex 1+ LB@ exit then
   file FILE-SOURCE$ nip ;

: LEX-STRING-END ( n n -- n ) {: file:n lex:n :}
   file lex LEX-BOUNDARY DEC-END !
   file FILE-SOURCE$ drop {: src:ptr :}
   lex LB@ lex LEX-TOK nip + {: floor:n :}
   begin DEC-END @ floor > while
      src DEC-END @ 1- + c@ LINT-WS? 0= if DEC-END @ exit then
      DEC-END @ 1- DEC-END !
   repeat DEC-END @ ;

: LEX-TOKEN-END ( n n -- n ) {: file:n lex:n :}
   lex LEX-TOK 2dup LINT-NORMAL-STRING-OPENER? if
      2drop file lex LEX-STRING-END exit
   then
   LINT-ESC-STRING-OPENER? if file lex LEX-STRING-END exit then
   lex LB@ lex LEX-TOK nip + ;

: COPY-LEX-TOKENS ( n -- ) {: file:n :}
   TOK-N FILE-TOK-V file N!
   0 begin dup L# @ < while
      file TOK-FILE-V PUSH-N
      dup LK@ TOK-KIND-V PUSH-N
      dup LB@ TOK-START-V PUSH-N
      file over LEX-TOKEN-END TOK-END-V PUSH-N
      dup LL@ TOK-LINE-V PUSH-N
      dup LC@ TOK-COL-V PUSH-N
      dup LEX-TOK {: a:ptr u:n :}
      a TOK-A-V PUSH-A
      u TOK-U-V PUSH-N
      1+
   repeat drop
   TOK-N FILE-TOK-V file N@ - FILE-TOK-N-V file N! ;

: FILE-MISSING ( n -- ) {: file:n :}
   DEPTH @ 0 > if E-DISC-MISSING FRAME-TOK DEPTH @ 1- cells + @ FAIL-TOK then
   E-DISC-MISSING file FAIL-FILE0 ;

: FREEZE-BUF ( -- ptr u8 )
   FREEZE-A @ 0= if OUT-CAP MEM:BYTES-ALLOC-LEN MEM:ALLOC-BYTES drop FREEZE-A ! then
   FREEZE-A @ ;

: READ-PATH$ ( -- ptr u8 n )
   READ-PATH-A @ READ-PATH-U @ ;

: READ-ACT ( -- )
   READ-PATH$ FREEZE-BUF OUT-CAP READ-ALL FREEZE-U ! ;

: READ-FROZEN ( ptr u8 n n -- ) {: path:ptr pathu:n file:n :}
   path READ-PATH-A !
   pathu READ-PATH-U !
   [: READ-ACT ;] catch {: code:n :}
   code 0= if exit then
   code E-FS-CAPACITY = if E-DISC-CAPACITY file FAIL-FILE0 then
   file FILE-MISSING ;

: FILE-READ ( ptr u8 n n -- ) {: path:ptr pathu:n file:n :}
   path pathu file READ-FROZEN
   FREEZE-U @ {: size:n :}
   size 1+ ALLOC {: src:ptr :}
   FREEZE-BUF src size BYTE-COPY
   SP src size + c!
   src FILE-SRC-V file A!
   size FILE-SRC-U-V file N!
   DIGEST-U ALLOC {: dg:ptr :}
   src size dg SHA256
   dg FILE-DG-V file A!
   src size LEX-SOURCE
   LEX-UNTERM-QUOTE? if E-DISC-UNTERM file FAIL-FILE0 then
   file COPY-LEX-TOKENS ;

: FILE-ADD ( ptr u8 n -- n ) {: path:ptr pathu:n :}
   FILE-N {: file:n :}
   path pathu BYTES-COPY FILE-PATH-V PUSH-A
   pathu FILE-U-V PUSH-N
   PATH-BUF FILE-SRC-V PUSH-A
   0 FILE-SRC-U-V PUSH-N
   DIGEST-BUF FILE-DG-V PUSH-A
   0 FILE-TOK-V PUSH-N
   0 FILE-TOK-N-V PUSH-N
   0 FILE-ACTIVE-V PUSH-N
   path pathu file FILE-READ
   file ;

: FILE-ID ( ptr u8 n -- n ) {: path:ptr pathu:n :}
   path pathu FILE-FIND dup 0 >= if exit then
   drop path pathu FILE-ADD ;

: REG-N ( -- n )
   REG-U-V VEC-LEN@ LEN>N ;

: REG-PATH$ ( n -- ptr u8 n ) {: reg:n :}
   REG-PATH-V reg A@ REG-U-V reg N@ ;

: REG-FIND ( ptr u8 n -- n ) {: a:ptr u:n :}
   0 begin dup REG-N < while
      dup REG-PATH$ a u LINT-STR= if exit then
      1+
   repeat drop -1 ;

: REG-ADD ( ptr u8 n -- ) {: path:ptr pathu:n :}
   REG-N REQUIRE-MAX >= if E-DISC-CAPACITY FAIL-FILE @ FAIL-LINE @ FAIL-COL @ FAIL-BYTE @ FAIL! then
   path pathu BYTES-COPY REG-PATH-V PUSH-A
   pathu REG-U-V PUSH-N ;

: ACTION>CODE ( action -- n )
   MATCH action
      include OF 0 ENDOF
      require OF 1 ENDOF
      provide OF 2 ENDOF
   ;MATCH ;

: REGISTRY-STATE>CODE ( registry-state -- n )
   MATCH registry-state
      fresh OF 0 ENDOF
      known OF 1 ENDOF
   ;MATCH ;

: EVENT-ROOM ( n -- ) {: add:n :}
   add 0 < if E-DISC-CAPACITY FAIL-FILE @ FAIL-LINE @ FAIL-COL @ FAIL-BYTE @ FAIL! then
   EVENT-U @ add + EVENT-U @ < if E-DISC-CAPACITY FAIL-FILE @ FAIL-LINE @ FAIL-COL @ FAIL-BYTE @ FAIL! then
   EVENT-U @ add + EVENT-CAP > if E-DISC-CAPACITY FAIL-FILE @ FAIL-LINE @ FAIL-COL @ FAIL-BYTE @ FAIL! then ;

: EVENT-BYTES+ ( ptr u8 n -- ) {: a:ptr u:n :}
   u EVENT-ROOM
   a EVENT-A @ EVENT-U @ + u BYTE-COPY
   EVENT-U @ u + EVENT-U ! ;

: EVENT-U64+ ( n -- )
   INT-BUF BE64!
   INT-BUF 8 EVENT-BYTES+ ;

: EVENT+ ( action registry-state n n n n ptr u8 n -- )
   {: kind:action state:registry-state file:n line:n col:n byte:n path:ptr pathu:n :}
   kind ACTION>CODE EVENT-U64+
   state REGISTRY-STATE>CODE EVENT-U64+
   file EVENT-U64+
   line EVENT-U64+
   col EVENT-U64+
   byte EVENT-U64+
   pathu EVENT-U64+
   path pathu EVENT-BYTES+ ;

: OUT-ROOM ( n -- ) {: add:n :}
   add 0 < if E-DISC-CAPACITY FAIL-FILE @ FAIL-LINE @ FAIL-COL @ FAIL-BYTE @ FAIL! then
   OUT-U @ add + OUT-U @ < if E-DISC-CAPACITY FAIL-FILE @ FAIL-LINE @ FAIL-COL @ FAIL-BYTE @ FAIL! then
   OUT-U @ add + OUT-CAP > if E-DISC-CAPACITY FAIL-FILE @ FAIL-LINE @ FAIL-COL @ FAIL-BYTE @ FAIL! then ;

: OUT+ ( ptr u8 n -- ) {: a:ptr u:n :}
   u OUT-ROOM
   a OUT-A @ OUT-U @ + u BYTE-COPY
   OUT-U @ u + OUT-U ! ;

: OUT-C ( n -- ) {: c:n :}
   1 OUT-ROOM
   c OUT-A @ OUT-U @ + c!
   OUT-U @ 1+ OUT-U ! ;

: U$ ( n -- ptr u8 n ) {: value:n :}
   32 DEC-I !
   value 0= if
      DEC-I @ 1- DEC-I !
      $30 NUM-BUF DEC-I @ + c!
      NUM-BUF DEC-I @ + 1 exit
   then
   value begin dup 0 > while
      dup 10 mod $30 +
      DEC-I @ 1- DEC-I !
      NUM-BUF DEC-I @ + c!
      10 /
   repeat drop
   NUM-BUF DEC-I @ + 32 DEC-I @ - ;

: OUT-N ( n -- )
   U$ OUT+ ;

: PATH-ESC-C ( n -- ) {: c:n :}
   c DQ = if BSLASH OUT-C $71 OUT-C exit then
   c BSLASH = if BSLASH OUT-C BSLASH OUT-C exit then
   c LF = if BSLASH OUT-C $6E OUT-C exit then
   c CR = if BSLASH OUT-C $72 OUT-C exit then
   c TAB = if BSLASH OUT-C $74 OUT-C exit then
   c SP < c $7F >= or if
      BSLASH OUT-C $78 OUT-C
      c 4 rshift $F and NIB>HEX OUT-C
      c $F and NIB>HEX OUT-C
      exit
   then
   c OUT-C ;

: PATH-ESC+ ( ptr u8 n -- ) {: a:ptr u:n :}
   0 begin dup u < while
      a over + c@ PATH-ESC-C
      1+
   repeat drop ;

: FILE-MARK ( n -- ) {: file:n :}
   COMPILING @ 0 <> if exit then
   LF OUT-C
   $53 OUT-C BSLASH OUT-C DQ OUT-C SP OUT-C
   file FILE-PATH$ PATH-ESC+
   DQ OUT-C
   s"  DIAG-FILE!" OUT+
   LF OUT-C ;

: MAP+ ( n n n n n n -- ) {: out:n u:n file:n src:n line:n col:n :}
   u 0= if exit then
   out MAP-OUT-V PUSH-N
   u MAP-U-V PUSH-N
   file MAP-FILE-V PUSH-N
   src MAP-SRC-V PUSH-N
   line MAP-LINE-V PUSH-N
   col MAP-COL-V PUSH-N ;

: SOURCE+ ( n n n n n -- ) {: file:n off:n u:n line:n col:n :}
   u 0= if exit then
   OUT-U @ {: out:n :}
   file FILE-SOURCE$ drop off + u OUT+
   out u file off line col MAP+ ;

: FRAME-IDX ( -- n )
   DEPTH @ 1- dup 0 < if E-DISC-MALFORMED FAIL-FILE @ FAIL-LINE @ FAIL-COL @ FAIL-BYTE @ FAIL! then ;

: FRAME@ ( ptr a -- n ) {: table:ptr :}
   table FRAME-IDX cells + @ ;

: FRAME! ( n ptr a -- ) {: value:n table:ptr :}
   value table FRAME-IDX cells + ! ;

: FRAME-PUSH! ( n ptr a -- ) {: value:n table:ptr :}
   value table DEPTH @ cells + ! ;

: ADV-ORIGIN ( n n -- ) {: file:n end:n :}
   FRAME-SRC FRAME@ WALK-I !
   file FILE-SOURCE$ drop {: src:ptr :}
   begin WALK-I @ end < while
      src WALK-I @ + c@ LF = if
         FRAME-LINE FRAME@ 1+ FRAME-LINE FRAME!
         1 FRAME-COL FRAME!
      else
         FRAME-COL FRAME@ 1+ FRAME-COL FRAME!
      then
      WALK-I @ 1+ WALK-I !
   repeat
   end FRAME-SRC FRAME! ;

: APPEND-RAW-TO ( n -- ) {: end:n :}
   FRAME-FILE FRAME@ {: file:n :}
   FRAME-SRC FRAME@ {: off:n :}
   file off end off - FRAME-LINE FRAME@ FRAME-COL FRAME@ SOURCE+
   file end ADV-ORIGIN ;

: APPEND-BLANK ( n n n n -- ) {: file:n start:n end:n origin:n :}
   OUT-U @ {: out:n :}
   file FILE-SOURCE$ drop {: src:ptr :}
   start begin dup end < while
      src over + c@ dup LF = over CR = or if else drop SP then OUT-C
      1+
   repeat drop
   out end start - file start TOK-LINE-V origin N@ TOK-COL-V origin N@ MAP+ ;

: PREV-SOURCE ( n n -- n ) {: start:n tok:n :}
   tok 1- begin dup 0 >= while
      dup TOK-START-V swap N@ start < if drop -1 exit then
      dup ANY-STRING? if exit then
      dup WORD? if exit then
      1-
   repeat drop -1 ;

: NEXT-WORD ( n n -- n ) {: tok:n end:n :}
   tok 1+ begin dup end < while
      dup WORD? if exit then
      1+
   repeat drop -1 ;

: ORIGIN-MARK ( n -- ) {: tok:n :}
   tok FRAME-END FRAME@ NEXT-WORD {: name:n :}
   name 0 < if E-DISC-MALFORMED tok FAIL-TOK then
   TOK-START-V tok N@ APPEND-RAW-TO
   LF OUT-C
   TOK-LINE-V name N@ OUT-N SP OUT-C
   TOK-COL-V name N@ OUT-N SP OUT-C
   TOK-START-V name N@ OUT-N
   s"  DIAG-ORIGIN!" OUT+
   LF OUT-C ;

: DEF-OPENER? ( n -- bool ) {: tok:n :}
   tok s" :" TOK=? if TRUE exit then
   tok s" +:" TOK=? if TRUE exit then
   tok s" TRUSTED:" TOK=? if TRUE exit then
   tok s" KERNEL:" TOK=? if TRUE exit then
   tok s" PRIM:" TOK=? ;

: DATA-DEFINER? ( n -- bool ) {: tok:n :}
   tok s" create" TOK=? if TRUE exit then
   tok s" variable" TOK=? if TRUE exit then
   tok s" constant" TOK=? if TRUE exit then
   tok s" LAYOUT-BUFFER" TOK=? ;

: DEF-CLOSER? ( n -- bool ) {: tok:n :}
   tok s" ;" TOK=? if TRUE exit then
   tok s" PRIM;" TOK=? ;

: LOADER? ( n -- bool ) {: tok:n :}
   tok s" include" TOK=? if TRUE exit then
   tok s" included" TOK=? if TRUE exit then
   tok s" require" TOK=? if TRUE exit then
   tok s" required" TOK=? if TRUE exit then
   tok s" provided" TOK=? ;

: LOADER-NAME? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   a u s" include" LINT-STR=CI if TRUE exit then
   a u s" included" LINT-STR=CI if TRUE exit then
   a u s" require" LINT-STR=CI if TRUE exit then
   a u s" required" LINT-STR=CI if TRUE exit then
   a u s" provided" LINT-STR=CI ;

: OPEN-STATE ( n -- ) {: tok:n :}
   -1 NAME-NEXT !
   tok s" undefine" TOK=? if exit then
   tok DEF-OPENER? if -1 DEF-OPEN ! -1 COMPILING ! then ;

: NAME-STEP ( n -- bool ) {: tok:n :}
   NAME-NEXT @ 0= if FALSE exit then
   tok WORD? 0= if FALSE exit then
   0 NAME-NEXT !
   tok LOADER? if E-DISC-SHADOW tok FAIL-TOK then
   TRUE ;

: STATE-STEP ( n -- bool ) {: tok:n :}
   tok NAME-STEP if TRUE exit then
   tok s" undefine" TOK=? if tok OPEN-STATE TRUE exit then
   tok DATA-DEFINER? if -1 NAME-NEXT ! TRUE exit then
   tok DEF-OPENER? if tok OPEN-STATE TRUE exit then
   tok s" [:" TOK=? if QUOTE-DEPTH @ 1+ QUOTE-DEPTH ! -1 COMPILING ! TRUE exit then
   tok s" ;]" TOK=? if
      QUOTE-DEPTH @ 1- dup 0 < if E-DISC-MALFORMED tok FAIL-TOK then
      QUOTE-DEPTH !
      QUOTE-DEPTH @ 0= DEF-OPEN @ 0= and if 0 COMPILING ! then
      TRUE exit
   then
   tok s" [" TOK=? if 0 COMPILING ! TRUE exit then
   tok s" ]" TOK=? if DEF-OPEN @ 0= 0= QUOTE-DEPTH @ 0 > or COMPILING ! TRUE exit then
   tok DEF-CLOSER? if
      0 DEF-OPEN !  0 COMPILING !  0 NAME-NEXT !
      TRUE exit
   then
   FALSE ;

: NAME-START ( n n -- n ) {: file:n tok:n :}
   TOK-END-V tok N@ WALK-I !
   file FILE-SOURCE$ {: src:ptr u:n :}
   begin WALK-I @ u < while
      src WALK-I @ + c@ SP <= if WALK-I @ 1+ WALK-I ! else WALK-I @ exit then
   repeat WALK-I @ ;

: IMM-PATH ( n n -- ptr u8 n n ) {: file:n tok:n :}
   tok FRAME-END FRAME@ NEXT-WORD {: ptok:n :}
   ptok 0 < if E-DISC-DYNAMIC tok FAIL-TOK then
   file tok NAME-START TOK-START-V ptok N@ <> if E-DISC-MALFORMED tok FAIL-TOK then
   ptok ANY-STRING? if E-DISC-MALFORMED ptok FAIL-TOK then
   ptok TOK$ {: path:ptr pathu:n :}
   path pathu file ptok PATH-CHECK
   path pathu TOK-END-V ptok N@ ;

: STACK-PATH ( n n -- ptr u8 n n ) {: start:n tok:n :}
   start tok PREV-SOURCE {: stok:n :}
   stok 0 < if E-DISC-DYNAMIC tok FAIL-TOK then
   stok STRING? 0= if
      stok ANY-STRING? if E-DISC-OPENER stok FAIL-TOK then
      E-DISC-DYNAMIC tok FAIL-TOK
   then
   stok DECODE-PATH TOK-START-V stok N@ ;

: RECORD-EVENT ( action registry-state ptr u8 n n -- )
   {: kind:action state:registry-state path:ptr pathu:n tok:n :}
   kind state TOK-FILE-V tok N@ TOK-LINE-V tok N@ TOK-COL-V tok N@
   TOK-START-V tok N@ path pathu EVENT+ ;

: REG-STATE ( ptr u8 n -- registry-state )
   REG-FIND 0 >= if construct registry-state known else construct registry-state fresh then ;

: INCLUDE-ACTION? ( action -- bool )
   MATCH action
      include OF TRUE ENDOF
      require OF FALSE ENDOF
      provide OF FALSE ENDOF
   ;MATCH ;

: REQUIRE-ACTION? ( action -- bool )
   MATCH action
      include OF FALSE ENDOF
      require OF TRUE ENDOF
      provide OF FALSE ENDOF
   ;MATCH ;

: REGISTRY-KNOWN? ( registry-state -- bool )
   MATCH registry-state
      fresh OF FALSE ENDOF
      known OF TRUE ENDOF
   ;MATCH ;

defer COMPOSE-FILE ( n -- )

: LOAD-ACTION ( action ptr u8 n n -- ) {: kind:action path:ptr pathu:n tok:n :}
   path pathu FAIL-TARGET!
   path pathu REG-STATE {: state:registry-state :}
   kind state path pathu tok RECORD-EVENT
   kind INCLUDE-ACTION? if
      LF OUT-C
      path pathu FILE-ID COMPOSE-FILE
      FRAME-FILE FRAME@ FILE-MARK
      LF OUT-C
      exit
   then
   state REGISTRY-KNOWN? if exit then
   path pathu REG-ADD
   kind REQUIRE-ACTION? if
      LF OUT-C
      path pathu FILE-ID COMPOSE-FILE
      FRAME-FILE FRAME@ FILE-MARK
      LF OUT-C
   then ;

: APPLY-ACTION ( action n n bool -- n ) {: kind:action start:n tok:n parse-name:bool :}
   tok FRAME-TOK FRAME!
   FRAME-FILE FRAME@ {: file:n :}
   parse-name if
      file tok IMM-PATH {: path:ptr pathu:n end:n :}
      start APPEND-RAW-TO
      kind path pathu tok LOAD-ACTION
      file start end tok APPEND-BLANK
      file end ADV-ORIGIN
      end
      exit
   then
   start tok PREV-SOURCE {: origin:n :}
   start tok STACK-PATH {: path:ptr pathu:n action-start:n :}
   action-start APPEND-RAW-TO
   kind path pathu tok LOAD-ACTION
   file action-start TOK-END-V tok N@ origin APPEND-BLANK
   file TOK-END-V tok N@ ADV-ORIGIN
   TOK-END-V tok N@ ;

: RETIRE-STEP ( n -- bool ) {: tok:n :}
   tok s" UNDEFINE-IF-DEFINED" TOK=? 0= if FALSE exit then
   FRAME-SRC FRAME@ tok PREV-SOURCE {: stok:n :}
   stok 0 < if E-DISC-RETIRE tok FAIL-TOK then
   stok STRING? 0= if E-DISC-RETIRE tok FAIL-TOK then
   stok DECODE-PATH LOADER-NAME? if E-DISC-RETIRE tok FAIL-TOK then
   TRUE ;

: TOKEN-STEP ( n -- n ) {: tok:n :}
   0 FAIL-TARGET-U !
   tok FRAME-TOK FRAME!
   tok WORD? 0= if tok 1+ exit then
   tok DEF-OPENER? if
      COMPILING @ 0 <> if E-DISC-DYNAMIC tok FAIL-TOK then
      tok ORIGIN-MARK
   then
   tok STATE-STEP if tok 1+ exit then
   COMPILING @ 0 <> tok LOADER? and if E-DISC-DYNAMIC tok FAIL-TOK then
   tok RETIRE-STEP if tok 1+ exit then
   tok s" include" TOK=? if construct action include TOK-START-V tok N@ tok TRUE APPLY-ACTION drop tok 2 + exit then
   tok s" require" TOK=? if construct action require TOK-START-V tok N@ tok TRUE APPLY-ACTION drop tok 2 + exit then
   tok s" included" TOK=? if construct action include FRAME-SRC FRAME@ tok FALSE APPLY-ACTION drop tok 1+ exit then
   tok s" required" TOK=? if construct action require FRAME-SRC FRAME@ tok FALSE APPLY-ACTION drop tok 1+ exit then
   tok s" provided" TOK=? if construct action provide FRAME-SRC FRAME@ tok FALSE APPLY-ACTION drop tok 1+ exit then
   tok 1+ ;

: FRAME-PUSH ( n -- ) {: file:n :}
   DEPTH @ INCLUDE-MAX-DEPTH >= if E-DISC-CAPACITY file FAIL-FILE0 then
   file FRAME-FILE FRAME-PUSH!
   FILE-TOK-V file N@ FRAME-TOK FRAME-PUSH!
   FILE-TOK-V file N@ FILE-TOK-N-V file N@ + FRAME-END FRAME-PUSH!
   0 FRAME-SRC FRAME-PUSH!
   1 FRAME-LINE FRAME-PUSH!
   1 FRAME-COL FRAME-PUSH!
   DEPTH @ 1+ DEPTH ! ;

: FILE-ACTIVE? ( n -- bool )
   FILE-ACTIVE-V swap N@ 0= 0= ;

: FILE-ACTIVE! ( bool n -- ) {: active:bool file:n :}
   active if -1 else 0 then FILE-ACTIVE-V file N! ;

: COMPOSE-FILE-IMPL ( n -- ) {: file:n :}
   file FILE-ACTIVE? if
      DEPTH @ 0 > if E-DISC-CYCLE FRAME-TOK FRAME@ FAIL-TOK then
      E-DISC-CYCLE file FAIL-FILE0
   then
   TRUE file FILE-ACTIVE!
   file FRAME-PUSH
   file FILE-MARK
   begin FRAME-TOK FRAME@ FRAME-END FRAME@ < while
      FRAME-TOK FRAME@ TOKEN-STEP FRAME-TOK FRAME!
   repeat
   FILE-SRC-U-V file N@ APPEND-RAW-TO
   DEPTH @ 1- DEPTH !
   FALSE file FILE-ACTIVE!
   ;

: INSTALL-COMPOSER ( -- )
   [: COMPOSE-FILE-IMPL ;] is COMPOSE-FILE ;

INSTALL-COMPOSER

: MAP-ROOM ( n -- ) {: add:n :}
   MAP-U @ add + MAP-U @ < if E-DISC-CAPACITY FAIL-FILE @ FAIL-LINE @ FAIL-COL @ FAIL-BYTE @ FAIL! then
   MAP-U @ add + MAP-CAP > if E-DISC-CAPACITY FAIL-FILE @ FAIL-LINE @ FAIL-COL @ FAIL-BYTE @ FAIL! then ;

: MAP-BYTES+ ( ptr u8 n -- ) {: a:ptr u:n :}
   u MAP-ROOM
   a MAP-A @ MAP-U @ + u BYTE-COPY
   MAP-U @ u + MAP-U ! ;

: MAP-C+ ( n -- ) {: c:n :}
   1 MAP-ROOM
   c MAP-A @ MAP-U @ + c!
   MAP-U @ 1+ MAP-U ! ;

: MAP-U+ ( n -- )
   U$ MAP-BYTES+ ;

: MAP-HEX+ ( ptr u8 n -- ) {: a:ptr u:n :}
   0 begin dup u < while
      a over + c@ {: c:n :}
      c 4 rshift $F and NIB>HEX MAP-C+
      c $F and NIB>HEX MAP-C+
      1+
   repeat drop ;

: MAP-FIELD ( n -- )
   MAP-U+
   TAB MAP-C+ ;

: RENDER-MAP-ROW ( n -- ) {: row:n :}
   MAP-OUT-V row N@ MAP-FIELD
   MAP-U-V row N@ MAP-FIELD
   MAP-SRC-V row N@ MAP-FIELD
   MAP-LINE-V row N@ MAP-FIELD
   MAP-COL-V row N@ MAP-FIELD
   MAP-FILE-V row N@ FILE-PATH$ MAP-HEX+
   LF MAP-C+ ;

: RENDER-MAP ( -- )
   0 MAP-U !
   s" HABUMAP1\n" MAP-BYTES+
   0 MAP-I !
   begin MAP-I @ MAP-N < while
      MAP-I @ RENDER-MAP-ROW
      MAP-I @ 1+ MAP-I !
   repeat
   MAP-A @ MAP-U @ MAP-DIGEST-BUF SHA256 ;

: DIGEST-U64 ( n -- ) {: value:n :}
   value INT-BUF BE64!
   INT-BUF 8 SHA256-UPDATE ;

: DIGEST-BYTES ( ptr u8 n -- ) {: a:ptr u:n :}
   u DIGEST-U64
   a u SHA256-UPDATE ;

: DIGEST-FILES ( -- )
   0 DIGEST-I !
   begin DIGEST-I @ FILE-N < while
      DIGEST-I @ FILE-PATH$ DIGEST-BYTES
      FILE-SRC-U-V DIGEST-I @ N@ DIGEST-U64
      FILE-DG-V DIGEST-I @ A@ DIGEST-U SHA256-UPDATE
      DIGEST-I @ 1+ DIGEST-I !
   repeat ;

: DIGEST-EVENTS ( -- )
   EVENT-A @ EVENT-U @ SHA256-UPDATE ;

: DIGEST-PLAN ( -- )
   SHA256-RESET
   s" source-compose-v1" SHA256-UPDATE
   DIGEST-FILES
   DIGEST-EVENTS
   OUT-A @ OUT-U @ SHA256-UPDATE
   MAP-DIGEST-BUF DIGEST-U SHA256-UPDATE
   PLAN-DIGEST-BUF SHA256-FINAL ;

: RESET ( -- )
   ENSURE-VECTORS
   OUT-A @ 0= if OUT-CAP MEM:BYTES-ALLOC-LEN MEM:ALLOC-BYTES drop OUT-A ! then
   MAP-A @ 0= if MAP-CAP MEM:BYTES-ALLOC-LEN MEM:ALLOC-BYTES drop MAP-A ! then
   EVENT-A @ 0= if EVENT-CAP MEM:BYTES-ALLOC-LEN MEM:ALLOC-BYTES drop EVENT-A ! then
   0 OUT-U !
   0 MAP-U !
   0 EVENT-U !
   0 FREEZE-U !
   0 DEPTH !
   0 COMPILING !
   0 DEF-OPEN !
   0 QUOTE-DEPTH !
   0 NAME-NEXT !
   0 FAIL-CODE !
   -1 FAIL-FILE !
   1 FAIL-LINE !
   1 FAIL-COL !
   0 FAIL-BYTE !
   0 FAIL-TARGET-U ! ;

public

: BUILD ( ptr u8 n -- ) {: path:ptr pathu:n :}
   RESET
   path pathu 0 -1 PATH-CHECK
   path pathu FILE-ID COMPOSE-FILE
   DEF-OPEN @ 0= 0= QUOTE-DEPTH @ 0 > or if E-DISC-MALFORMED 0 FAIL-FILE0 then
   RENDER-MAP
   DIGEST-PLAN ;

: SOURCE$ ( -- ptr u8 n )
   OUT-A @ OUT-U @ ;

: MAP$ ( -- ptr u8 n )
   MAP-A @ MAP-U @ ;

: DIGEST$ ( -- ptr u8 n )
   PLAN-DIGEST-BUF DIGEST-U ;

: MAP-DIGEST$ ( -- ptr u8 n )
   MAP-DIGEST-BUF DIGEST-U ;

: WRITE-SOURCE ( ptr u8 n -- )
   SOURCE$ WRITE-ALL ;

: WRITE-MAP ( ptr u8 n -- )
   MAP$ WRITE-ALL ;

: KEY+ ( -- )
   s" source-compose-v1" CK-TEXT+
   PLAN-DIGEST-BUF CK-DIGEST+
   MAP-DIGEST-BUF CK-DIGEST+ ;

: FAILURE ( -- n n n n n )
   FAIL-CODE @ FAIL-FILE @ FAIL-LINE @ FAIL-COL @ FAIL-BYTE @ ;

: FAILURE-FILE$ ( -- ptr u8 n )
   FAIL-FILE @ dup 0 < if drop s" <entry>" exit then
   FILE-PATH$ ;

private

: CODE$ ( n -- ptr u8 n ) {: code:n :}
   code E-DISC-SHADOW = if s" E-DISC-SHADOW" exit then
   code E-DISC-DYNAMIC = if s" E-DISC-DYNAMIC" exit then
   code E-DISC-OPENER = if s" E-DISC-OPENER" exit then
   code E-DISC-UNTERM = if s" E-DISC-UNTERM" exit then
   code E-DISC-CAPACITY = if s" E-DISC-CAPACITY" exit then
   code E-DISC-RETIRE = if s" E-DISC-RETIRE" exit then
   code E-DISC-CYCLE = if s" E-DISC-CYCLE" exit then
   code E-DISC-MALFORMED = if s" E-DISC-MALFORMED" exit then
   code E-DISC-NUL-PATH = if s" E-DISC-NUL-PATH" exit then
   code E-DISC-MISSING = if s" E-DISC-MISSING" exit then
   s" E-DISC-UNKNOWN" ;

: REASON$ ( n -- ptr u8 n ) {: code:n :}
   code E-DISC-SHADOW = if s" loader word is shadowed" exit then
   code E-DISC-DYNAMIC = if s" loader path is not a top-level literal" exit then
   code E-DISC-OPENER = if s" loader opener is invalid" exit then
   code E-DISC-UNTERM = if s" source contains an unterminated string" exit then
   code E-DISC-CAPACITY = if s" source composition exceeds a checked capacity" exit then
   code E-DISC-RETIRE = if s" loader retirement cannot be composed exactly" exit then
   code E-DISC-CYCLE = if s" source dependency cycle" exit then
   code E-DISC-MALFORMED = if s" malformed source loader" exit then
   code E-DISC-NUL-PATH = if s" source path contains NUL" exit then
   code E-DISC-MISSING = if s" source dependency is missing" exit then
   s" source composition failed" ;

public

: CHAIN-N ( -- n )
   DEPTH @ FAIL-TARGET-U @ 0 > if 1+ then ;

: CHAIN-FILE$ ( n -- ptr u8 n ) {: idx:n :}
   idx 0 < if E-VEC-BOUNDS throw then
   idx DEPTH @ < if FRAME-FILE idx cells + @ FILE-PATH$ exit then
   idx DEPTH @ = FAIL-TARGET-U @ 0 > and if FAIL-TARGET-BUF FAIL-TARGET-U @ exit then
   E-VEC-BOUNDS throw ;

private

: TEXT-N+ ( n -- )
   U$ SB-APPEND ;

: TEXT-CHAIN+ ( -- )
   CHAIN-N 0= if exit then
   s" include chain: " SB-APPEND
   0 begin dup CHAIN-N < while
      dup 0 > if s" ->" SB-APPEND then
      dup CHAIN-FILE$ SB-APPEND
      1+
   repeat drop
   LF SB-APPEND-C ;

public

: TEXT$ ( -- ptr u8 n )
   SB-RESET
   FAIL-CODE @ CODE$ SB-APPEND
   SP SB-APPEND-C
   FAILURE-FILE$ SB-APPEND
   $3A SB-APPEND-C FAIL-LINE @ TEXT-N+
   $3A SB-APPEND-C FAIL-COL @ TEXT-N+
   s"  byte " SB-APPEND FAIL-BYTE @ TEXT-N+
   s" : " SB-APPEND FAIL-CODE @ REASON$ SB-APPEND
   LF SB-APPEND-C
   TEXT-CHAIN+
   SB$ ;

private

: JSON-CHAIN ( -- )
   JW-ARRAY-START
   0 begin dup CHAIN-N < while
      dup 0 > if JW-COMMA then
      dup CHAIN-FILE$ JW-STRING
      1+
   repeat drop
   JW-ARRAY-END ;

public

: JSON$ ( -- ptr u8 n )
   JW-RESET
   JW-OBJECT-START
   s" schema_version" 1 JW-FIELD-U JW-COMMA
   s" code" FAIL-CODE @ CODE$ JW-FIELD-S JW-COMMA
   s" verdict" s" rejected" JW-FIELD-S JW-COMMA
   s" file" FAILURE-FILE$ JW-FIELD-S JW-COMMA
   s" line" FAIL-LINE @ JW-FIELD-U JW-COMMA
   s" column" FAIL-COL @ JW-FIELD-U JW-COMMA
   s" byte_start" FAIL-BYTE @ JW-FIELD-U JW-COMMA
   s" reason" FAIL-CODE @ REASON$ JW-FIELD-S JW-COMMA
   s" include_chain" JW-KEY JSON-CHAIN
   JW-OBJECT-END
   JW$ ;



;package
