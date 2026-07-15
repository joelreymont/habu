\ icode-fixup-test.f - ARM64 pending-fixup chain and free-list regression.
\ Run: bin/hb --load test/icode-fixup-test.f

require lib/errors.f
require lib/string.f
require lib/test.f
require lib/test/outcome.f
require lib/memory.f
require lib/process.f
require lib/process-argv.f

package ICODE-FIXUP-LOAD

: WORD? ( ptr u8 n -- bool )
   XREF-FIND 0= 0= ;

public

: LOAD ( -- )
   s" ASM-INIT" WORD? if exit then
   s" src/arch/arm64/asm.f" included
   s" src/arch/arm64/icode.f" included
   s" src/arch/arm64/mnem.f" included ;

;package

ICODE-FIXUP-LOAD:LOAD

package ICODE-FIXUP-TEST

$801 constant SEQ-N
$28000 constant WANT-TAB-BYTES
$1000 constant CAPTURE-CAP
10000 constant TIMEOUT-MS

create OUT CAPTURE-CAP allot
create ERR CAPTURE-CAP allot
variable WP

: WP@ ( -- ptr u8 )
   WP 0 ptr-field @ ;

: WORD@ ( n -- n )
   CW@ WP 0 ptr-field !
   WP@ c@
   WP@ 1 CODE-BYTE+ c@ $8 lshift or
   WP@ 2 CODE-BYTE+ c@ $10 lshift or
   WP@ 3 CODE-BYTE+ c@ $18 lshift or ;

: SLOT@ ( ptr n n -- n )
   cells + @ ;

: EMIT-PAIR ( -- n )
   ASM-CP @
   LBL dup B, dup B, LBL, ;

: PAIR= ( n -- )
   dup WORD@ $14000002 T=
   1 + WORD@ $14000001 T= ;

: TEST-SEQUENTIAL ( -- )
   ASM-INIT
   0 begin dup SEQ-N < while
      EMIT-PAIR PAIR=
      1 +
   repeat drop
   ASM-CP @ SEQ-N 2 * T=
   NFX @ 0 T=
   FX-NEW @ 2 T=
   FX-FREE @ 0 T=
   ICODE-TAB-BYTES WANT-TAB-BYTES T= ;

: TEST-MIXED-PENDING ( label label -- )
   {: a:label b:label :}
   NFX @ 6 T=
   FX-NEW @ 6 T=
   FX-FREE @ -1 T=
   FXH a LABEL>N SLOT@ 4 T=
   FXN 4 SLOT@ 2 T=
   FXN 2 SLOT@ 0 T=
   FXN 0 SLOT@ -1 T=
   FXH b LABEL>N SLOT@ 5 T=
   FXN 5 SLOT@ 3 T=
   FXN 3 SLOT@ 1 T=
   FXN 1 SLOT@ -1 T=
   FXS 0 SLOT@ 0 T=  FXK 0 SLOT@ 0 T=
   FXS 1 SLOT@ 1 T=  FXK 1 SLOT@ 1 T=
   FXS 2 SLOT@ 2 T=  FXK 2 SLOT@ 1 T=
   FXS 3 SLOT@ 3 T=  FXK 3 SLOT@ 1 T=
   FXS 4 SLOT@ 4 T=  FXK 4 SLOT@ 2 T=
   FXS 5 SLOT@ 5 T=  FXK 5 SLOT@ 0 T= ;

: TEST-A-RESOLVED ( label label -- )
   {: a:label b:label :}
   NFX @ 3 T=
   FXH a LABEL>N SLOT@ -1 T=
   FXH b LABEL>N SLOT@ 5 T=
   FXN 5 SLOT@ 3 T=
   FXN 3 SLOT@ 1 T=
   FXN 1 SLOT@ -1 T=
   0 WORD@ $14000006 T=
   1 WORD@ $54000000 T=
   2 WORD@ $B4000083 T=
   3 WORD@ $B5000004 T=
   4 WORD@ $10000045 T=
   5 WORD@ $94000000 T= ;

: TEST-MIXED-FREE ( -- )
   FX-FREE @ 1 T=
   FXN 1 SLOT@ 3 T=
   FXN 3 SLOT@ 5 T=
   FXN 5 SLOT@ 0 T=
   FXN 0 SLOT@ 2 T=
   FXN 2 SLOT@ 4 T=
   FXN 4 SLOT@ -1 T= ;

: TEST-MIXED ( -- )
   ASM-INIT
   LBL {: a:label :}
   LBL {: b:label :}
   a B,
   C-EQ b BCOND,
   3 a CBZ,
   4 b CBNZ,
   5 a ADR,
   b BL,
   a b TEST-MIXED-PENDING
   a LBL,
   a b TEST-A-RESOLVED
   b LBL,
   NFX @ 0 T=
   1 WORD@ $540000A0 T=
   3 WORD@ $B5000064 T=
   5 WORD@ $94000001 T=
   TEST-MIXED-FREE ;

: TEST-BACKWARD ( -- )
   ASM-INIT
   LBL {: target:label :}
   target LBL,
   LBLP target LABEL>N SLOT@ 0 T=
   0 EMITW
   target B,
   target BL,
   C-EQ target BCOND,
   3 target CBZ,
   4 target CBNZ,
   5 target ADR,
   ASM-CP @ 7 T=
   0 WORD@ 0 T=
   1 WORD@ $17FFFFFF T=
   2 WORD@ $97FFFFFE T=
   3 WORD@ $54FFFFA0 T=
   4 WORD@ $B4FFFF83 T=
   5 WORD@ $B5FFFF64 T=
   6 WORD@ $10FFFF45 T=
   NFX @ 0 T=
   FX-FREE @ -1 T= ;

: TEST-REBIND-STATE ( label n -- )
   {: target:label words:n :}
   target LBL-BOUND? TTRUE
   LBLP target LABEL>N SLOT@ 2 T=
   LBLP target LABEL>N 1 + SLOT@ -1 T=
   FXH target LABEL>N SLOT@ -1 T=
   FXH target LABEL>N 1 + SLOT@ 1 T=
   FX-FREE @ 0 T=
   NFX @ 1 T=
   FX-NEW @ 2 T=
   FXN 0 SLOT@ -1 T=
   FXN 1 SLOT@ -1 T=
   FXS 0 SLOT@ 0 T=
   FXK 0 SLOT@ 0 T=
   FXS 1 SLOT@ 1 T=
   FXK 1 SLOT@ 0 T=
   ASM-CP @ words T=
   0 WORD@ $14000002 T=
   1 WORD@ $14000000 T=
   words 3 = if 2 WORD@ 0 T= then ;

: TEST-REBIND-AT ( bool -- )
   ASM-INIT
   LBL {: target:label :}
   LBL {: pending:label :}
   target B,
   pending B,
   target LBL,
   if 0 EMITW 3 else 2 then
   target swap TEST-REBIND-STATE ;

: FULL-WANT ( n -- n )
   ICODE-TAB-CELLS swap - $14000000 or ;

: TEST-FULL-WORDS ( -- )
   0 begin dup ICODE-TAB-CELLS < while
      dup WORD@ over FULL-WANT T=
      1 +
   repeat drop ;

: TEST-FULL-FREE ( -- )
   FX-FREE @ 0 T=
   0 begin dup ICODE-TAB-CELLS 1 - < while
      dup FXN swap SLOT@ over 1 + T=
      1 +
   repeat drop
   FXN ICODE-TAB-CELLS 1 - SLOT@ -1 T= ;

: TEST-FULL ( -- )
   ASM-INIT
   LBL {: target:label :}
   ICODE-TAB-CELLS 0 ?do target B, loop
   NFX @ ICODE-TAB-CELLS T=
   FX-NEW @ ICODE-TAB-CELLS T=
   FX-FREE @ -1 T=
   FXH target LABEL>N SLOT@ ICODE-TAB-CELLS 1 - T=
   target LBL,
   NFX @ 0 T=
   TEST-FULL-WORDS
   TEST-FULL-FREE ;

: EMIT-OVERFLOW ( -- )
   ASM-INIT
   LBL {: target:label :}
   ICODE-TAB-CELLS 1 + 0 ?do target B, loop ;

: EMIT-CORRUPT-LOW ( -- )
   ASM-INIT
   -2 FX-FREE !
   LBL B, ;

: EMIT-CORRUPT-FUTURE ( -- )
   ASM-INIT
   0 FX-FREE !
   LBL B, ;

: EMIT-REDEFINE ( bool -- )
   ASM-INIT
   LBL {: target:label :}
   LBL {: pending:label :}
   target B,
   pending B,
   target LBL,
   if 0 EMITW then
   target LBL, ;

: CHILD-MODE? ( ptr u8 n -- bool )
   SCRIPT-ARGC 1 <> if 2drop 0 0= 0= exit then
   0 SCRIPT-ARGV$ 2swap STR= ;

: ARG+ ( ptr u8 n -- )
   >LEN PROC-ARGV+ ;

: HB$ ( -- ptr u8 n )
   s" HABU_UNDER_TEST" GETENV
   dup 0= if 2drop s" bin/hb" then ;

: TEST-DIAG ( ptr u8 n ptr u8 n -- )
   \ typed-local-lint: allow-bare-local - counted strings retain ptr-u8 roles
   {: mode modeu want wantu :}
   PROC-ARGV-RESET
   s" --load" ARG+
   s" test/icode-fixup-test.f" ARG+
   s" --" ARG+
   mode modeu ARG+
   HB$ >LEN OUT CAPTURE-CAP >LEN ERR CAPTURE-CAP >LEN TIMEOUT-MS >MS
   RUN-ARGV-CAPTURE-OUTCOME
   ICODE-EXIT-RC T-OUTCOME-EXITED=
   LEN>N {: erru:n :}
   LEN>N {: outu:n :}
   outu 0 T=
   ERR erru want wantu T$= ;

: TEST-OVERFLOW ( -- )
   s" overflow" s" icode: out of fixups" TEST-DIAG ;

: TEST-CORRUPT ( -- )
   s" corrupt-low" s" icode: fixup free list corrupt" TEST-DIAG
   s" corrupt-future" s" icode: fixup free list corrupt" TEST-DIAG ;

: TEST-REDEFINE ( -- )
   0 0= 0= TEST-REBIND-AT
   0 0= TEST-REBIND-AT
   s" redefine-same" s" icode: label redefined" TEST-DIAG
   s" redefine-different" s" icode: label redefined" TEST-DIAG ;

: MAIN ( -- )
   T-RESET
   TEST-SEQUENTIAL
   TEST-MIXED
   TEST-BACKWARD
   TEST-REDEFINE
   TEST-FULL
   TEST-OVERFLOW
   TEST-CORRUPT
   T-REPORT
   s" icode-fixup-test: ok" type cr ;

: RUN ( -- )
   s" overflow" CHILD-MODE? if EMIT-OVERFLOW exit then
   s" corrupt-low" CHILD-MODE? if EMIT-CORRUPT-LOW exit then
   s" corrupt-future" CHILD-MODE? if EMIT-CORRUPT-FUTURE exit then
   s" redefine-same" CHILD-MODE? if 0 0= 0= EMIT-REDEFINE exit then
   s" redefine-different" CHILD-MODE? if 0 0= EMIT-REDEFINE exit then
   MAIN ;

RUN

;package
