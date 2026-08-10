\ icode-fixup-test.f - ARM64 pending-fixup chain and free-list regression.
\ Run: bin/hb --load test/icode-fixup-test.f

require lib/errors.f
require lib/string.f
require lib/test.f
require lib/test/outcome.f
require lib/memory.f
require lib/process.f
require lib/process-argv.f

\ The three ARM64 encoder sources are `require`d, not probed-and-included.
\ A probe on ASM-INIT decided whether to load asm.f, but ASM-INIT is defined in
\ icode.f, so the probe only ever answered "is icode.f loaded" - it reported
\ asm.f absent whenever the two were not loaded together, and loading asm.f a
\ second time is a duplicate definition, not a no-op. `require` asks the
\ registry that actually records what is loaded, which is also the registry the
\ ten-plus existing `require src/arch/arm64/asm.f` sites already share.
require src/arch/arm64/asm.f
require src/arch/arm64/icode.f
require src/arch/arm64/mnem.f

package ICODE-FIXUP-TEST
\ The ARM64 encoders are package A64ASM's public surface (src/arch/arm64/asm.f).
using A64ASM

$801 constant SEQ-N
$28000 constant WANT-TAB-BYTES
$1000 constant CAPTURE-CAP
10000 constant TIMEOUT-MS
FX-ADR 1 + constant KIND-BAD     \ first kind past FX-ADR: the historic silent-ADR value

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
   FXS 0 SLOT@ 0 T=  FXK 0 SLOT@ FX-B26 T=
   FXS 1 SLOT@ 1 T=  FXK 1 SLOT@ FX-B19 T=
   FXS 2 SLOT@ 2 T=  FXK 2 SLOT@ FX-B19 T=
   FXS 3 SLOT@ 3 T=  FXK 3 SLOT@ FX-B19 T=
   FXS 4 SLOT@ 4 T=  FXK 4 SLOT@ FX-ADR T=
   FXS 5 SLOT@ 5 T=  FXK 5 SLOT@ FX-B26 T= ;

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

: TEST-KIND-VALIDATE ( -- )
   FX-B26 FX-KIND-OK? TTRUE
   FX-B19 FX-KIND-OK? TTRUE
   FX-ADR FX-KIND-OK? TTRUE
   KIND-BAD FX-KIND-OK? TFALSE
   -1 FX-KIND-OK? TFALSE
   $7F FX-KIND-OK? TFALSE ;

: TEST-KIND-GUARD-PURE ( -- )
   ASM-INIT
   LBL {: t:label :}
   t B,  t B,
   NFX @ 2 T=  FX-NEW @ 2 T=  FX-FREE @ -1 T=  ASM-CP @ 2 T=
   KIND-BAD FX-KIND-OK? TFALSE                  \ pure check rejects, mutates nothing
   NFX @ 2 T=  FX-NEW @ 2 T=  FX-FREE @ -1 T=  ASM-CP @ 2 T= ;

\ signed-reach predicates: max delta on the valid side is in reach; one beyond
\ each boundary (LO-1 and HI) is rejected. LO is inclusive, HI exclusive.
: TEST-REACH-VALIDATE ( -- )
   REL26-HI 1 - REL26-OK? TTRUE   REL26-LO REL26-OK? TTRUE      \ max +/- word delta
   REL26-HI REL26-OK? TFALSE      REL26-LO 1 - REL26-OK? TFALSE \ one beyond each
   REL19-HI 1 - REL19-OK? TTRUE   REL19-LO REL19-OK? TTRUE
   REL19-HI REL19-OK? TFALSE      REL19-LO 1 - REL19-OK? TFALSE
   ADR-HI 1 - ADR-OK? TTRUE       ADR-LO ADR-OK? TTRUE          \ byte delta
   ADR-HI ADR-OK? TFALSE          ADR-LO 1 - ADR-OK? TFALSE ;

\ a pure out-of-reach check mutates no fixup/code state (mirrors the kind guard)
: TEST-REACH-PURE ( -- )
   ASM-INIT
   LBL {: t:label :}
   t B,  t B,
   NFX @ 2 T=  FX-NEW @ 2 T=  FX-FREE @ -1 T=  ASM-CP @ 2 T=
   REL19-HI REL19-OK? TFALSE
   NFX @ 2 T=  FX-NEW @ 2 T=  FX-FREE @ -1 T=  ASM-CP @ 2 T= ;

\ exact-boundary encodings through the real emitters (pin the instruction word).
\ Backward (immediate BR-EMIT/ADR,) exercises the max-negative delta; forward
\ (deferred FX-ENC via LBL,) exercises the max-positive delta. Label positions
\ are crafted via ASM-CP so a boundary delta needs no MB-scale code emission;
\ deferred PATCH still writes the real site word (FXS = word 0).
: ENC-REL19-MAXNEG ( -- )                        \ immediate backward, delta = -2^18
   ASM-INIT  LBL {: t:label :}
   t LBL,  REL19-HI ASM-CP !  C-EQ t BCOND,
   REL19-HI WORD@ $54800000 T= ;

: ENC-REL19-MAXPOS ( -- )                        \ deferred forward, delta = 2^18 - 1
   ASM-INIT  LBL {: t:label :}
   C-EQ t BCOND,  REL19-HI 1 - ASM-CP !  t LBL,
   0 WORD@ $547FFFE0 T= ;

: ENC-REL26-MAXPOS ( -- )                        \ deferred forward, delta = 2^25 - 1
   ASM-INIT  LBL {: t:label :}
   t B,  REL26-HI 1 - ASM-CP !  t LBL,
   0 WORD@ $15FFFFFF T= ;

: ENC-REL26-MAXNEG ( -- )                        \ deferred, crafted negative bind, delta = -2^25
   ASM-INIT  LBL {: t:label :}
   t B,  REL26-LO ASM-CP !  t LBL,
   0 WORD@ $16000000 T= ;

: ENC-ADR-MAXPOS ( -- )                          \ deferred forward, byte delta = (2^18-1)*4
   ASM-INIT  LBL {: t:label :}
   5 t ADR,  ADR-HI 4 / 1 - ASM-CP !  t LBL,
   0 WORD@ $107FFFE5 T= ;

: ENC-ADR-MAXNEG ( -- )                          \ immediate backward, byte delta = -2^20
   ASM-INIT  LBL {: t:label :}
   t LBL,  ADR-HI 4 / ASM-CP !  5 t ADR,
   ADR-HI 4 / WORD@ $10800005 T= ;

: TEST-REACH-ENCODE ( -- )
   ENC-REL19-MAXNEG  ENC-REL19-MAXPOS
   ENC-REL26-MAXPOS  ENC-REL26-MAXNEG
   ENC-ADR-MAXPOS    ENC-ADR-MAXNEG ;

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
   FXK 0 SLOT@ FX-B26 T=
   FXS 1 SLOT@ 1 T=
   FXK 1 SLOT@ FX-B26 T=
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

\ producer path: FX+ must reject an invalid kind before touching any state
: EMIT-BADKIND ( -- )
   ASM-INIT
   LBL {: target:label :}
   0 target KIND-BAD FX+ ;

\ consumer path: a corrupt FXK kind must fail closed at patch time, not patch ADR
: EMIT-BADKIND-PATCH ( -- )
   ASM-INIT
   LBL {: target:label :}
   target B,
   KIND-BAD 0 cells FXK + !
   target LBL, ;

\ out-of-reach fixtures: one-beyond each signed-reach boundary must die (exit 72),
\ never wrap. Forward cases fail at the deferred chokepoint (FX-ENC via LBL,),
\ backward cases at the immediate site (BR-EMIT/ADR,) before EMITW. REL26 reach
\ (2^25 words) exceeds the code window, so its bind position is crafted directly.
: EMIT-REL19-FAR-FWD ( -- )                      \ forward delta = 2^18 -> deferred die
   ASM-INIT
   LBL {: t:label :}
   C-EQ t BCOND,  REL19-HI ASM-CP !  t LBL, ;

: EMIT-REL19-FAR-BACK ( -- )                     \ backward delta = -(2^18+1) -> immediate die
   ASM-INIT
   LBL {: t:label :}
   t LBL,  REL19-HI 1 + ASM-CP !  C-EQ t BCOND, ;

: EMIT-REL26-FAR-FWD ( -- )                      \ forward delta = 2^25 -> deferred die
   ASM-INIT
   LBL {: t:label :}
   t B,  REL26-HI ASM-CP !  t LBL, ;

: EMIT-REL26-FAR-BACK ( -- )                     \ crafted delta = -(2^25+1) -> deferred die
   ASM-INIT
   LBL {: t:label :}
   t B,  REL26-LO 1 - ASM-CP !  t LBL, ;

: EMIT-ADR-FAR-FWD ( -- )                        \ forward byte delta = 2^20 -> deferred die
   ASM-INIT
   LBL {: t:label :}
   5 t ADR,  ADR-HI 4 / ASM-CP !  t LBL, ;

: EMIT-ADR-FAR-BACK ( -- )                       \ backward byte delta = -(2^20+4) -> immediate die
   ASM-INIT
   LBL {: t:label :}
   t LBL,  ADR-HI 4 / 1 + ASM-CP !  5 t ADR, ;

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

: TEST-BADKIND ( -- )
   s" badkind" s" icode: invalid fixup kind" TEST-DIAG
   s" badkind-patch" s" icode: invalid fixup kind" TEST-DIAG ;

: TEST-REACH-DIAG ( -- )
   s" rel19-far-fwd"  s" icode: cond branch out of reach" TEST-DIAG
   s" rel19-far-back" s" icode: cond branch out of reach" TEST-DIAG
   s" rel26-far-fwd"  s" icode: branch out of reach" TEST-DIAG
   s" rel26-far-back" s" icode: branch out of reach" TEST-DIAG
   s" adr-far-fwd"    s" icode: adr out of reach" TEST-DIAG
   s" adr-far-back"   s" icode: adr out of reach" TEST-DIAG ;

: MAIN ( -- )
   T-RESET
   TEST-SEQUENTIAL
   TEST-MIXED
   TEST-BACKWARD
   TEST-KIND-VALIDATE
   TEST-KIND-GUARD-PURE
   TEST-REACH-VALIDATE
   TEST-REACH-PURE
   TEST-REACH-ENCODE
   TEST-REDEFINE
   TEST-FULL
   TEST-OVERFLOW
   TEST-CORRUPT
   TEST-BADKIND
   TEST-REACH-DIAG
   T-REPORT
   s" icode-fixup-test: ok" type cr ;

: RUN ( -- )
   s" overflow" CHILD-MODE? if EMIT-OVERFLOW exit then
   s" corrupt-low" CHILD-MODE? if EMIT-CORRUPT-LOW exit then
   s" corrupt-future" CHILD-MODE? if EMIT-CORRUPT-FUTURE exit then
   s" redefine-same" CHILD-MODE? if 0 0= 0= EMIT-REDEFINE exit then
   s" redefine-different" CHILD-MODE? if 0 0= EMIT-REDEFINE exit then
   s" badkind" CHILD-MODE? if EMIT-BADKIND exit then
   s" badkind-patch" CHILD-MODE? if EMIT-BADKIND-PATCH exit then
   s" rel19-far-fwd" CHILD-MODE? if EMIT-REL19-FAR-FWD exit then
   s" rel19-far-back" CHILD-MODE? if EMIT-REL19-FAR-BACK exit then
   s" rel26-far-fwd" CHILD-MODE? if EMIT-REL26-FAR-FWD exit then
   s" rel26-far-back" CHILD-MODE? if EMIT-REL26-FAR-BACK exit then
   s" adr-far-fwd" CHILD-MODE? if EMIT-ADR-FAR-FWD exit then
   s" adr-far-back" CHILD-MODE? if EMIT-ADR-FAR-BACK exit then
   MAIN ;

RUN

;using
;package
