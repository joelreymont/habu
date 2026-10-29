\ chain-scale.f - how the native chain's spill rewrite grows with the module it
\ rewrites, as one slope per body shape. Only idle-machine runs compare.
\   bin/hb --load tools/chain-scale.f [-- BOUND]   with BOUND (1.1): refuse a
\                                                   larger slope, E-SCALE-EXCEEDED
\   scale: <family> k <k> ops <n> plan <n> calls <n> ns-per-call <n> ns-per-handled <n>
\   scale: slope spill-line <milli> spill-frame <milli> compiled <n>

require src/lib/errors.f
require src/lib/string.f
require src/lib/fmt.f
require src/lib/float.f
require src/lib/fmath.f
require src/lib/argv.f
require src/lib/fd.f

-7703 constant E-SCALE-UNCOMPILED
-7704 constant E-SCALE-EXCEEDED
-7705 constant E-SCALE-CAPACITY

package CHAIN-SCALE
private

20 constant DEFS-MAX               \ definitions in the largest measured set
8 constant POINTS-MAX              \ family members
$8000 constant SRC-CAP             \ source bytes for one set
1000 constant MILLI
$0A constant LF
74 constant IO-RC                  \ sysexits EX_IOERR, as tools/lint/text.f uses it

28 constant LIVE-N                 \ values live at once in both spill families

create SRC SRC-CAP allot
create SRC-OFF DEFS-MAX cells allot
create SRC-LEN DEFS-MAX cells allot
variable SRC-FILL
variable SET-N                     \ definitions in the set being measured

create PT-K POINTS-MAX cells allot
create PT-OPS POINTS-MAX cells allot
create PT-PLAN POINTS-MAX cells allot
create PT-NS POINTS-MAX cells allot
create PT-CALLS POINTS-MAX cells allot
variable PT-N

variable NC-COUNT                  \ NCOMP:COMPILE dispatches since MAIN took the cell
variable NC-MARK                   \ NC-COUNT when the current set opened
variable PRIOR-TIER

8 constant PREFIX-CAP
create LIVE-PREFIX PREFIX-CAP allot
variable PREFIX-U
variable SET-LIVE?

variable S-LINE                    \ the two slopes, in thousandths
variable S-FRAME
variable BOUND-MILLI               \ ratchet bound; read only when one was given

\ ---- the dispatch counter ----------------------------------------------------
\ `set-tier` is engine-global: a tier-1 selection that did not take would report
\ tier 0's number as the optimizing compiler's. Each set must add exactly its
\ own definition count or the tool refuses with E-SCALE-UNCOMPILED.
: COUNTING-COMPILE ( ptr u8 n -- )
   NC-COUNT @ 1+ NC-COUNT !
   NCOMP:COMPILE ;

\ ---- refusal ----------------------------------------------------------------
: EMIT-ERR ( ptr u8 n -- ) {: msg:ptr mu :}
   FD:STDERR FD>N msg mu write mu <> if
      s" chain-scale: stderr write failed" IO-RC die
   then ;

: REFUSE ( ptr u8 n n -- ) {: msg:ptr mu thrown :}
   msg mu EMIT-ERR
   thrown throw ;

\ ---- building one set, before the clock starts ------------------------------
: SRC+ ( ptr u8 n -- ) {: a:ptr u :}
   SRC-FILL @ u + SRC-CAP > if E-SCALE-CAPACITY throw then
   a SRC SRC-FILL @ + u BYTE-COPY
   SRC-FILL @ u + SRC-FILL ! ;

: CHUNK+ ( -- ) SB$ SRC+ ;

: DEF$ ( n -- ptr u8 n ) {: ix :}
   SRC SRC-OFF ix cells + @ +  SRC-LEN ix cells + @ ;

\ Each family keeps its shape (functions, blocks per function, live values) as k
\ grows: a slope means something only when size is what scales. LIVE-N values
\ live at once overflow the register file, so A64SPILL:REWRITE runs; k
\ additions inside each grow the operation count.
: LINE-BODY ( n -- ) {: k :}
   SB-RESET s"  ( n -- n ) " SB-APPEND CHUNK+
   LIVE-N 0 ?do
      SB-RESET s" dup " SB-APPEND CHUNK+
      k 0 ?do SB-RESET i 1+ FMT:SB-U s"  + " SB-APPEND CHUNK+ loop
      SB-RESET s" swap " SB-APPEND CHUNK+
   loop
   LIVE-N 0 ?do SB-RESET s" + " SB-APPEND CHUNK+ loop ;

\ The same live set across k conditional regions: 2k+1 blocks in one function,
\ where the pass's per-block and per-edge work lives. The allocator takes about
\ 90% of this family's wall time at k=4 and is not timed.
: FRAME-BODY ( n -- ) {: k :}
   SB-RESET s"  ( n -- n ) " SB-APPEND CHUNK+
   LIVE-N 0 ?do
      SB-RESET s" dup " SB-APPEND i 1+ FMT:SB-U s"  + swap " SB-APPEND CHUNK+
   loop
   k 0 ?do
      SB-RESET s" dup 5 < if " SB-APPEND i 1+ FMT:SB-U s"  + then " SB-APPEND
      CHUNK+
   loop
   LIVE-N 0 ?do SB-RESET s" + " SB-APPEND CHUNK+ loop ;

0 constant SH-LINE
1 constant SH-FRAME

: BODY ( n n -- ) {: shape k :}
   shape SH-LINE = if k LINE-BODY exit then
   k FRAME-BODY ;

: BUILD-ONE ( n n n -- ) {: shape k ix :}
   SRC-FILL @ SRC-OFF ix cells + !
   SB-RESET s" : " SB-APPEND  LIVE-PREFIX PREFIX-U @ SB-APPEND  ix 1+ FMT:SB-U
   CHUNK+
   shape k BODY
   SB-RESET s" ; " SB-APPEND CHUNK+
   SRC-FILL @  SRC-OFF ix cells + @ -  SRC-LEN ix cells + ! ;

: BUILD-SET ( ptr u8 n n n -- ) {: px:ptr pu shape k :}
   pu PREFIX-CAP > if E-SCALE-CAPACITY throw then
   SET-N @ DEFS-MAX > if E-SCALE-CAPACITY throw then
   px LIVE-PREFIX pu BYTE-COPY
   pu PREFIX-U !
   0 SRC-FILL !
   SET-N @ 0 ?do shape k i BUILD-ONE loop ;

\ A measured set is scaffolding, not a result: names the caller never asked for,
\ and a second set under the same prefix would die of a duplicate definition.
: UNDEF-ONE ( n -- ) {: ix :}
   SB-RESET
   s" undefine " SB-APPEND  LIVE-PREFIX PREFIX-U @ SB-APPEND  ix FMT:SB-U
   SB$ evaluate-closed ;

: TEARDOWN ( -- )
   SET-LIVE? @ 0= if exit then
   SET-N @ 0 ?do i 1+ UNDEF-ONE loop
   0 SET-LIVE? ! ;

: COMPILE-SET ( -- )
   SET-N @ 0 ?do i DEF$ evaluate-closed loop ;

: DISPATCHES ( -- n )  NC-COUNT @ NC-MARK @ - ;

: EXPECT ( n n -- ) {: want got :}
   want got = if exit then
   SB-RESET
   s" chain-scale: a measured set put " SB-APPEND  got FMT:SB-U
   s"  definitions through ncomp, expected " SB-APPEND  want FMT:SB-U
   LF SB-APPEND-C
   SB$ E-SCALE-UNCOMPILED REFUSE ;

\ ---- one family member ------------------------------------------------------
\ NPROF times phase `spill` around each A64SPILL:REWRITE call (LOWERED,
\ src/arch/arm64/passes.f) and the pass adds its module's operation count. The
\ divisor is the accumulator's own call count: a definition that needed two
\ lowering turns contributes two calls, and one the pass never ran on none.
: MEAN ( n n -- n ) {: total calls :}
   calls 0= if
      SB-RESET
      s" chain-scale: a family member never reached the pass it measures"
      SB-APPEND LF SB-APPEND-C
      SB$ E-SCALE-UNCOMPILED REFUSE
   then
   total calls / ;

\ The decisions the sealed plan carried, per call: one more operation the
\ lowering writes for each of them, which is why they count as work handled.
: PLAN-PER-CALL ( -- n )
   NPROF-PHASE:SPILL-PLAN NPROF:NS@
   NPROF-PHASE:SPILL-PLAN NPROF:N@ MEAN ;

: RECORD ( n NPROF:phase NPROF:phase -- ) {: k ph:NPROF:phase ops:NPROF:phase :}
   PT-N @ {: p:n :}
   p POINTS-MAX >= if E-SCALE-CAPACITY throw then
   k p cells PT-K + !
   ops NPROF:NS@  ops NPROF:N@ MEAN  p cells PT-OPS + !
   PLAN-PER-CALL  p cells PT-PLAN + !
   ph NPROF:NS@  ph NPROF:N@ MEAN  p cells PT-NS + !
   ph NPROF:N@  p cells PT-CALLS + !
   p 1+ PT-N ! ;

: MEASURE ( ptr u8 n n n n NPROF:phase NPROF:phase -- )
   {: px:ptr pu shape k defs ph:NPROF:phase ops:NPROF:phase :}
   defs SET-N !
   TEARDOWN
   px pu shape k BUILD-SET
   NC-COUNT @ NC-MARK !
   NPROF:OPEN
   COMPILE-SET
   1 SET-LIVE? !
   defs DISPATCHES EXPECT
   k ph ops RECORD ;

\ ---- what the family says ---------------------------------------------------
\ What a pass HANDLES is what it reads plus what its plan makes it write. On
\ spill-frame the plan grows as ops^1.27: one run's slope read 1192 against ops
\ and 1017 against what it handles, the allocator's spill density read as this
\ pass getting slower.
: HANDLED ( n -- n ) {: p:n :}
   p cells PT-OPS + @  p cells PT-PLAN + @ + ;

: PT-LINE ( ptr u8 n n -- ) {: fam:ptr fu p:n :}
   SB-RESET
   s" scale: " SB-APPEND  fam fu SB-APPEND
   s"  k " SB-APPEND            p cells PT-K + @ FMT:SB-U
   s"  ops " SB-APPEND          p cells PT-OPS + @ FMT:SB-U
   s"  plan " SB-APPEND         p cells PT-PLAN + @ FMT:SB-U
   s"  calls " SB-APPEND        p cells PT-CALLS + @ FMT:SB-U
   s"  ns-per-call " SB-APPEND  p cells PT-NS + @ FMT:SB-U
   s"  ns-per-handled " SB-APPEND
   p cells PT-NS + @  p HANDLED MEAN  FMT:SB-U
   SB$ type cr ;

\ ln(ns per call, last point over first) over ln(handled, last over first),
\ times 1000: 1000 is cost proportional to the work, above it a pass that does
\ more per operation as the module grows. The per-point ns-per-handled column is
\ the same fact without the fit.
: SLOPE-MILLI ( -- n )
   PT-N @ 2 < if E-SCALE-CAPACITY throw then
   PT-N @ 1- {: last:n :}
   last cells PT-NS + @ s>f  0 cells PT-NS + @ s>f f/ FMATH:FLN {: dy:r :}
   last HANDLED s>f  0 HANDLED s>f f/ FMATH:FLN {: dx:r :}
   dy dx f/ MILLI s>f f* FMATH:FROUND ;

: FAMILY ( ptr u8 n -- n ) {: fam:ptr fu :}
   PT-N @ 0 ?do fam fu i PT-LINE loop
   SLOPE-MILLI ;

: RUN-LINE ( -- )
   0 PT-N !
   s" P"   SH-LINE 1 5 NPROF-PHASE:SPILL NPROF-PHASE:SPILL-OPS MEASURE
   s" Q"   SH-LINE 2 5 NPROF-PHASE:SPILL NPROF-PHASE:SPILL-OPS MEASURE
   s" R"   SH-LINE 4 5 NPROF-PHASE:SPILL NPROF-PHASE:SPILL-OPS MEASURE
   s" S"   SH-LINE 8 5 NPROF-PHASE:SPILL NPROF-PHASE:SPILL-OPS MEASURE
   s" spill-line" FAMILY S-LINE ! ;

: RUN-FRAME ( -- )
   0 PT-N !
   s" U"   SH-FRAME 1 3 NPROF-PHASE:SPILL NPROF-PHASE:SPILL-OPS MEASURE
   s" V"   SH-FRAME 2 3 NPROF-PHASE:SPILL NPROF-PHASE:SPILL-OPS MEASURE
   s" W"   SH-FRAME 4 3 NPROF-PHASE:SPILL NPROF-PHASE:SPILL-OPS MEASURE
   s" spill-frame" FAMILY S-FRAME ! ;

: REPORT ( -- )
   SB-RESET
   s" scale: slope spill-line " SB-APPEND  S-LINE @ FMT:SB-U
   s"  spill-frame " SB-APPEND          S-FRAME @ FMT:SB-U
   s"  compiled " SB-APPEND             NC-COUNT @ FMT:SB-U
   SB$ type cr ;

\ ---- the ratchet ------------------------------------------------------------
\ The bound is compared with the printed milli-slopes, so a run passes exactly
\ when every printed slope is at most the bound.
: BOUND-MILLI-OF ( ptr u8 n -- n )   \ "1.1" -> 1100
   STR>FLOAT MATCH option
     none OF s" the bound must be a decimal slope" ARGV:FAIL ENDOF
     some OF MILLI s>f f* f>s ENDOF
   ;MATCH ;

: OVER-BOUND ( n ptr u8 n -- ) {: got fam:ptr fu :}
   SB-RESET
   s" chain-scale: " SB-APPEND  fam fu SB-APPEND
   s"  slope " SB-APPEND  got FMT:SB-U
   s"  is above the bound of " SB-APPEND  BOUND-MILLI @ FMT:SB-U
   LF SB-APPEND-C
   SB$ E-SCALE-EXCEEDED REFUSE ;

: RATCHET ( -- )
   ARGV:POS# 0= if exit then
   BOUND-MILLI @ {: bound:n :}
   S-LINE @ bound > if S-LINE @ s" spill-line" OVER-BOUND then
   S-FRAME @ bound > if S-FRAME @ s" spill-frame" OVER-BOUND then ;

\ The bound is read and converted BEFORE anything is measured, so a malformed
\ argument costs a usage line rather than the whole measurement.
: CONFIG ( -- )
   s" bin/hb --load tools/chain-scale.f -- [slope-bound]" ARGV:USAGE!
   ARGV:PARSE
   0 1 ARGV:EXPECT-POS
   ARGV:POS# 0= if exit then
   0 ARGV:POS$ BOUND-MILLI-OF BOUND-MILLI ! ;

\ ---- the borrow -------------------------------------------------------------
\ Every boot installs NCOMP:COMPILE in the tier-1 `:` dispatch, the entry
\ COUNTING-COMPILE forwards to, so PUT-BACK ends the borrow with NCOMP:INSTALL.
\ MAIN takes the dispatch, tier and measurement and gives them back through
\ `finally`, as tools/compile-floor.f does, so a loader carries on unchanged.
: TAKE-OVER ( -- )
   tier@ PRIOR-TIER !
   0 NC-COUNT !
   ['] COUNTING-COMPILE NCOMP:DISPATCH! ;

\ TEARDOWN first: on the refusing paths a measured set is still standing, and
\ the caller gets its dictionary back along with the compiler's dispatch and
\ its tier.
: PUT-BACK ( -- )
   TEARDOWN
   NCOMP:INSTALL
   PRIOR-TIER @ set-tier
   NPROF:CLOSE ;

: MEASURE-ALL ( -- )
   1 set-tier
   RUN-LINE
   RUN-FRAME
   REPORT
   RATCHET ;

public

: MAIN ( -- )
   CONFIG
   TAKE-OVER
   [: MEASURE-ALL ;] [: PUT-BACK ;] finally ;

;package

CHAIN-SCALE:MAIN
