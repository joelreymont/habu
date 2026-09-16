\ chain-scale.f - how the native chain's module-rebuilding pass grows with the
\ module it rewrites, as one slope per shape.
\
\ Run:
\     bin/hb --load tools/chain-scale.f              \ report only
\     bin/hb --load tools/chain-scale.f -- 1.1       \ report, then hold the bound
\
\ Machine-readable lines, one per family member and then one verdict:
\
\     scale: <family> k <k> ops <n> plan <n> calls <n> ns-per-call <n> ns-per-handled <n>
\     scale: slope spill-line <milli> spill-frame <milli> compiled <n>
\
\ A slope is ln(ns per call at the last point / at the first) over ln(operations
\ HANDLED at the last / at the first), times 1000: 1000 is cost proportional to
\ the work, and anything above it is a pass that does more per operation the
\ larger the module gets. Handled is the module's own operations plus the sealed
\ plan's decisions, because each decision is one more operation the lowering
\ writes - on the spill-frame family the plan grows as ops^1.27, so dividing by
\ the module size alone reads the ALLOCATOR's spill density as this pass getting
\ slower (1192 against ops, 1017 against what it handles, on one run). The
\ per-point ns-per-handled column is the same fact without the fit, so a reader
\ can see whether it is flat.
\
\ MEASURED on aarch64 Linux, quiet box (1-minute load average under 4, no other
\ hb over 50% CPU), 2026-09-12, on this lane's UNCOMMITTED tree - the state the
\ commits above this one reconstruct - against its parent 495dea80:
\
\     spill-line 1002, spill-frame 1033   (the lane's tree)
\     spill-line 1046, spill-frame 1160  (495dea80)

\ Those runs also carried a third family, `combine`, over a body of k foldable
\ movz/add pairs. Selection folds those pairs as it writes them, so no pass
\ rebuilds the module for them any more and the family measures nothing.
\
\ Only 495dea80 is a commit a reader can check out and re-run; the pair has NOT
\ been re-taken against the committed stack, because the box has not been quiet
\ since. Re-run both sides before quoting these as this lane's result.
\
\ Results are only comparable between runs taken on an idle machine; a slope is
\ a ratio and survives moderate load far better than the microseconds in
\ tools/compile-floor.f, but a box compiling in four other workspaces moves it
\ by a few percent.
\
\ THE TWO FAMILIES. Each is one body shape whose operation count grows with k
\ while its SHAPE - how many functions, how many blocks per function, how many
\ values live at once - stays put, because a slope only means something when the
\ thing being scaled is size.
\
\   spill-line  28 values live across k additions each, one block: the register
\               file overflows, so A64SPILL:REWRITE runs, and it runs on a
\               straight line of operations.
\   spill-frame the same 28 live values across k conditional regions: 2k+1
\               blocks in one function, which is where the pass's per-block and
\               per-edge work lives. The allocator dominates this family's
\               wall time (about 90% of it at k=4) and is not what is timed.
\
\ WHAT IS TIMED. NPROF (src/compiler/native/prof.f) is the chain's own
\ stopwatch: NCOMP:LOWERED opens `spill` around one A64SPILL:REWRITE call, and
\ the pass adds the operation count of the module it was handed. This tool resets it,
\ compiles a set, and divides by the call count the accumulator itself kept, so
\ a definition that needed two lowering turns counts as two calls and not one.
\ Every source string is built BEFORE the set is compiled.
\
\ WHY THE COUNT IS PROVED AND NOT ASSUMED. `set-tier` is engine-global state,
\ and a tier-1 selection that silently did not take would report the JIT's
\ number as the optimizing compiler's. A wrapper on NCOMP-DISPATCH:XT-CELL
\ counts every dispatch into NCOMP:COMPILE; each set must contribute exactly its
\ own definition count or the tool refuses with E-SCALE-UNCOMPILED instead of
\ printing a number. The dispatch cell, the tier and the measurement names are
\ all taken inside MAIN and given back through `finally`, exactly as
\ tools/compile-floor.f takes them, so a process may load this file, read the
\ lines, and carry on unchanged.
\
\ THE RATCHET. Given a bound the tool refuses with E-SCALE-EXCEEDED - and a
\ nonzero exit - when any of the three slopes is above it. The comparison is
\ against the milli-slopes this tool PRINTS, so a run that passes the ratchet
\ and a run that reports slopes below the bound are the same run.

require lib/errors.f
require lib/string.f
require lib/fmt.f
require lib/float.f
require lib/fmath.f
require lib/argv.f

-7703 constant E-SCALE-UNCOMPILED
-7704 constant E-SCALE-EXCEEDED
-7705 constant E-SCALE-CAPACITY

package CHAIN-SCALE
private

20 constant DEFS-MAX               \ definitions in the largest measured set
8 constant POINTS-MAX              \ family members
$8000 constant SRC-CAP             \ source bytes for one set
1000 constant MILLI
2 constant ERR-FD
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
variable PRIOR-XT
variable PRIOR-TIER

8 constant PREFIX-CAP
create LIVE-PREFIX PREFIX-CAP allot
variable PREFIX-U
variable SET-LIVE?

variable S-LINE                    \ the two slopes, in thousandths
variable S-FRAME
variable BOUND-MILLI               \ ratchet bound; read only when one was given

\ ---- the dispatch counter ----------------------------------------------------
: COUNTING-COMPILE ( ptr u8 n -- )
   NC-COUNT @ 1+ NC-COUNT !
   NCOMP:COMPILE ;

: DISPATCH-CELL ( -- ptr a )
   data-base NCOMP-DISPATCH:XT-CELL + ;

\ ---- refusal ----------------------------------------------------------------
: EMIT-ERR ( ptr u8 n -- ) {: msg:ptr mu :}
   ERR-FD msg mu write mu <> if
      s" chain-scale: stderr write failed" IO-RC die
   then ;

: REFUSE ( ptr u8 n n -- ) {: msg:ptr mu thrown :}
   msg mu EMIT-ERR
   thrown throw ;

\ ---- the two trust boundaries this tool needs -------------------------------
TRUSTED: EVAL$ ( ptr u8 n -- ) evaluate ;
TRUSTED: SELECT-TIER ( n -- ) set-tier ;

\ ---- building one set, before the clock starts ------------------------------
: SRC+ ( ptr u8 n -- ) {: a:ptr u :}
   SRC-FILL @ u + SRC-CAP > if E-SCALE-CAPACITY throw then
   a SRC SRC-FILL @ + u BYTE-COPY
   SRC-FILL @ u + SRC-FILL ! ;

: CHUNK+ ( -- ) SB$ SRC+ ;

: DEF$ ( n -- ptr u8 n ) {: ix :}
   SRC SRC-OFF ix cells + @ +  SRC-LEN ix cells + @ ;

\ LIVE-N values live at once, which is what overflows the register file; k
\ additions inside each one, so the operation count grows and the live set does not.
: LINE-BODY ( n -- ) {: k :}
   SB-RESET s"  ( n -- n ) " SB-APPEND CHUNK+
   LIVE-N 0 ?do
      SB-RESET s" dup " SB-APPEND CHUNK+
      k 0 ?do SB-RESET i 1+ FMT:SB-U s"  + " SB-APPEND CHUNK+ loop
      SB-RESET s" swap " SB-APPEND CHUNK+
   loop
   LIVE-N 0 ?do SB-RESET s" + " SB-APPEND CHUNK+ loop ;

\ The same live set carried across k conditional regions: 2k+1 blocks in one
\ function, so the pass's per-block and per-edge work is what grows.
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
   SB$ EVAL$ ;

: TEARDOWN ( -- )
   SET-LIVE? @ 0= if exit then
   SET-N @ 0 ?do i 1+ UNDEF-ONE loop
   0 SET-LIVE? ! ;

: COMPILE-SET ( -- )
   SET-N @ 0 ?do i DEF$ EVAL$ loop ;

: DISPATCHES ( -- n )  NC-COUNT @ NC-MARK @ - ;

: EXPECT ( n n -- ) {: want got :}
   want got = if exit then
   SB-RESET
   s" chain-scale: a measured set put " SB-APPEND  got FMT:SB-U
   s"  definitions through ncomp, expected " SB-APPEND  want FMT:SB-U
   LF SB-APPEND-C
   SB$ E-SCALE-UNCOMPILED REFUSE ;

\ ---- one family member ------------------------------------------------------
\ The accumulator's own call count is the divisor: a definition that needed two
\ lowering turns contributes two calls, and one the pass never ran on
\ contributes none.
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
\ What a pass HANDLES is what it reads plus what its plan makes it write: a
\ lowering whose plan doubled has twice the work at the same module size, and
\ dividing by the module size alone would read the allocator's spill density as
\ this pass getting slower.
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

\ Two points and the widest span the family has: a power law read off its ends
\ is what the per-point ns-per-op column above is printed to corroborate.
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
: TAKE-OVER ( -- )
   DISPATCH-CELL @ PRIOR-XT !
   tier@ PRIOR-TIER !
   0 NC-COUNT !
   ['] COUNTING-COMPILE DISPATCH-CELL xt! ;

\ TEARDOWN first: on the refusing paths a measured set is still standing, and
\ the caller gets its dictionary back along with its dispatch and its tier.
: PUT-BACK ( -- )
   TEARDOWN
   PRIOR-XT @ DISPATCH-CELL xt!
   PRIOR-TIER @ SELECT-TIER
   NPROF:CLOSE ;

: MEASURE-ALL ( -- )
   1 SELECT-TIER
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
