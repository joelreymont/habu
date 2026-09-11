\ compile-scaling.f - how register allocation scales with the size of one
\ definition, as one slope.
\
\ Run:
\     bin/hb --load tools/compile-scaling.f              \ report only
\     bin/hb --load tools/compile-scaling.f -- 1.1       \ report, then ratchet
\
\ Two machine-readable families, every mean in microseconds per definition:
\
\     scaling: values <n> compile <us> alloc <us>        \ one line per size
\     scaling: slope <x.xx>
\     scaling-locals: values <n> compile <us> alloc <us>
\     scaling-locals: slope <x.xx>
\
\ `alloc` is the time A64RA:ALLOCATE itself spent, read from the accumulator the
\ allocator keeps (A64RA:ALLOC-NS), zeroed by this tool before each set. It is
\ not a subtraction of two wall clocks and not a sampling profiler: the pass
\ times itself, because one compile enters it several times - the spill fixpoint
\ re-allocates the rewritten module - and no caller can see those boundaries.
\
\ MEASURED ON 495dea80 plus this file's accumulator, and on that same tree with
\ the three cost removals above this commit: aarch64 Linux, 12 cores, 2026-09-12,
\ three consecutive runs of each engine at a 1-minute load average between 1.05
\ and 2.08 with no other `hb` over 50% CPU. The `after` engine also carried a
\ fourth step, a block-scan bisection that moved neither family and was dropped
\ for it, so that row is this stack's. The `alloc` column in microseconds per
\ definition, one representative run of the three - the three agreed within 0.3%
\ at every size:
\
\     shape   values     32     64    128    256    512   1024   slope
\     chain   before   1458   3187   6874  14722  33157  80408    1.15
\     chain   after     997   1886   3639   7163  14210  28295    0.97
\     locals  before   1459   2924   6019  12900  29128  72182    1.12
\     locals  after    1026   1890   3611   7059  13960  27766    0.95
\
\ The bound the campaign wants is a slope of 1.1, which is where the ratchet
\ argument goes. Loading the Tender source under the forced optimizing tier on
\ the same box moved with them, 153.3 s before to 131.6 s after (one run each).
\
\ THE TWO SHAPES, AND WHY THEY ARE THESE.
\
\ `chain` opens with HOLD-N `dup 1 +` steps, which leave HOLD-N values standing
\ on the data stack, runs a chain of `1 +` over the top of them, and consumes
\ them with HOLD-N `+`. The standing values are read only at the end, so their
\ live ranges span the whole body: a class with a full-length hull is what makes
\ the allocator's per-class interval questions cost anything, and a body of only
\ short-lived values would never ask them. The chain is what grows the count.
\
\ `locals` binds LOCAL-N locals from computed values and then reads them in
\ rotation for the length of the body. Same growth, different residency - the
\ locals arrive as values the elaborator named rather than as stack places - so
\ a slope that held for only one body shape would show up as two slopes here.
\
\ WHAT THESE SHAPES DO NOT REACH, AND WHY NOT. Neither one spills, and no Habu
\ source reached through NCOMP:COMPILE was found that does. Three ceilings stand
\ in the way. The elaborator's compile-time value stack is 64 deep (`VMAX`,
\ src/compiler/native/elaborate.f), so no body can stand more than 63 values up
\ at once. A body standing 61 up was allocated at A64RA:SPILLS 0, and its value
\ count grew 7 per extra standing value against 2.8 lower down, which is the
\ selector lowering the deep ones onto the data stack before the allocator sees
\ them. And a routine that arrives with 32 register arguments is refused
\ outright with E-A64RA-POOL, before any spill is considered. So what this tool
\ measures is what the allocator costs with an EMPTY spill plan, and since those
\ ceilings are the compiler's and not this fixture's, that is the term the
\ Tender load pays too.
\
\ WHY THE VALUE COUNT IS A MEASUREMENT AND NOT AN ASSUMPTION. Each set's size is
\ read back from A64RA:VALUES, the sealed allocation's own count, and printed.
\ The target sizes - SIZE-BASE doubled SIZES-N times - are reached by taking a
\ measured per-shape overhead off them, so a compiler change that moves that
\ overhead shows up as a moved `values` column rather than as a silently
\ mis-sized family. The tool refuses with E-SCALING-SHAPE when the family is not
\ strictly increasing or when its largest size falls under SPAN-MIN values,
\ because a slope over a family that did not grow is not a slope.
\
\ WHY THE COUNT IS PROVED AND NOT ASSUMED. `set-tier` is engine-global state and
\ a tier-1 selection that silently did not take would report the JIT's number as
\ the optimizing compiler's, and the JIT never enters A64RA at all - which would
\ report `alloc 0` as a success. A wrapper on NCOMP-DISPATCH:XT-CELL counts every
\ dispatch into NCOMP:COMPILE; each set must contribute exactly REPS or the tool
\ refuses with E-SCALING-UNCOMPILED instead of printing a number.
\
\ AND WHY THE BORROW IS SCOPED. The dispatch cell, the tier and the measurement
\ names are the engine's, not this tool's. All three are taken inside MAIN and
\ given back through `finally`, so they are returned on the refusing path
\ exactly as on the reporting one, and a process may load this file, read the
\ lines and carry on unchanged. tools/compile-floor.f states the same argument
\ at length; this tool follows it.
\
\ THE SLOPE. Least squares of ln(alloc) on ln(values) over the six sizes of one
\ family, computed from the nanosecond means rather than the printed
\ microseconds: a constant scale factor moves the intercept and not the slope,
\ so that is the same answer with the rounding taken out. What is compared
\ against the ratchet is the two-decimal number the tool PRINTS, so a run that
\ passes and a run that reports a slope at or below the bound are the same run.
\
\ MEASURE ON A QUIET MACHINE. This is wall-clock time inside one process and
\ nothing isolates it from the neighbours; quote a slope only with the load
\ average it was taken at. The bar is a 1-minute load average under 4 with no
\ other `hb` over 50% CPU.

require lib/errors.f
require lib/string.f
require lib/fmt.f
require lib/float.f
require lib/fmath.f
require lib/argv.f

-7710 constant E-SCALING-UNCOMPILED
-7711 constant E-SCALING-EXCEEDED
-7712 constant E-SCALING-CAPACITY
-7713 constant E-SCALING-SHAPE

package COMPILE-SCALING
private

6 constant SIZES-N                 \ 32 64 128 256 512 1024 values
32 constant SIZE-BASE
8 constant REPS                    \ definitions per measured set
512 constant SPAN-MIN              \ the largest size must reach at least this
$20000 constant SRC-CAP            \ source bytes for one set
1000 constant NS-PER-US
2 constant ERR-FD
$0A constant LF
74 constant IO-RC                  \ sysexits EX_IOERR, as tools/lint/text.f uses it

2 constant SHAPES-N
0 constant SH-CHAIN
1 constant SH-LOCAL

12 constant HOLD-N                 \ values the chain shape stands on the stack
8 constant LOCAL-N                 \ locals the locals shape binds
29 constant CHAIN-OVER             \ values each shape costs besides its body,
12 constant LOCAL-OVER             \ measured on 495dea80

create SRC SRC-CAP allot
create SRC-OFF REPS cells allot
create SRC-LEN REPS cells allot
variable SRC-FILL

create R-VALS SIZES-N cells allot
create R-COMP SIZES-N cells allot  \ mean nanoseconds of one compile
create R-ALLOC SIZES-N cells allot \ mean nanoseconds inside A64RA:ALLOCATE

variable NC-COUNT                  \ NCOMP:COMPILE dispatches since MAIN took the cell
variable NC-MARK                   \ NC-COUNT when the current set opened
variable SCI                       \ reduction index for the least-squares sums

variable PRIOR-XT                  \ the dispatch this tool borrowed, to be put back
variable PRIOR-TIER                \ the tier the caller had selected

8 constant PREFIX-CAP
create LIVE-PREFIX PREFIX-CAP allot
variable PREFIX-U                  \ length of the prefix in LIVE-PREFIX
variable SET-LIVE?                 \ that set is defined and not yet removed

variable BOUND?                    \ a ratchet was given
variable BOUND-R                   \ and this is it

\ ---- the dispatch counter ---------------------------------------------------

: COUNTING-COMPILE ( ptr u8 n -- )
   NC-COUNT @ 1+ NC-COUNT !
   NCOMP:COMPILE ;

: DISPATCH-CELL ( -- ptr a )
   data-base NCOMP-DISPATCH:XT-CELL + ;

\ ---- refusal ----------------------------------------------------------------

: EMIT-ERR ( ptr u8 n -- ) {: msg:ptr mu :}
   ERR-FD msg mu write mu <> if
      s" compile-scaling: stderr write failed" IO-RC die
   then ;

: REFUSE ( ptr u8 n n -- ) {: msg:ptr mu thrown :}
   msg mu EMIT-ERR
   thrown throw ;

\ ---- the two trust boundaries this tool needs -------------------------------

TRUSTED: EVAL$ ( ptr u8 n -- ) evaluate ;
TRUSTED: SELECT-TIER ( n -- ) set-tier ;

\ ---- building one set, before the clock starts ------------------------------

: PUT ( ptr u8 n -- ) {: a:ptr u:n :}
   SRC-FILL @ u + SRC-CAP > if E-SCALING-CAPACITY throw then
   a SRC SRC-FILL @ + u BYTE-COPY
   SRC-FILL @ u + SRC-FILL ! ;

: PUT-U ( n -- )
   SB-RESET FMT:SB-U SB$ PUT ;

: LOCAL-NAME ( n -- ) {: ix:n :}
   s" zv" PUT  ix PUT-U ;

: CHAIN-BODY ( n -- ) {: k:n :}
   HOLD-N 0 ?do s" dup 1 + " PUT loop
   k 0 ?do s" 1 + " PUT loop
   HOLD-N 0 ?do s" + " PUT loop ;

: LOCAL-BINDS ( -- )
   LOCAL-N 0 ?do
      s" dup " PUT  i 1+ PUT-U  s"  + {: " PUT  i LOCAL-NAME  s"  :} " PUT
   loop ;

: LOCAL-BODY ( n -- ) {: k:n :}
   LOCAL-BINDS
   k 0 ?do  i LOCAL-N mod LOCAL-NAME  s"  + " PUT  loop ;

\ Every question below answers SH-CHAIN or falls through to SH-LOCAL, so a
\ shape that is neither is refused once, here, rather than silently measured as
\ the locals one.
: SHAPE-CK ( n -- ) {: sh:n :}
   sh SH-CHAIN =  sh SH-LOCAL = or 0= if E-SCALING-SHAPE throw then ;

: SHAPE-BODY ( n n -- ) {: sh:n k:n :}
   sh SH-CHAIN = if k CHAIN-BODY exit then
   k LOCAL-BODY ;

: SHAPE-PREFIX ( n -- ptr u8 n ) {: sh:n :}
   sh SH-CHAIN = if s" ZCH" exit then
   s" ZLC" ;

: SHAPE-LABEL ( n -- ptr u8 n ) {: sh:n :}
   sh SH-CHAIN = if s" scaling: " exit then
   s" scaling-locals: " ;

\ What a refusal calls the shape; the label above is what a reader parses.
: SHAPE-NAME ( n -- ptr u8 n ) {: sh:n :}
   sh SH-CHAIN = if s" chain" exit then
   s" locals" ;

: TARGET ( n -- n )                SIZE-BASE swap lshift ;

\ The body length that reaches one target, given what the shape costs around it.
: SHAPE-K ( n n -- n ) {: sh:n target:n :}
   sh SH-CHAIN = if target CHAIN-OVER - 1 max exit then
   target LOCAL-OVER - LOCAL-N max ;

: DEF-BEGIN ( ptr u8 n n -- ) {: px:ptr pu:n ix:n :}
   SRC-FILL @ ix cells SRC-OFF + !
   s" : " PUT  px pu PUT  ix PUT-U  s"  ( n -- n ) " PUT ;

: DEF-END ( n -- ) {: ix:n :}
   s" ;" PUT
   SRC-FILL @  ix cells SRC-OFF + @ -  ix cells SRC-LEN + ! ;

: BUILD-SET ( n n -- ) {: sh:n k:n :}
   sh SHAPE-PREFIX {: px:ptr pu:n :}
   pu PREFIX-CAP > if E-SCALING-CAPACITY throw then
   px LIVE-PREFIX pu BYTE-COPY
   pu PREFIX-U !
   0 SRC-FILL !
   REPS 0 ?do
      px pu i DEF-BEGIN
      sh k SHAPE-BODY
      i DEF-END
   loop ;

: DEF$ ( n -- ptr u8 n ) {: ix:n :}
   SRC SRC-OFF ix cells + @ +  SRC-LEN ix cells + @ ;

\ ---- the measured window ----------------------------------------------------

: COMPILE-SET ( -- n )             \ nanoseconds for the whole built set
   mono-ns
   REPS 0 ?do i DEF$ EVAL$ loop
   mono-ns swap - ;

: MEAN ( n -- n )  REPS / ;

: NS>US ( n -- n ) NS-PER-US / ;

: DISPATCHES ( -- n )  NC-COUNT @ NC-MARK @ - ;

\ A measured set is scaffolding, not a result: REPS names the caller never asked
\ for, and a second MAIN in one process would collide with every one of them.
: UNDEF-ONE ( n -- ) {: ix:n :}
   SB-RESET
   s" undefine " SB-APPEND  LIVE-PREFIX PREFIX-U @ SB-APPEND  ix FMT:SB-U
   SB$ EVAL$ ;

: TEARDOWN ( -- )
   SET-LIVE? @ 0= if exit then
   REPS 0 ?do i UNDEF-ONE loop
   0 SET-LIVE? ! ;

: EXPECT ( n n n n -- ) {: want:n got:n sh:n target:n :}
   want got = if exit then
   SB-RESET
   s" compile-scaling: the " SB-APPEND  sh SHAPE-NAME SB-APPEND
   s"  set at " SB-APPEND  target FMT:SB-U
   s"  values put " SB-APPEND  got FMT:SB-U
   s"  definitions through ncomp, expected " SB-APPEND  want FMT:SB-U
   LF SB-APPEND-C
   SB$ E-SCALING-UNCOMPILED REFUSE ;

\ TEARDOWN leads, so at most one set is ever live and LIVE-PREFIX always names
\ it: the removal happens before BUILD-SET overwrites the prefix it needs.
: MEASURE-SIZE ( n n -- ) {: sh:n ix:n :}
   TEARDOWN
   sh  sh ix TARGET SHAPE-K  BUILD-SET
   NC-COUNT @ NC-MARK !
   A64RA:ALLOC-NS-RESET
   COMPILE-SET MEAN  ix cells R-COMP + !
   1 SET-LIVE? !
   REPS DISPATCHES sh ix TARGET EXPECT
   A64RA:ALLOC-NS MEAN  ix cells R-ALLOC + !
   A64RA:VALUES  ix cells R-VALS + ! ;

\ ---- what makes the family a family -----------------------------------------

: SHAPE-SAY ( n -- ) {: sh:n :}
   SB-RESET
   s" compile-scaling: the " SB-APPEND  sh SHAPE-NAME SB-APPEND
   s"  family " SB-APPEND ;

: SHAPE-THROW ( -- )
   LF SB-APPEND-C
   SB$ E-SCALING-SHAPE REFUSE ;

\ A slope over a family that did not grow, that never reached the sizes it
\ claims, or over a set the allocator spent no measurable time on, is not a
\ slope.
: FAMILY-CK ( n -- ) {: sh:n :}
   SIZES-N 1 ?do
      i cells R-VALS + @  i 1- cells R-VALS + @ {: hi:n lo:n :}
      hi lo > 0= if
         sh SHAPE-SAY
         s" did not grow: " SB-APPEND  lo FMT:SB-U
         s"  values and then " SB-APPEND  hi FMT:SB-U
         SHAPE-THROW
      then
   loop
   SIZES-N 1- cells R-VALS + @ {: top:n :}
   top SPAN-MIN < if
      sh SHAPE-SAY
      s" tops out at " SB-APPEND  top FMT:SB-U
      s"  values, under the " SB-APPEND  SPAN-MIN FMT:SB-U
      s"  a slope needs" SB-APPEND
      SHAPE-THROW
   then
   SIZES-N 0 ?do
      i cells R-ALLOC + @ 0 > 0= if
         sh SHAPE-SAY
         s" spent no measurable time at " SB-APPEND  i cells R-VALS + @ FMT:SB-U
         s"  values" SB-APPEND
         SHAPE-THROW
      then
   loop ;

\ ---- the slope --------------------------------------------------------------

: LN-X ( n -- r )                  cells R-VALS + @ s>f FMATH:FLN ;
: LN-Y ( n -- r )                  cells R-ALLOC + @ s>f FMATH:FLN ;

: SUM-X ( -- r )
   0.0  0 SCI !
   begin SCI @ SIZES-N < while
      SCI @ LN-X f+
      SCI @ 1+ SCI !
   repeat ;

: SUM-Y ( -- r )
   0.0  0 SCI !
   begin SCI @ SIZES-N < while
      SCI @ LN-Y f+
      SCI @ 1+ SCI !
   repeat ;

: SUM-XX ( -- r )
   0.0  0 SCI !
   begin SCI @ SIZES-N < while
      SCI @ LN-X dup f* f+
      SCI @ 1+ SCI !
   repeat ;

: SUM-XY ( -- r )
   0.0  0 SCI !
   begin SCI @ SIZES-N < while
      SCI @ LN-X  SCI @ LN-Y  f* f+
      SCI @ 1+ SCI !
   repeat ;

: ROUND2 ( r -- r )
   100.0 f* FMATH:FROUND s>f 100.0 f/ ;

\ The printed number and the compared number are one number.
: SLOPE ( -- r )
   SIZES-N s>f {: n:r :}
   SUM-X {: sx:r :}
   SUM-Y {: sy:r :}
   SUM-XX {: sxx:r :}
   SUM-XY {: sxy:r :}
   n sxy f*  sx sy f* f-
   n sxx f*  sx sx f* f-
   f/ ROUND2 ;

\ ---- the report -------------------------------------------------------------

: REPORT-SIZE ( n n -- ) {: sh:n ix:n :}
   SB-RESET
   sh SHAPE-LABEL SB-APPEND
   s" values " SB-APPEND     ix cells R-VALS + @ FMT:SB-U
   s"  compile " SB-APPEND   ix cells R-COMP + @ NS>US FMT:SB-U
   s"  alloc " SB-APPEND     ix cells R-ALLOC + @ NS>US FMT:SB-U
   SB$ type cr ;

: REPORT-SLOPE ( n r -- ) {: sh:n s:r :}
   SB-RESET
   sh SHAPE-LABEL SB-APPEND
   s" slope " SB-APPEND  s 2 FMT:SB-FIX
   SB$ type cr ;

\ ---- the ratchet ------------------------------------------------------------

: RATCHET ( n r -- ) {: sh:n s:r :}
   BOUND? @ 0= if exit then
   s BOUND-R @ f> 0= if exit then
   SB-RESET
   s" compile-scaling: the " SB-APPEND  sh SHAPE-NAME SB-APPEND
   s"  slope " SB-APPEND  s 2 FMT:SB-FIX
   s"  is above the bound of " SB-APPEND  BOUND-R @ 2 FMT:SB-FIX
   LF SB-APPEND-C
   SB$ E-SCALING-EXCEEDED REFUSE ;

: RUN-SHAPE ( n -- ) {: sh:n :}
   sh SHAPE-CK
   SIZES-N 0 ?do sh i MEASURE-SIZE loop
   sh FAMILY-CK
   SIZES-N 0 ?do sh i REPORT-SIZE loop
   SLOPE {: s:r :}
   sh s REPORT-SLOPE
   sh s RATCHET ;

: BOUND! ( ptr u8 n -- )           \ "1.1" -> the slope ceiling
   STR>FLOAT MATCH option
     none OF s" bound must be a decimal slope" ARGV:FAIL ENDOF
     some OF BOUND-R !  true BOUND? ! ENDOF
   ;MATCH ;

\ The bound is read and converted BEFORE anything is measured, so a malformed
\ argument costs a usage line rather than half a minute of compiling.
: CONFIG ( -- )
   s" bin/hb --load tools/compile-scaling.f -- [max-slope]" ARGV:USAGE!
   ARGV:PARSE
   0 1 ARGV:EXPECT-POS
   false BOUND? !
   ARGV:POS# 0= if exit then
   0 ARGV:POS$ BOUND! ;

\ ---- the borrow -------------------------------------------------------------

: TAKE-OVER ( -- )
   DISPATCH-CELL @ PRIOR-XT !
   tier@ PRIOR-TIER !
   0 NC-COUNT !
   0 SET-LIVE? !
   ['] COUNTING-COMPILE DISPATCH-CELL xt!
   1 SELECT-TIER ;

\ TEARDOWN first: on the refusing paths a measured set is still standing, and
\ the caller gets its dictionary back along with its dispatch and its tier.
: PUT-BACK ( -- )
   TEARDOWN
   PRIOR-XT @ DISPATCH-CELL xt!
   PRIOR-TIER @ SELECT-TIER ;

: MEASURE-ALL ( -- )
   SHAPES-N 0 ?do i RUN-SHAPE loop ;

public

\ CONFIG runs before the borrow so a usage failure restores nothing, having
\ taken nothing. Everything after it is inside the cleanup's reach.
: MAIN ( -- )
   CONFIG
   TAKE-OVER
   [: MEASURE-ALL ;] [: PUT-BACK ;] finally ;

;package

COMPILE-SCALING:MAIN
