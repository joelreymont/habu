\ compile-scaling.f - how register allocation (A64RA:ALLOCATE) scales with the
\ size of one definition, as one slope per body shape. Only quiet-machine runs
\ compare: 1-minute load under 4, no other `hb` over 50% CPU; quote the load.
\   bin/hb --load tools/compile-scaling.f [-- BOUND]   BOUND (1.1): refuse a larger slope
\   scaling: values <n> compile <us> alloc <us>, then scaling: slope <x.xx>;
\   scaling-locals: the same for the locals shape. Means in us per definition.

require src/lib/errors.f
require src/lib/string.f
require src/lib/fmt.f
require src/lib/float.f
require src/lib/fmath.f
require src/lib/argv.f
require src/lib/fd.f

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
$0A constant LF
74 constant IO-RC                  \ sysexits EX_IOERR, as tools/lint/text.f uses it

2 constant SHAPES-N
\ `chain` stands HOLD-N values (`dup 1 +`), runs a `1 +` chain over them and
\ consumes them with `+`: their live ranges span the body, and a full-length
\ class is what makes the allocator's per-class interval questions cost
\ anything. `locals` binds LOCAL-N locals and reads them in rotation: the same
\ growth with values the elaborator named instead of stack places, so a slope
\ that held for one body shape only shows as two slopes.
\ Neither shape spills, and no Habu source through NCOMP:COMPILE was found that
\ does: the elaborator's value stack is 64 deep (VMAX,
\ src/compiler/native/elaborate.f); a body standing 61 values ran at
\ A64RA:SPILLS 0, its value count growing 7 per standing value against 2.8 lower
\ down (the selector lowers deep ones onto the data stack); a wide entry run is
\ put away load by load (regalloc.f MB-ANCHOR). The tool measures the allocator
\ with an empty spill plan, the term the Tender load pays too.
0 constant SH-CHAIN
1 constant SH-LOCAL

12 constant HOLD-N                 \ values the chain shape stands on the stack
8 constant LOCAL-N                 \ locals the locals shape binds
29 constant CHAIN-OVER             \ values each shape costs besides its body
12 constant LOCAL-OVER

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

variable PRIOR-TIER                \ the tier the caller had selected

8 constant PREFIX-CAP
create LIVE-PREFIX PREFIX-CAP allot
variable PREFIX-U
variable SET-LIVE?                 \ that set is defined and not yet removed

variable BOUND?                    \ a ratchet was given
variable BOUND-R

\ ---- the dispatch counter ---------------------------------------------------
\ `set-tier` is engine-global: a tier-1 selection that did not take would report
\ tier 0's number with `alloc 0`, as tier 0 never enters A64RA. Each set must add
\ exactly REPS dispatches or the tool refuses with E-SCALING-UNCOMPILED.

: COUNTING-COMPILE ( ptr u8 n -- )
   NC-COUNT @ 1+ NC-COUNT !
   NCOMP:COMPILE ;

\ ---- refusal ----------------------------------------------------------------

: EMIT-ERR ( ptr u8 n -- ) {: msg:ptr mu :}
   FD:STDERR FD>N msg mu write mu <> if
      s" compile-scaling: stderr write failed" IO-RC die
   then ;

: REFUSE ( ptr u8 n n -- ) {: msg:ptr mu thrown :}
   msg mu EMIT-ERR
   thrown throw ;

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
   REPS 0 ?do i DEF$ evaluate-closed loop
   mono-ns swap - ;

: MEAN ( n -- n )  REPS / ;

: NS>US ( n -- n ) NS-PER-US / ;

: DISPATCHES ( -- n )  NC-COUNT @ NC-MARK @ - ;

\ A measured set is scaffolding, not a result: REPS names the caller never asked
\ for, and a second MAIN in one process would collide with every one of them.
: UNDEF-ONE ( n -- ) {: ix:n :}
   SB-RESET
   s" undefine " SB-APPEND  LIVE-PREFIX PREFIX-U @ SB-APPEND  ix FMT:SB-U
   SB$ evaluate-closed ;

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

\ Each set's size is read back from A64RA:VALUES; targets (SIZE-BASE doubled
\ SIZES-N times) are reached by taking a measured per-shape overhead off, so a
\ compiler change that moves the overhead shows in the `values` column. A family
\ that does not strictly grow, tops out under SPAN-MIN, or holds a set with no
\ measurable allocator time is refused (E-SCALING-SHAPE): it has no slope.
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
\ Least squares of ln(alloc) on ln(values) over one family's six sizes, from the
\ nanosecond means: a scale factor moves the intercept, not the slope. The
\ ratchet compares the printed two-decimal number.

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
\ The bound is a slope of 1.1: it refuses the allocator before its three cost
\ removals and admits it after. alloc us per definition at 32, 64, 128, 256, 512
\ and 1024 values, then the slope (aarch64 Linux, 12 cores, load 1.05-2.08,
\ three runs within 0.3%):
\     chain   before   1458   3187   6874  14722  33157  80408    1.15
\     chain   after     997   1886   3639   7163  14210  28295    0.97
\     locals  before   1459   2924   6019  12900  29128  72182    1.12
\     locals  after    1026   1890   3611   7059  13960  27766    0.95

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
\ A64RA:ALLOC-NS is the allocator's own accumulator: one compile enters
\ A64RA:ALLOCATE several times (the spill fixpoint re-allocates) and no caller
\ sees those boundaries. Only this tool arms it: the cell is DATA the AOT capture
\ bakes into the engine, so a stopwatch running in every build would make two
\ same-host builds differ there. A64RA accumulates only between ALLOC-NS-OPEN
\ and ALLOC-NS-CLOSE, and CLOSE zeroes the cell.
\ MAIN borrows the dispatch, the tier and the stopwatch and gives them back
\ through `finally`, on the refusing path too: a process may load this file and
\ carry on unchanged.

\ Every boot installs NCOMP:COMPILE in the tier-1 `:` dispatch, the entry
\ COUNTING-COMPILE forwards to, so PUT-BACK ends the borrow with NCOMP:INSTALL.
: TAKE-OVER ( -- )
   tier@ PRIOR-TIER !
   0 NC-COUNT !
   0 SET-LIVE? !
   ['] COUNTING-COMPILE NCOMP:DISPATCH!
   1 set-tier
   A64RA:ALLOC-NS-OPEN ;

\ TEARDOWN first: on the refusing paths a measured set is still standing, and
\ the caller gets its dictionary back along with the compiler's dispatch, its
\ tier and a disarmed stopwatch.
: PUT-BACK ( -- )
   TEARDOWN
   NCOMP:INSTALL
   PRIOR-TIER @ set-tier
   A64RA:ALLOC-NS-CLOSE ;

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
