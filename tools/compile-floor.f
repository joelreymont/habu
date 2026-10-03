\ compile-floor.f - the tier-1 per-definition compile floor, as one number.
\
\ Run:
\     bin/hb --load tools/compile-floor.f                \ report only
\     bin/hb --load tools/compile-floor.f -- 0.5         \ report, then ratchet
\
\ One machine-readable line, every number in microseconds per definition (one
\ line in the output, wrapped here):
\
\     floor: trivial-t1 <us> three-op-t1 <us> trivial-t0 <us> compiled <n>
\            least-trivial-t1 <us> least-three-op-t1 <us> least-trivial-t0 <us>
\
\ The first three are each set's mean over every round, and the least- fields
\ each set's cheapest single definition in any round; compiled counts the
\ definitions the tier-1 compiler took. The ratchet below judges the
\ trivial-t1 mean, and the target is 500 us (0.5 ms) of it: a program pays for
\ every definition it compiles, so the mean is the cost a quote is about, and
\ it carries a cost only some definitions pay, which no least sees. It also
\ carries the cores the definitions landed on, so ratchet on a quiet machine;
\ test/compile-floor-gate.f, which runs at any load, judges each least against
\ a budget and each mean only against a loose ceiling.
\
\ WHAT IS TIMED. 100 definitions `: Tn ( n -- n ) 1 + ;` and 100 definitions
\ `: Un ( n n -- n ) swap drop ;`, each handed to the engine's
\ `evaluate-closed` inside its own window of this thread's CPU time
\ (TIME:THREAD-CPU-NS, lib/time-cpu.f). Every source string is built BEFORE
\ the first window opens, so what a window holds is compile time and not string
\ building. The tier-0 line compiles the trivial body a third time under its
\ own names (`Vn`), so the contrast never redefines the set it contrasts with.
\ A round compiles the three sets in that order, each torn down before the next
\ is built, and a run is five rounds.
\
\ WHY `swap drop` IS THE SECOND BODY. It carries no combinable pair and `1 +`
\ carries one, so the two lines together price the fold. They used to price a
\ whole module rebuild: the pair was found after selection and the module was
\ written again to hold the combined form, which cost about 160 us of the
\ trivial line. Selection writes the combined form itself now, so what is left
\ between the two lines is the work the pair's own operations cost.
\
\ WHY THE COUNT IS PROVED AND NOT ASSUMED. `set-tier` is engine-global state,
\ and a tier-1 selection that silently did not take would report the JIT's
\ number as the optimizing compiler's - which is the one mistake that makes
\ this whole file lie. A wrapper on NCOMP-DISPATCH:XT-CELL counts every
\ dispatch into NCOMP:COMPILE. Each tier-1 set must contribute exactly 100 and
\ the tier-0 set exactly 0, or the tool refuses with E-FLOOR-UNCOMPILED
\ instead of printing a number.
\
\ AND WHY THE BORROW IS SCOPED. The dispatch cell, the tier and 300 dictionary
\ entries are the engine's, not this tool's. All three are taken inside MAIN,
\ after the argument is parsed, and all three are given back through
\ `finally` - so they are returned on the refusing path exactly as on the
\ reporting one. Installing the wrapper at file load instead would count every
\ definition the loading process made afterwards on tier 1; leaving tier 0
\ selected would hand the caller a different compiler than it had; and leaving
\ the measurement names behind would make the second MAIN in a process die of
\ `duplicate definition: T1`. A process may load this file, read the line, and
\ carry on unchanged; the in-process controls in
\ test/compiler/compile-floor.f are what hold that open.
\
\ WHY CPU TIME, WHY THE LEAST, AND WHY ROUNDS. A wall-clock window also holds
\ every slice the scheduler gave the neighbours: the same tool on one commit
\ reported trivial-t1 anywhere from 9193 to 29407 us while sibling lanes
\ compiled. The thread's CPU time leaves those slices out, but not the core
\ the thread ran on: an efficiency core compiles the same set about 2.5-3x
\ slower than a performance core, and under load part of a set lands on one.
\ That only ever adds time, and a regression slows every definition, so a
\ set's cheapest definition is its cost on the fastest core the run found;
\ that is the number test/compile-floor-gate.f judges. One pass of the tier-0
\ set is about 4 ms of CPU and fits inside one slow stretch: at load average
\ 90 on this 12-core machine, one run in 96 read least-trivial-t0 75 us
\ against a usual 34. Five rounds spread every set across the whole run, about
\ half a second of CPU. The least prices only a cost every definition pays:
\ one that only some definitions pay shows in the mean alone. Quote a number
\ with the host and the load it was taken at.
\
\ THE RATCHET. Given a floor in milliseconds the tool refuses with
\ E-FLOOR-EXCEEDED - and a nonzero exit - when the trivial tier-1 mean is
\ above it. The comparison is against the microseconds this tool PRINTS, so a
\ run that passes the ratchet and a run that reports a number below the floor
\ are the same run. Without the argument it only reports.

require lib/errors.f
require lib/string.f
require lib/fmt.f
require lib/float.f
require lib/argv.f
require lib/time-cpu.f

-7700 constant E-FLOOR-UNCOMPILED
-7701 constant E-FLOOR-EXCEEDED
-7702 constant E-FLOOR-CAPACITY

package COMPILE-FLOOR
private

100 constant SET-N                 \ definitions per measured set
5 constant ROUNDS                  \ every set is compiled once per round
$2000 constant SRC-CAP             \ source bytes for one set
1000 constant NS-PER-US
1000 constant US-PER-MS
2 constant ERR-FD
$0A constant LF
74 constant IO-RC                  \ sysexits EX_IOERR, as tools/lint/text.f uses it

create SRC SRC-CAP allot
create SRC-OFF SET-N cells allot
create SRC-LEN SET-N cells allot
variable SRC-FILL

variable NC-COUNT                  \ NCOMP:COMPILE dispatches since MAIN took the cell
variable NC-MARK                   \ NC-COUNT when the current set opened

variable PRIOR-TIER                \ the tier the caller had selected

8 constant PREFIX-CAP
create LIVE-PREFIX PREFIX-CAP allot
variable PREFIX-U                  \ length of the prefix in LIVE-PREFIX
variable SET-LIVE?                 \ that set is defined and not yet removed

\ The three sets, as rows of the two tables below.
0 constant TRIV-T1
1 constant THREE-T1
2 constant TRIV-T0
3 constant SETS
$7FFFFFFFFFFFFFFF constant NO-LEAST \ above any least: the first sample replaces it
create SET-NS SETS cells allot     \ each set's CPU ns, summed over every round
create SET-LEAST SETS cells allot  \ each set's cheapest definition in any round
variable BOUND-US                  \ ratchet floor in us; read only when a floor was given

\ ---- the dispatch counter ---------------------------------------------------
\ The wrapper stands in front of the compiler's own entry, so the count
\ covers every definition compiled on tier 1 while it is installed and nothing
\ else: tier 0 reaches LCOMPILE without reading this cell at all.

: COUNTING-COMPILE ( ptr u8 n -- )
   NC-COUNT @ 1+ NC-COUNT !
   NCOMP:COMPILE ;

\ ---- refusal ----------------------------------------------------------------

: EMIT-ERR ( ptr u8 n -- ) {: msg:ptr mu :}
   ERR-FD msg mu write mu <> if
      s" compile-floor: stderr write failed" IO-RC die
   then ;

: REFUSE ( ptr u8 n n -- ) {: msg:ptr mu thrown :}
   msg mu EMIT-ERR
   thrown throw ;

\ ---- selecting the tier -----------------------------------------------------
\ `set-tier` is refused inside a plain checked body. It is wrapped in one call,
\ with nothing else in the body, so the unchecked surface is exactly the
\ primitive and not the measurement around it. The built set and its teardown
\ load through `evaluate-closed`: definitions and undefines leave no cell.

TRUSTED: SELECT-TIER ( n -- ) set-tier ;

\ ---- building one set, before the clock starts ------------------------------

: SRC+ ( ptr u8 n n -- ) {: a:ptr u ix :}
   SRC-FILL @ u + SRC-CAP > if E-FLOOR-CAPACITY throw then
   SRC-FILL @ SRC-OFF ix cells + !
   u SRC-LEN ix cells + !
   a SRC SRC-FILL @ + u BYTE-COPY
   SRC-FILL @ u + SRC-FILL ! ;

: BUILD-SET ( ptr u8 n ptr u8 n -- ) {: px:ptr pu tx:ptr tu :}
   pu PREFIX-CAP > if E-FLOOR-CAPACITY throw then
   px LIVE-PREFIX pu BYTE-COPY
   pu PREFIX-U !
   0 SRC-FILL !
   SET-N 0 ?do
      SB-RESET
      s" : " SB-APPEND  px pu SB-APPEND  i 1+ FMT:SB-U  tx tu SB-APPEND
      SB$ i SRC+
   loop ;

: DEF$ ( n -- ptr u8 n ) {: ix :}
   SRC SRC-OFF ix cells + @ +  SRC-LEN ix cells + @ ;

\ ---- the measured window ----------------------------------------------------

: COMPILE-ONE ( n -- n )           \ CPU nanoseconds for one definition
   TIME:THREAD-CPU-NS swap
   DEF$ evaluate-closed
   TIME:THREAD-CPU-NS swap - ;

: NS-CELL ( n -- ptr n ) cells SET-NS + ;

: LEAST-CELL ( n -- ptr n ) cells SET-LEAST + ;

\ Every definition's CPU time added to the set's sum, the cheapest kept.
: COMPILE-SET ( n -- ) {: set :}
   SET-N 0 ?do
      i COMPILE-ONE
      dup set NS-CELL +!
      set LEAST-CELL @ min set LEAST-CELL !
   loop ;

: MEAN-US ( n -- n )  NS-CELL @ NS-PER-US / SET-N ROUNDS * / ;

: LEAST-US ( n -- n )  LEAST-CELL @ NS-PER-US / ;

: RESET-SETS ( -- )
   SETS 0 ?do
      0 i NS-CELL !
      NO-LEAST i LEAST-CELL !
   loop ;

: DISPATCHES ( -- n )  NC-COUNT @ NC-MARK @ - ;

\ A measured set is scaffolding, not a result: 100 names the caller never asked
\ for, and a second MAIN in one process would collide with every one of them
\ (the engine refuses a redefinition outright). Removing them is what makes
\ this tool callable twice, and so what lets the in-process controls exist.
: UNDEF-ONE ( n -- ) {: ix :}
   SB-RESET
   s" undefine " SB-APPEND  LIVE-PREFIX PREFIX-U @ SB-APPEND  ix FMT:SB-U
   SB$ evaluate-closed ;

: TEARDOWN ( -- )
   SET-LIVE? @ 0= if exit then
   SET-N 0 ?do i 1+ UNDEF-ONE loop
   0 SET-LIVE? ! ;

\ TEARDOWN leads, so at most one set is ever live and LIVE-PREFIX always names
\ it: the removal happens before BUILD-SET overwrites the prefix it needs.
\ That is the invariant rather than an ordering the callers below must keep.
\
\ COMPILE-SET either defines every name in the set or throws, so once it has
\ returned the whole set exists and TEARDOWN's count is exact. A throw inside
\ it leaves SET-LIVE? clear and the partial set in the dictionary - reachable
\ only if compiling `1 +` fails, which kills the run loudly anyway.
\
\ MEASURE is one round of one set: its name prefix, its body tail and its row.
: MEASURE ( ptr u8 n ptr u8 n n -- ) {: px:ptr pu tx:ptr tu set :}
   TEARDOWN
   px pu tx tu BUILD-SET
   NC-COUNT @ NC-MARK !
   set COMPILE-SET
   1 SET-LIVE? ! ;

: EXPECT ( n n ptr u8 n -- ) {: want got lbl:ptr lu :}
   want got = if exit then
   SB-RESET
   s" compile-floor: " SB-APPEND  lbl lu SB-APPEND
   s"  put " SB-APPEND  got FMT:SB-U
   s"  definitions through ncomp, expected " SB-APPEND  want FMT:SB-U
   LF SB-APPEND-C
   SB$ E-FLOOR-UNCOMPILED REFUSE ;

: RUN-TIER1 ( -- )
   1 SELECT-TIER
   s" T" s"  ( n -- n ) 1 + ; " TRIV-T1 MEASURE
   SET-N DISPATCHES s" the trivial tier-1 set" EXPECT
   s" U" s"  ( n n -- n ) swap drop ; " THREE-T1 MEASURE
   SET-N DISPATCHES s" the three-op tier-1 set" EXPECT ;

: RUN-TIER0 ( -- )
   0 SELECT-TIER
   s" V" s"  ( n -- n ) 1 + ; " TRIV-T0 MEASURE
   0 DISPATCHES s" the trivial tier-0 set" EXPECT ;

: REPORT ( -- )
   SB-RESET
   s" floor: trivial-t1 " SB-APPEND  TRIV-T1 MEAN-US FMT:SB-U
   s"  three-op-t1 " SB-APPEND       THREE-T1 MEAN-US FMT:SB-U
   s"  trivial-t0 " SB-APPEND        TRIV-T0 MEAN-US FMT:SB-U
   s"  compiled " SB-APPEND          NC-COUNT @ FMT:SB-U
   s"  least-trivial-t1 " SB-APPEND  TRIV-T1 LEAST-US FMT:SB-U
   s"  least-three-op-t1 " SB-APPEND THREE-T1 LEAST-US FMT:SB-U
   s"  least-trivial-t0 " SB-APPEND  TRIV-T0 LEAST-US FMT:SB-U
   SB$ type cr ;

\ ---- the ratchet ------------------------------------------------------------

: FLOOR-US ( ptr u8 n -- n )       \ "0.5" -> 500
   STR>FLOAT MATCH option
     none OF s" floor must be a decimal number of milliseconds" ARGV:FAIL ENDOF
     some OF US-PER-MS s>f f* f>s ENDOF
   ;MATCH ;

: RATCHET ( -- )
   ARGV:POS# 0= if exit then
   TRIV-T1 MEAN-US BOUND-US @ <= if exit then
   SB-RESET
   s" compile-floor: trivial-t1 " SB-APPEND  TRIV-T1 MEAN-US FMT:SB-U
   s"  us is above the floor of " SB-APPEND  BOUND-US @ FMT:SB-U
   s"  us" SB-APPEND  LF SB-APPEND-C
   SB$ E-FLOOR-EXCEEDED REFUSE ;

\ The floor is read and converted BEFORE anything is measured, so a malformed
\ argument costs a usage line rather than two seconds of compiling.
: CONFIG ( -- )
   s" bin/hb --load tools/compile-floor.f -- [floor-ms]" ARGV:USAGE!
   ARGV:PARSE
   0 1 ARGV:EXPECT-POS
   ARGV:POS# 0= if exit then
   0 ARGV:POS$ FLOOR-US BOUND-US ! ;

\ ---- the borrow ------------------------------------------------------------

\ Every boot installs NCOMP:COMPILE in the tier-1 `:` dispatch, the entry
\ COUNTING-COMPILE forwards to, so PUT-BACK ends the borrow with NCOMP:INSTALL.
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
   PRIOR-TIER @ SELECT-TIER ;

: MEASURE-ALL ( -- )
   RESET-SETS
   ROUNDS 0 ?do RUN-TIER1 RUN-TIER0 loop
   REPORT
   RATCHET ;

public

\ CONFIG runs before the borrow so a usage failure restores nothing, having
\ taken nothing. Everything after it is inside the cleanup's reach.
: MAIN ( -- )
   CONFIG
   TAKE-OVER
   [: MEASURE-ALL ;] [: PUT-BACK ;] finally ;

;package

COMPILE-FLOOR:MAIN
