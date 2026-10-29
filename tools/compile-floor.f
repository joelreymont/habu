\ compile-floor.f - the tier-1 per-definition compile floor, as one number.
\   bin/hb --load tools/compile-floor.f [-- FLOOR-MS]   with FLOOR-MS (0.5): refuse
\   a larger trivial-t1 mean, E-FLOOR-EXCEEDED. One line, us per definition:
\   floor: trivial-t1 <us> three-op-t1 <us> [trivial-t0 <us>] compiled <n>
\          least-trivial-t1 <us> least-three-op-t1 <us> [least-trivial-t0 <us>]
\ The t0 fields are skipped on x86-64; compiled counts tier-1 definitions.

require src/lib/errors.f
require src/lib/string.f
require src/lib/fmt.f
require src/lib/float.f
require src/lib/argv.f
require src/lib/time-cpu.f
require src/lib/fd.f

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
\ `set-tier` is engine-global: a tier-1 selection that did not take would report
\ tier 0's number as the optimizing compiler's. The wrapper stands in front of
\ the compiler's own entry, so it counts every definition compiled on tier 1
\ while installed and nothing else: tier 0 never reads this cell. Each tier-1
\ set must add exactly SET-N and the tier-0 set 0, or E-FLOOR-UNCOMPILED.

: COUNTING-COMPILE ( ptr u8 n -- )
   NC-COUNT @ 1+ NC-COUNT !
   NCOMP:COMPILE ;

\ ---- refusal ----------------------------------------------------------------

: EMIT-ERR ( ptr u8 n -- ) {: msg:ptr mu :}
   FD:STDERR FD>N msg mu write mu <> if
      s" compile-floor: stderr write failed" IO-RC die
   then ;

: REFUSE ( ptr u8 n n -- ) {: msg:ptr mu thrown :}
   msg mu EMIT-ERR
   thrown throw ;

\ ---- building one set, before the clock starts ------------------------------
\ The built set and its teardown load through `evaluate-closed`: definitions
\ and undefines leave no cell.

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
\ Each definition is one `evaluate-closed` inside its own window of this
\ thread's CPU time. Wall clock also holds the neighbours' slices: one commit
\ read trivial-t1 from 9193 to 29407 us while sibling lanes compiled. CPU time
\ still holds the core: an efficiency core compiles a set about 2.5-3x slower
\ than a performance core. That only adds time and a regression slows every
\ definition, so a set's least is its cost on the fastest core the run found,
\ the number test/compile-floor-gate.f judges; a cost only some definitions pay
\ shows in the mean alone. One tier-0 pass is about 4 ms of CPU and fits in one
\ slow stretch (at load average 90 on 12 cores, one run in 96 read
\ least-trivial-t0 75 us against a usual 34), so ROUNDS spread every set over
\ about half a second of CPU. Quote a number with its host and load.

: COMPILE-ONE ( n -- n )           \ CPU nanoseconds for one definition
   TIME:THREAD-CPU-NS swap
   DEF$ evaluate-closed
   TIME:THREAD-CPU-NS swap - ;

: NS-CELL ( n -- ptr n ) cells SET-NS + ;

: LEAST-CELL ( n -- ptr n ) cells SET-LEAST + ;

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

\ A measured set is scaffolding: a second MAIN in one process would collide with
\ every name (the engine refuses a redefinition), so removing them is what lets
\ test/compiler/compile-floor.f's in-process controls call this tool twice.
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
\ COMPILE-SET defines every name in the set or throws, so TEARDOWN's count is
\ exact; a throw leaves SET-LIVE? clear and the partial set in the dictionary,
\ reachable only if compiling `1 +` fails, which kills the run anyway.
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

\ `swap drop` carries no combinable pair and `1 +` carries one, so the two sets
\ together price the fold. The tier-0 set compiles the trivial body under its
\ own names so the contrast never redefines a tier-1 set.
: RUN-TIER1 ( -- )
   1 set-tier
   s" T" s"  ( n -- n ) 1 + ; " TRIV-T1 MEASURE
   SET-N DISPATCHES s" the trivial tier-1 set" EXPECT
   s" U" s"  ( n n -- n ) swap drop ; " THREE-T1 MEASURE
   SET-N DISPATCHES s" the three-op tier-1 set" EXPECT ;

: RUN-TIER0 ( -- )
   0 set-tier
   s" V" s"  ( n -- n ) 1 + ; " TRIV-T0 MEASURE
   0 DISPATCHES s" the trivial tier-0 set" EXPECT ;

: REPORT ( -- )
   SB-RESET
   s" floor: trivial-t1 " SB-APPEND  TRIV-T1 MEAN-US FMT:SB-U
   s"  three-op-t1 " SB-APPEND       THREE-T1 MEAN-US FMT:SB-U
   HB-TARGET-LINUX-X86-64? 0= if
      s"  trivial-t0 " SB-APPEND     TRIV-T0 MEAN-US FMT:SB-U
   then
   s"  compiled " SB-APPEND          NC-COUNT @ FMT:SB-U
   s"  least-trivial-t1 " SB-APPEND  TRIV-T1 LEAST-US FMT:SB-U
   s"  least-three-op-t1 " SB-APPEND THREE-T1 LEAST-US FMT:SB-U
   HB-TARGET-LINUX-X86-64? 0= if
      s"  least-trivial-t0 " SB-APPEND TRIV-T0 LEAST-US FMT:SB-U
   then
   SB$ type cr ;

\ ---- the ratchet ------------------------------------------------------------
\ The ratchet judges the trivial-t1 mean (target 500 us) as printed: a
\ program pays for every definition it compiles, and the mean carries costs no
\ least sees. It carries the cores too, so ratchet on a quiet machine;
\ test/compile-floor-gate.f, which runs at any load, judges each least against
\ a budget and each mean only against a loose ceiling.

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
\ The dispatch cell, the tier and the measurement names are the engine's: MAIN
\ takes them after parsing and gives all three back through `finally`, on the
\ refusing path too. A wrapper installed at load would count every later tier-1
\ definition, a tier left selected hands the caller another compiler, and names
\ left behind make a second MAIN die of `duplicate definition: T1`.

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
   PRIOR-TIER @ set-tier ;

: MEASURE-ALL ( -- )
   RESET-SETS
   ROUNDS 0 ?do
      RUN-TIER1
      HB-TARGET-LINUX-X86-64? 0= if RUN-TIER0 then
   loop
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
