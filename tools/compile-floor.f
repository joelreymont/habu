\ compile-floor.f - the tier-0 per-definition compile floor: its mean and least.
\   bin/hb --load tools/compile-floor.f [-- FLOOR-MS]   with FLOOR-MS: refuse
\   a larger trivial-t0 mean, E-FLOOR-EXCEEDED. One line, us per definition:
\   floor: trivial-t0 <us> least-trivial-t0 <us>

require src/lib/error.f
require src/lib/string.f
require src/lib/fmt.f
require src/lib/float.f
require src/lib/argv.f
require src/lib/time-cpu.f
require src/lib/fd.f

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

8 constant PREFIX-CAP
create LIVE-PREFIX PREFIX-CAP allot
variable PREFIX-U                  \ length of the prefix in LIVE-PREFIX
variable SET-LIVE?                 \ that set is defined and not yet removed

$7FFFFFFFFFFFFFFF constant NO-LEAST \ above any least: the first sample replaces it
variable SET-NS                    \ the set's CPU ns, summed over every round
variable SET-LEAST                 \ the set's cheapest definition in any round
variable BOUND-US                  \ ratchet floor in us; read only when a floor was given

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
\ Thread CPU time leaves out the neighbours' slices and a slow core only adds
\ time: a least is the fastest core's cost; ROUNDS outlast a slow stretch.

: COMPILE-ONE ( n -- n )           \ CPU nanoseconds for one definition
   TIME:THREAD-CPU-NS swap
   DEF$ evaluate-closed
   TIME:THREAD-CPU-NS swap - ;

: COMPILE-SET ( -- )
   SET-N 0 ?do
      i COMPILE-ONE
      dup SET-NS +!
      SET-LEAST @ min SET-LEAST !
   loop ;

: MEAN-US ( -- n )  SET-NS @ NS-PER-US / SET-N ROUNDS * / ;

: LEAST-US ( -- n )  SET-LEAST @ NS-PER-US / ;

: RESET-SET ( -- )
   0 SET-NS !
   NO-LEAST SET-LEAST ! ;

\ Every round rebuilds the same names and the engine refuses a redefinition,
\ so TEARDOWN removes a measured set before the next is built.
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
: MEASURE ( ptr u8 n ptr u8 n -- ) {: px:ptr pu tx:ptr tu :}
   TEARDOWN
   px pu tx tu BUILD-SET
   COMPILE-SET
   1 SET-LIVE? ! ;

: RUN-TIER0 ( -- )
   s" V" s"  ( n -- n ) 1 + ; " MEASURE ;

: REPORT ( -- )
   SB-RESET
   s" floor: trivial-t0 " SB-APPEND  MEAN-US FMT:SB-U
   s"  least-trivial-t0 " SB-APPEND  LEAST-US FMT:SB-U
   SB$ type cr ;

\ ---- the ratchet ------------------------------------------------------------
\ The mean carries the cores, so ratchet on a quiet machine.

: FLOOR-US ( ptr u8 n -- n )       \ "0.5" -> 500
   STR>FLOAT MATCH option
     none OF s" floor must be a decimal number of milliseconds" ARGV:FAIL ENDOF
     some OF US-PER-MS s>f f* f>s ENDOF
   ;MATCH ;

: RATCHET ( -- )
   ARGV:POS# 0= if exit then
   MEAN-US BOUND-US @ <= if exit then
   SB-RESET
   s" compile-floor: trivial-t0 " SB-APPEND  MEAN-US FMT:SB-U
   s"  us is above the floor of " SB-APPEND  BOUND-US @ FMT:SB-U
   s"  us" SB-APPEND  LF SB-APPEND-C
   SB$ E-FLOOR-EXCEEDED REFUSE ;

\ The floor is read and converted BEFORE anything is measured, so a malformed
\ argument costs a usage line rather than a measurement.
: CONFIG ( -- )
   s" bin/hb --load tools/compile-floor.f -- [floor-ms]" ARGV:USAGE!
   ARGV:PARSE
   0 1 ARGV:EXPECT-POS
   ARGV:POS# 0= if exit then
   0 ARGV:POS$ FLOOR-US BOUND-US ! ;

\ On the refusing paths a measured set is still standing: MAIN removes it
\ through `finally`, so the caller gets its dictionary back.
: PUT-BACK ( -- )
   TEARDOWN ;

: MEASURE-ALL ( -- )
   RESET-SET
   ROUNDS 0 ?do
      RUN-TIER0
   loop
   REPORT
   RATCHET ;

public

\ CONFIG runs first, so a usage failure has nothing to remove. Everything after
\ it is inside the cleanup's reach.
: MAIN ( -- )
   CONFIG
   [: MEASURE-ALL ;] [: PUT-BACK ;] finally ;

;package

COMPILE-FLOOR:MAIN
