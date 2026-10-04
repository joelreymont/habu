\ compile-floor-gate.f - checked performance budgets for available compiler tiers.
\
\ The child tools do the measurement; this gate only parses their machine-readable
\ lines and compares each number with a named budget. Both tools time in this
\ thread's CPU time (TIME:THREAD-CPU-NS), so a slice the scheduler hands a
\ neighbour is not in any number. What load still moves is the core: this
\ machine has 8 performance and 4 efficiency cores, and the same compile costs
\ 2.5-3x as much on an efficiency core. That only ever adds time, so the
\ gate judges the cheapest sample - the floor line's least- fields (each set's
\ cheapest definition in five rounds) and the least of a B line's fifteen
\ rounds - which is the cost on the fastest core the run found. Each tool says
\ why its rounds outlast a slow stretch.
\
\ A least moves only when every sample pays a cost, so the floor means have a
\ ceiling as well: a cost only some definitions pay shows in the mean alone.
\ Each ceiling is above every mean measured and below three times the lowest,
\ so a cost that triples a mean is red at any load: a scratch copy of
\ tools/compile-floor.f in which every other definition costs five times its
\ compile tripled the means and left the leasts alone, and only the ceilings
\ turned it red. A run confined to the efficiency cores (taskpolicy -b) read
\ trivial-t1 means of 1296-1587 us, across its 1300 ceiling, so the ceilings
\ hold for a run at any core QoS but background.
\
\ MEASURED on 2026-10-01 on this machine: twenty standalone runs at load
\ average 16-111 beside 36 busy loops, with the tools' earlier single floor
\ round and three bench rounds, and the current tools beside seven copies of
\ themselves and a native build at load average 143-172 (96 floor runs, 32
\ bench runs per tier). Each ARM constant carries the range of its judged number
\ over all of them. TOLERANCE: every budget is about 1.5x its maximum and
\ below twice its minimum, so a doubled compile or corpus cost is red (proved
\ with scratch copies of both tools doing twice the work per window). The
\ child output is printed on every run: the pool keeps it for a red suite, and
\ a standalone run (`bin/hb --load test/compile-floor-gate.f`) shows the
\ numbers on a green engine. Lower a budget when a floor dot lands and the new
\ maximum is known.
\ The x86-64 arithmetic and branch budgets use five standalone tier-1 corpus
\ runs on that native engine; their least ranges are recorded beside the two
\ constants. The other tier-1 limits already cover those native measurements.

require lib/errors.f
require lib/string.f
require lib/test.f
require lib/process.f
require lib/process-argv.f
require lib/process-env.f
require lib/test/outcome.f

package COMPILE-FLOOR-GATE

private

\ Budgets on the leasts in microseconds; the comment is the measured range.
710 constant TRIVIAL-T1-BUDGET      \ 460..468
660 constant THREE-OP-T1-BUDGET     \ 429..436
56 constant TRIVIAL-T0-BUDGET       \ 33..37
13200 constant T0-ARITH-BUDGET      \ 7996..8744
21000 constant T0-BRANCH-BUDGET     \ 12113..13676
200 constant T0-SEARCH-BUDGET       \ 126..128
6600 constant T0-FOLD-BUDGET        \ 4261..4350
320 constant T0-MOVE-BUDGET         \ 211..213
5400 constant T0-LINES-BUDGET       \ 3225..3596
1300 constant T1-ARITH-BUDGET       \ 849..866
2300 constant T1-BRANCH-BUDGET      \ 1412..1475
3000 constant X64-T1-ARITH-BUDGET   \ x86-64 leasts: 1918..1958
7100 constant X64-T1-BRANCH-BUDGET  \ x86-64 leasts: 4524..4682
38 constant T1-SEARCH-BUDGET        \ 24..25
1600 constant T1-FOLD-BUDGET        \ 1007..1017
320 constant T1-MOVE-BUDGET         \ 211..211
1850 constant T1-LINES-BUDGET       \ 1189..1221

\ Floor mean ceilings in microseconds; the comment is the measured range.
1300 constant TRIVIAL-T1-MEAN-CEILING   \ 481..661
1250 constant THREE-OP-T1-MEAN-CEILING  \ 468..632
100 constant TRIVIAL-T0-MEAN-CEILING    \ 35..69

$10000 constant OUT-CAP
$4000 constant ERR-CAP
180000 constant TIMEOUT-MS
32 constant SP
10 constant LF
48 constant ZERO-C
create OUT OUT-CAP allot
create ERR ERR-CAP allot
create TIER-BUF 1 allot
variable OUT-U
variable ERR-U
variable RC
variable EXITED
variable PARSE-POS

: OUT$ ( -- ptr u8 n ) OUT OUT-U @ ;
: ERR$ ( -- ptr u8 n ) ERR ERR-U @ ;

: STORE! ( len len outcome ptr u8 n -- )
   {: outu:len erru:len oc src:ptr srcu:n :}
   outu LEN>N OUT-U !  erru LEN>N ERR-U !
   oc MATCH outcome
     exited OF RC ! 0 0= EXITED ! ENDOF
     signaled OF RC ! 0 0= 0= EXITED ! ENDOF
     timeout OF src srcu OUT$ ERR$ T-TIMED-OUT ENDOF
   ;MATCH ;

: HB$ ( -- ptr u8 n )
   s" HABU_UNDER_TEST" >LEN PROC-ENV-DEFAULT$? if LEN>N exit then
   2drop
   s" HABU_UNDER_TEST" GETENV dup 0= if
      2drop s" bin/hb" exit
   then ;

: ARG+ ( ptr u8 n -- ) >LEN PROC-ARGV+ ;
: FLOOR$ ( -- ptr u8 n ) s" tools/compile-floor.f" ;
: BENCH$ ( -- ptr u8 n ) s" tools/tier-bench.f" ;

\ Report only: the budget is applied here, not by the tool's ratchet, so the
\ number has one home.
: RUN-FLOOR ( -- )
   PROC-ARGV-RESET
   s" --load" ARG+
   FLOOR$ ARG+
   HB$ >LEN OUT OUT-CAP >LEN ERR ERR-CAP >LEN TIMEOUT-MS >MS
   RUN-ARGV-CAPTURE-OUTCOME FLOOR$ STORE! ;

: RUN-BENCH ( n -- ) {: tier:n :}
   PROC-ARGV-RESET
   s" --load" ARG+
   BENCH$ ARG+
   s" --" ARG+
   tier ZERO-C + TIER-BUF c!
   TIER-BUF 1 ARG+
   s" lib/string.f" ARG+
   HB$ >LEN OUT OUT-CAP >LEN ERR ERR-CAP >LEN TIMEOUT-MS >MS
   RUN-ARGV-CAPTURE-OUTCOME BENCH$ STORE! ;

: SKIP-WS ( ptr u8 n n -- n ) {: a:ptr u:n from:n :}
   from begin dup u < while
      dup a + c@ SP <= if 1+ else exit then
   repeat ;

: TOKEN-END ( ptr u8 n n -- n ) {: a:ptr u:n from:n :}
   from begin dup u < while
      dup a + c@ SP > if 1+ else exit then
   repeat ;

: FIELD-AT ( ptr u8 n n n -- n ) {: a:ptr u:n from:n field:n :}
   from PARSE-POS !
   field 0 ?do
      a u PARSE-POS @ SKIP-WS {: ws:n :}
      a u ws TOKEN-END 1+ PARSE-POS !
   loop
   a u PARSE-POS @ SKIP-WS {: start:n :}
   a u start TOKEN-END {: finish:n :}
   a start + finish start -
   STR>NUMBER? MATCH option
     none OF s" compile-floor-gate: malformed measurement number" 74 die ENDOF
     some OF ENDOF
   ;MATCH ;

: LABEL-END ( ptr u8 n ptr u8 n -- n ) {: a:ptr u:n label:ptr labelu:n :}
   a u label labelu FIND-SUB MATCH option
     none OF s" compile-floor-gate: missing measurement label" 74 die ENDOF
     some OF IDX>N ENDOF
   ;MATCH labelu + ;

: FIELD-LABEL ( ptr u8 n ptr u8 n n -- n ) {: a:ptr u:n label:ptr labelu:n field:n :}
   a u label labelu LABEL-END {: from:n :}
   a u from field FIELD-AT ;

: LINE-END ( ptr u8 n n -- n ) {: a:ptr u:n from:n :}
   from begin dup u < while
      dup a + c@ LF = if exit then
      1+
   repeat ;

\ How many numbers lie between from and u.
: TOKEN-COUNT ( ptr u8 n n -- n ) {: a:ptr u:n from:n :}
   from PARSE-POS !
   0 begin
      a u PARSE-POS @ SKIP-WS dup u <
   while
      a u rot TOKEN-END PARSE-POS !
      1+
   repeat drop ;

: REPORT-OUT ( -- ) OUT$ type cr ;

: CHILD-OK? ( -- bool ) EXITED @ RC @ 0= and ;

: CHECK-LIMIT ( n n -- ) <= TTRUE ;

: CHECK-FLOOR ( -- )
   OUT$ s" least-trivial-t1 " 0 FIELD-LABEL TRIVIAL-T1-BUDGET CHECK-LIMIT
   OUT$ s" least-three-op-t1 " 0 FIELD-LABEL THREE-OP-T1-BUDGET CHECK-LIMIT
   OUT$ s" floor: trivial-t1 " 0 FIELD-LABEL TRIVIAL-T1-MEAN-CEILING CHECK-LIMIT
   OUT$ s"  three-op-t1 " 0 FIELD-LABEL THREE-OP-T1-MEAN-CEILING CHECK-LIMIT
   HB-TARGET-LINUX-X86-64? 0= if
      OUT$ s" least-trivial-t0 " 0 FIELD-LABEL TRIVIAL-T0-BUDGET CHECK-LIMIT
      OUT$ s"  trivial-t0 " 0 FIELD-LABEL TRIVIAL-T0-MEAN-CEILING CHECK-LIMIT
   then ;

\ The least of a B line's rounds: every number on it but the last, the median.
: LEAST-RUN ( ptr u8 n ptr u8 n -- n ) {: a:ptr u:n label:ptr labelu:n :}
   a u label labelu LABEL-END {: from:n :}
   a u from LINE-END {: eol:n :}
   a eol from TOKEN-COUNT 1 - {: rounds:n :}
   rounds 1 < if s" compile-floor-gate: a B line without a round" 74 die then
   a eol from 0 FIELD-AT
   rounds 1 ?do a eol from i FIELD-AT min loop ;

: CHECK-BENCH ( ptr u8 n ptr u8 n n -- )
   {: a:ptr u:n label:ptr labelu:n budget:n :}
   a u label labelu LEAST-RUN budget CHECK-LIMIT ;

: RUN-FLOOR-CASE ( -- )
   RUN-FLOOR
   REPORT-OUT
   CHILD-OK? TTRUE
   OUT$ s" floor: trivial-t1 " CONTAINS? TTRUE
   OUT$ s" compiled 1000 " CONTAINS? TTRUE
   CHECK-FLOOR ;

: RUN-BENCH-CASE ( n -- )
   dup RUN-BENCH
   REPORT-OUT
   CHILD-OK? TTRUE
   OUT$ s" B harness " CONTAINS? TTRUE
   OUT$ s" B lines " CONTAINS? TTRUE
   dup 0= if
      OUT$ s" B arith " T0-ARITH-BUDGET CHECK-BENCH
      OUT$ s" B branch " T0-BRANCH-BUDGET CHECK-BENCH
      OUT$ s" B search " T0-SEARCH-BUDGET CHECK-BENCH
      OUT$ s" B fold " T0-FOLD-BUDGET CHECK-BENCH
      OUT$ s" B move " T0-MOVE-BUDGET CHECK-BENCH
      OUT$ s" B lines " T0-LINES-BUDGET CHECK-BENCH
   else
      HB-TARGET-LINUX-X86-64? if
         OUT$ s" B arith " X64-T1-ARITH-BUDGET CHECK-BENCH
         OUT$ s" B branch " X64-T1-BRANCH-BUDGET CHECK-BENCH
      else
         OUT$ s" B arith " T1-ARITH-BUDGET CHECK-BENCH
         OUT$ s" B branch " T1-BRANCH-BUDGET CHECK-BENCH
      then
      OUT$ s" B search " T1-SEARCH-BUDGET CHECK-BENCH
      OUT$ s" B fold " T1-FOLD-BUDGET CHECK-BENCH
      OUT$ s" B move " T1-MOVE-BUDGET CHECK-BENCH
      OUT$ s" B lines " T1-LINES-BUDGET CHECK-BENCH
   then drop ;

public

: MAIN ( -- )
   T-RESET
   s" compile floor: available tier stays within the measured budget" T-LABEL
   RUN-FLOOR-CASE
   HB-TARGET-LINUX-X86-64? 0= if
      s" tier-0 corpus: every benchmark stays within its budget" T-LABEL
      0 RUN-BENCH-CASE
   then
   s" tier-1 corpus: every benchmark stays within its budget" T-LABEL
   1 RUN-BENCH-CASE
   T-REPORT ;

;package

COMPILE-FLOOR-GATE:MAIN
