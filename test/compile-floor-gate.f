\ compile-floor-gate.f - checked performance budgets for the two compiler tiers.
\
\ The child tools do the measurement; this gate only parses their machine-readable
\ lines and compares each number with a named budget. The budgets were set from
\ ten runs on a quiet, pinned core (2026-09-21, load under 3); each constant
\ carries its ten-run range. TOLERANCE: every budget is at least 1.5x its
\ ten-run maximum - the ten-run spread itself is under 5%, the rest is for a
\ core the gate pool saturates - and the smallest numbers (search, move) get
\ about 2x because a few microseconds of timer granularity is a large share of
\ them. A doubled compile or corpus cost is red. The child output is printed on
\ every run: the pool keeps it for a red suite, and a standalone run (`bin/hb
\ --load test/compile-floor-gate.f`) shows the numbers on a green engine. Lower
\ a budget when a floor dot lands and the new ten-run maximum is known.
\
\ The pinned core is the slow bound of this machine: the same suite unpinned in
\ a workspace at load average 5 measured trivial-t1 378 us and tier-0 arith
\ 8068 us, 2.5-4x under the pinned numbers.

require lib/errors.f
require lib/string.f
require lib/test.f
require lib/process.f
require lib/process-argv.f
require lib/process-env.f

package COMPILE-FLOOR-GATE

private

\ Budgets in microseconds; the comment is the ten-run range on the pinned core.
1800 constant TRIVIAL-T1-BUDGET     \ 1109..1123
1600 constant THREE-OP-T1-BUDGET    \ 974..984
100 constant TRIVIAL-T0-BUDGET      \ 56..59
35000 constant T0-ARITH-BUDGET      \ 21864..21944
55000 constant T0-BRANCH-BUDGET     \ 34404..36017
400 constant T0-SEARCH-BUDGET       \ 187..206
12000 constant T0-FOLD-BUDGET       \ 7183..7264
600 constant T0-MOVE-BUDGET         \ 328..331
9000 constant T0-LINES-BUDGET       \ 5629..5647
3000 constant T1-ARITH-BUDGET       \ 1644..1704
12000 constant T1-BRANCH-BUDGET     \ 6957..7009
100 constant T1-SEARCH-BUDGET       \ 42..43
3000 constant T1-FOLD-BUDGET        \ 1605..1622
600 constant T1-MOVE-BUDGET         \ 313..318
3500 constant T1-LINES-BUDGET       \ 1911..1946

$10000 constant OUT-CAP
$4000 constant ERR-CAP
180000 constant TIMEOUT-MS
32 constant SP
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

: STORE! ( len len outcome -- )
   MATCH outcome
     exited OF RC ! 0 0= EXITED ! ENDOF
     signaled OF RC ! 0 0= 0= EXITED ! ENDOF
     timeout OF 0 RC ! 0 0= 0= EXITED ! ENDOF
   ;MATCH
   LEN>N ERR-U ! LEN>N OUT-U ! ;

: HB$ ( -- ptr u8 n )
   s" HABU_UNDER_TEST" >LEN PROC-ENV-DEFAULT$? if LEN>N exit then
   2drop
   s" HABU_UNDER_TEST" GETENV dup 0= if
      2drop s" bin/hb" exit
   then ;

: ARG+ ( ptr u8 n -- ) >LEN PROC-ARGV+ ;

\ Report only: the budget is applied here, not by the tool's ratchet, so the
\ number has one home.
: RUN-FLOOR ( -- )
   PROC-ARGV-RESET
   s" --load" ARG+
   s" tools/compile-floor.f" ARG+
   HB$ >LEN OUT OUT-CAP >LEN ERR ERR-CAP >LEN TIMEOUT-MS >MS
   RUN-ARGV-CAPTURE-OUTCOME STORE! ;

: RUN-BENCH ( n -- ) {: tier:n :}
   PROC-ARGV-RESET
   s" --load" ARG+
   s" tools/tier-bench.f" ARG+
   s" --" ARG+
   tier ZERO-C + TIER-BUF c!
   TIER-BUF 1 ARG+
   s" lib/string.f" ARG+
   HB$ >LEN OUT OUT-CAP >LEN ERR ERR-CAP >LEN TIMEOUT-MS >MS
   RUN-ARGV-CAPTURE-OUTCOME STORE! ;

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

: FIELD-LABEL ( ptr u8 n ptr u8 n n -- n ) {: a:ptr u:n label:ptr labelu:n field:n :}
   a u label labelu FIND-SUB MATCH option
     none OF s" compile-floor-gate: missing measurement label" 74 die ENDOF
     some OF IDX>N ENDOF
   ;MATCH labelu + {: from:n :}
   a u from field FIELD-AT ;

: REPORT-OUT ( -- ) OUT$ type cr ;

: CHILD-OK? ( -- bool ) EXITED @ RC @ 0= and ;

: CHECK-LIMIT ( n n -- ) <= TTRUE ;

: CHECK-FLOOR ( -- )
   OUT$ s" floor: trivial-t1 " 0 FIELD-LABEL TRIVIAL-T1-BUDGET CHECK-LIMIT
   OUT$ s" three-op-t1 " 0 FIELD-LABEL THREE-OP-T1-BUDGET CHECK-LIMIT
   OUT$ s" trivial-t0 " 0 FIELD-LABEL TRIVIAL-T0-BUDGET CHECK-LIMIT ;

: CHECK-BENCH ( ptr u8 n ptr u8 n n -- )
   {: a:ptr u:n label:ptr labelu:n budget:n :}
   a u label labelu 3 FIELD-LABEL budget CHECK-LIMIT ;

: RUN-FLOOR-CASE ( -- )
   RUN-FLOOR
   REPORT-OUT
   CHILD-OK? TTRUE
   OUT$ s" floor: trivial-t1 " CONTAINS? TTRUE
   OUT$ s" compiled 200" CONTAINS? TTRUE
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
      OUT$ s" B arith " T1-ARITH-BUDGET CHECK-BENCH
      OUT$ s" B branch " T1-BRANCH-BUDGET CHECK-BENCH
      OUT$ s" B search " T1-SEARCH-BUDGET CHECK-BENCH
      OUT$ s" B fold " T1-FOLD-BUDGET CHECK-BENCH
      OUT$ s" B move " T1-MOVE-BUDGET CHECK-BENCH
      OUT$ s" B lines " T1-LINES-BUDGET CHECK-BENCH
   then drop ;

public

: MAIN ( -- )
   T-RESET
   s" compile floor: both tiers stay within the measured budget" T-LABEL
   RUN-FLOOR-CASE
   s" tier-0 corpus: every benchmark stays within its budget" T-LABEL
   0 RUN-BENCH-CASE
   s" tier-1 corpus: every benchmark stays within its budget" T-LABEL
   1 RUN-BENCH-CASE
   T-REPORT ;

;package

COMPILE-FLOOR-GATE:MAIN
