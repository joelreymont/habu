\ tier-bench.f - run time of tier-0 and tier-1 code on the same real words.
\ Run: bin/hb --load tools/tier-bench.f -- <tier> <source-file>
\ Per benchmark: `B <name> <run1> ... <run15> <median>` in microseconds of thread
\ CPU, then `R <name> <value>`, its result over the whole output. Every round
\ must compute run 0's value; test/compile-floor-gate.f judges the least run and
\ requires the tiers to agree. Subtract `harness`, the bare timing loop.

\ The clock loads ahead of the selection, at the engine's tier: after it, its FFI
\ and task runtime cost a tier-1 run about a second of compiling. The files it
\ shares with the targets are baked (src/lib/errors.f, string.f, memory.f,
\ adt/option.f), so no word under test compiles before the selection.
require src/lib/time-cpu.f

package TIER-SELECT
private

64 constant USAGE-RC                \ sysexits EX_USAGE
48 constant ZERO-C

public

\ Top-level code ahead of the targets' requires: only code compiled after
\ `set-tier` is the selected tier's, and baked words (the checker, the compiler,
\ src/lib/string.f, the cold prefix) read the same at both tiers. Every word under
\ test comes from src/lib/array.f or tools/lint/text.f, loaded after this.
: APPLY ( -- )
   script-argc 2 < if
      s" usage: bin/hb --load tools/tier-bench.f -- <tier> <source-file>"
      USAGE-RC die
   then
   0 script-argv$ {: a:ptr u :}
   u 1 <> if s" tier-bench: tier must be 0 or 1" USAGE-RC die then
   a c@ ZERO-C - {: t :}
   t 0 < t 1 > or if s" tier-bench: tier must be 0 or 1" USAGE-RC die then
   t set-tier ;

;package

TIER-SELECT:APPLY

require src/lib/array.f
require tools/lint/text.f

package TIER-BENCH
private

$40000 constant CELLS-N             \ 262144 cells under the integer loops
$100000 constant TEXT-CAP           \ destination for the byte-moving benchmarks
1000 constant MAX-LINES             \ LINT-SPLIT:SPLIT-LINES holds $400 fields
1000 constant NS-PER-US
15 constant RUNS
32 constant DIG-CAP
$0A constant LF
$20 constant SP
48 constant ZERO-C
74 constant IO-RC                   \ sysexits EX_IOERR
70 constant WRONG-RC                \ sysexits EX_SOFTWARE
64 constant MSG-CAP

\ Repeat counts, one per benchmark, chosen so a run lands in tens of
\ milliseconds at tier 0 on this machine: long enough to swamp the clock read,
\ short enough that fifteen rounds cost about half a second of CPU at tier 0.
8 constant ARITH-REPS
8 constant BRANCH-REPS
1 constant SEARCH-REPS
32 constant FOLD-REPS
64 constant MOVE-REPS
32 constant LINES-REPS
1000000 constant HARNESS-REPS

create ABUF CELLS-N cells allot
create DEST TEXT-CAP allot

create DIG DIG-CAP allot
create CHB 1 allot                  \ EMIT-C's own byte; sharing DIG would let it
variable DEC-V                      \ overwrite the digit EMIT-DEC has not read yet
variable DEC-K

\ RUN-NS holds RUNS samples per benchmark, one row per benchmark.
0 constant HARNESS-ROW
1 constant ARITH-ROW
2 constant BRANCH-ROW
3 constant SEARCH-ROW
4 constant FOLD-ROW
5 constant MOVE-ROW
6 constant LINES-ROW
7 constant ROWS
create RUN-NS ROWS RUNS * cells allot
create SORTED RUNS cells allot      \ one row's samples, ascending: its median
create RESULTS ROWS cells allot     \ each benchmark's SINK after run 0
create MSG MSG-CAP allot            \ KEEP's message, which names the benchmark
TYPED-VARIABLE MSG-N len

variable TEXT-N                     \ bytes of the loaded source actually used
variable LINES-N                    \ bytes of it holding at most MAX-LINES lines
variable SINK                       \ keeps a benchmark's result from being dead
variable NEEDLE-N                   \ bytes of the text's tail B-SEARCH looks for

\ ---- output -----------------------------------------------------------------

: EMIT-C ( n -- ) {: c :}
   c CHB c!
   CHB 1 type ;

: EMIT-DEC ( n -- )
   DEC-V !
   DEC-V @ 0= if ZERO-C EMIT-C exit then
   0 DEC-K !
   begin DEC-V @ 0 > while
      DEC-V @ 10 mod ZERO-C + DIG DEC-K @ + c!
      DEC-K @ 1 + DEC-K !
      DEC-V @ 10 / DEC-V !
   repeat
   begin DEC-K @ 0 > while
      DEC-K @ 1 - DEC-K !
      DIG DEC-K @ + c@ EMIT-C
   repeat ;

: EMIT-FIELD ( n -- ) SP EMIT-C EMIT-DEC ;


: ROW-NAME$ ( n -- ptr u8 n ) {: row :}
   row HARNESS-ROW = if s" harness" exit then
   row ARITH-ROW = if s" arith" exit then
   row BRANCH-ROW = if s" branch" exit then
   row SEARCH-ROW = if s" search" exit then
   row FOLD-ROW = if s" fold" exit then
   row MOVE-ROW = if s" move" exit then
   row LINES-ROW = if s" lines" exit then
   s" tier-bench: no such benchmark row" WRONG-RC die ;

\ ---- the source under the text benchmarks -----------------------------------

: TEXT$ ( -- ptr u8 n ) LINT-SOURCE:TEXT drop TEXT-N @ ;

: LINES$ ( -- ptr u8 n ) LINT-SOURCE:TEXT drop LINES-N @ ;

\ LINT-SPLIT:SPLIT-LINES dies rather than truncating when a file has more
\ fields than its table holds, so the prefix handed to it is cut at MAX-LINES
\ newlines instead of at a byte count that happens to work for one file.
: LINE-LIMIT ( -- )
   LINT-SOURCE:TEXT drop {: a:ptr :}
   0 {: seen :}
   TEXT-N @ LINES-N !
   seen TEXT-N @ 0 ?do
      a i + c@ LF = if 1 + then
      dup MAX-LINES >= if i LINES-N ! leave then
   loop drop ;

: LOAD-SOURCE ( ptr u8 n -- )
   LINT-SOURCE:LOAD
   LINT-SOURCE:TEXT nip {: got :}
   got 0= if s" tier-bench: source file is empty" IO-RC die then
   got TEXT-CAP > if TEXT-CAP else got then TEXT-N !
   LINE-LIMIT ;

: FILL-CELLS ( -- )
   CELLS-N 0 ?do i 7 mod ABUF i cells + ! loop ;

\ The offset where the first match starts; the text's length when there is none.
: FOUND-AT ( option<n> -- n )
   MATCH option
     none OF TEXT-N @ ENDOF
     some OF ENDOF
   ;MATCH ;

: NEEDLE$ ( -- ptr u8 n )
   LINT-SOURCE:TEXT drop TEXT-N @ NEEDLE-N @ - + NEEDLE-N @ ;

: SEARCH-AT ( -- n ) TEXT$ NEEDLE$ LINT-FIND-SUB FOUND-AT ;


\ The shortest tail of the text that occurs nowhere earlier in it, so a search
\ for it tries every offset and its first match is the tail itself.
: PICK-NEEDLE ( -- )
   1 NEEDLE-N !
   begin
      NEEDLE-N @ TEXT-N @ < if SEARCH-AT TEXT-N @ NEEDLE-N @ - <> else false then
   while
      NEEDLE-N @ 1 + NEEDLE-N !
   repeat ;

: CLEAR-DEST ( -- ) TEXT-N @ 0 ?do 0 DEST i + c! loop ;


\ Each byte of DEST that fold and move write, weighted by its position, so a
\ byte left out, wrong or out of place changes the sum.
: SUM-DEST ( -- )
   0 TEXT-N @ 0 ?do DEST i + c@ i 1 + * + loop SINK ! ;

\ ---- the benchmark bodies ---------------------------------------------------
\ Each leaves something in SINK so the work cannot be dropped as unused. For
\ fold and move that is one byte; their timing words replace it, outside the
\ window, with SUM-DEST over the whole output.

: B-HARNESS ( -- )
   HARNESS-REPS 0 ?do i SINK ! loop ;

: B-ARITH ( -- )
   ARITH-REPS 0 ?do
      ABUF CELLS-N ARRAY:A-LEN ARRAY:A-SUM SINK !
   loop ;

: B-BRANCH ( -- )
   BRANCH-REPS 0 ?do
      ABUF CELLS-N ARRAY:A-LEN ARRAY:A-COUNT-EVEN COUNT>N SINK !
   loop ;

\ PICK-NEEDLE's tail first occurs at the end, so the scan runs to the end of
\ the file every time, and the offset it returns shows where it stopped.
: B-SEARCH ( -- )
   SEARCH-REPS 0 ?do SEARCH-AT SINK ! loop ;

: B-FOLD ( -- )
   FOLD-REPS 0 ?do
      TEXT$ DEST FOLD-TO
      DEST c@ SINK !
   loop ;

: B-MOVE ( -- )
   MOVE-REPS 0 ?do
      LINT-SOURCE:TEXT drop DEST TEXT-N @ LINT-BMOVE
      DEST c@ SINK !
   loop ;

: B-LINES ( -- )
   LINES-REPS 0 ?do
      LINES$ LINT-SPLIT:SPLIT-LINES
      LINT-SPLIT:SN# @ SINK !
   loop ;

\ ---- timing -----------------------------------------------------------------

: SAMPLE ( n n -- ptr n ) {: row run :}
   RUN-NS row RUNS * run + cells + ;

: SORTED-AT ( n -- ptr n ) cells SORTED + ;
: RESULT-AT ( n -- ptr n ) cells RESULTS + ;

\ v into SORTED's first k samples, which are in ascending order.
: INSERT ( n n -- ) {: v k :}
   k begin
      dup 0 > if dup 1 - SORTED-AT @ v > else false then
   while
      dup 1 - SORTED-AT @ over SORTED-AT !
      1 -
   repeat
   SORTED-AT v swap ! ;

\ RUNS is odd, so the median is one sample.
: MEDIAN ( n -- n ) {: row :}
   RUNS 0 ?do row i SAMPLE @ i INSERT loop
   RUNS 2 / SORTED-AT @ ;

: REPORT ( n -- ) {: row :}
   s" B " type
   row ROW-NAME$ type
   RUNS 0 ?do row i SAMPLE @ NS-PER-US / EMIT-FIELD loop
   row MEDIAN NS-PER-US / EMIT-FIELD
   LF EMIT-C ;


: RESULT ( n -- ) {: row :}
   s" R " type
   row ROW-NAME$ type
   row RESULT-AT @ EMIT-FIELD
   LF EMIT-C ;


: MSG+ ( ptr u8 n -- ) MSG MSG-CAP MSG-N BUF-APPEND ;


\ Run 0 records each benchmark's result; a later round that computes another
\ one has run a miscompiled word, whose time would mean nothing. Called after
\ the window's closing clock read, so the check is not timed.
: KEEP ( n n -- ) {: row run :}
   run 0= if SINK @ row RESULT-AT ! exit then
   SINK @ row RESULT-AT @ = if exit then
   MSG-N BUF-RESET
   s" tier-bench: " MSG+
   row ROW-NAME$ MSG+
   s"  computed a different result in a later round" MSG+
   MSG MSG-N BUF-LEN@ WRONG-RC die ;

\ One `xt` per benchmark would need a quotation the checker can call from a
\ loop; naming the bodies and timing each in its own word costs one line each
\ and keeps every body a plain checked call.
: TIME-HARNESS ( n -- ) {: run :}
   TIME:THREAD-CPU-NS B-HARNESS TIME:THREAD-CPU-NS swap - HARNESS-ROW run SAMPLE !
   HARNESS-ROW run KEEP ;
: TIME-ARITH ( n -- ) {: run :}
   TIME:THREAD-CPU-NS B-ARITH TIME:THREAD-CPU-NS swap - ARITH-ROW run SAMPLE !
   ARITH-ROW run KEEP ;
: TIME-BRANCH ( n -- ) {: run :}
   TIME:THREAD-CPU-NS B-BRANCH TIME:THREAD-CPU-NS swap - BRANCH-ROW run SAMPLE !
   BRANCH-ROW run KEEP ;
: TIME-SEARCH ( n -- ) {: run :}
   TIME:THREAD-CPU-NS B-SEARCH TIME:THREAD-CPU-NS swap - SEARCH-ROW run SAMPLE !
   SEARCH-ROW run KEEP ;
: TIME-FOLD ( n -- ) {: run :}
   CLEAR-DEST
   TIME:THREAD-CPU-NS B-FOLD TIME:THREAD-CPU-NS swap - FOLD-ROW run SAMPLE !
   SUM-DEST FOLD-ROW run KEEP ;
: TIME-MOVE ( n -- ) {: run :}
   CLEAR-DEST
   TIME:THREAD-CPU-NS B-MOVE TIME:THREAD-CPU-NS swap - MOVE-ROW run SAMPLE !
   SUM-DEST MOVE-ROW run KEEP ;
: TIME-LINES ( n -- ) {: run :}
   TIME:THREAD-CPU-NS B-LINES TIME:THREAD-CPU-NS swap - LINES-ROW run SAMPLE !
   LINES-ROW run KEEP ;

\ Rounds are the outer loop, so a benchmark's runs lie a round apart, not back
\ to back on one slow core: thread CPU time excludes a neighbour's slice but not
\ a slow core or an evicted cache, which only ever read slower. The gate judges
\ the least run, so the rounds must outlast a slow stretch: on a 12-core machine
\ at load 90 (32 runs of 101 rounds per tier) a benchmark ran over budget at most
\ five rounds in a row, never seven. A round costs about 31 ms of CPU at tier 0
\ and 8 ms at tier 1.
: ROUND ( n -- ) {: run :}
   run TIME-HARNESS
   run TIME-ARITH
   run TIME-BRANCH
   run TIME-SEARCH
   run TIME-FOLD
   run TIME-MOVE
   run TIME-LINES ;

public

: MAIN ( -- )
   1 script-argv$ LOAD-SOURCE
   PICK-NEEDLE
   FILL-CELLS
   RUNS 0 ?do i ROUND loop
   ROWS 0 ?do i REPORT loop
   ROWS 0 ?do i RESULT loop ;

;package

TIER-BENCH:MAIN
