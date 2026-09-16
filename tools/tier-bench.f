\ tier-bench.f - run time of tier-0 and tier-1 code on the same real words.
\
\ Run:
\     bin/hb --load tools/tier-bench.f -- <tier> <source-file>
\
\ Prints one line per benchmark, three timed runs and their median, in
\ microseconds:
\
\     B <name> <run1> <run2> <run3> <median>
\
\ WHY THE TIER IS SELECTED BEFORE THE REQUIRES. Only code compiled after
\ `set-tier` is the selected tier's; the engine's baked words - the checker, the
\ compiler, lib/string.f, everything in the cold prefix - were compiled when the
\ engine was built and do not change. So a benchmark that ran a baked word would
\ report the same number at both tiers no matter what either compiler does. The
\ selection is therefore top-level code at the head of this file, ahead of the
\ requires, and every word under test comes from a file loaded after it:
\ lib/array.f for the integer loops and tools/lint/text.f for the text work.
\
\ WHY `harness` IS MEASURED TOO. The timing loop in this file is compiled at the
\ selected tier along with everything else, so part of every number below is the
\ loop, not the word. `harness` times the same loop over a body that does
\ nothing; subtract it before comparing a small benchmark across tiers.
\
\ WHY THREE RUNS AND A MEDIAN. This measures wall-clock time on a shared
\ machine. One run can be anything; the median of three survives a single
\ neighbour. Quote a number only with the load average it was taken at - see
\ tools/compile-floor.f, which says the same thing about compile time.

package TIER-SELECT
private

64 constant USAGE-RC                \ sysexits EX_USAGE
48 constant ZERO-C

\ `set-tier` is refused inside a plain checked body; one row, nothing else in it.
TRUSTED: SET ( n -- ) set-tier ;

public

\ Runs as top-level code, before the requires below, so the benchmark targets
\ are compiled by the tier the caller asked for.
: APPLY ( -- )
   script-argc 2 < if
      s" usage: bin/hb --load tools/tier-bench.f -- <tier> <source-file>"
      USAGE-RC die
   then
   0 script-argv$ {: a:ptr u :}
   u 1 <> if s" tier-bench: tier must be 0 or 1" USAGE-RC die then
   a c@ ZERO-C - {: t :}
   t 0 < t 1 > or if s" tier-bench: tier must be 0 or 1" USAGE-RC die then
   t SET ;

;package

TIER-SELECT:APPLY

require lib/array.f
require tools/lint/text.f

package TIER-BENCH
private

$40000 constant CELLS-N             \ 262144 cells under the integer loops
$100000 constant TEXT-CAP           \ destination for the byte-moving benchmarks
1000 constant MAX-LINES             \ LINT-SPLIT:SPLIT-LINES holds $400 fields
1000 constant NS-PER-US
3 constant RUNS
32 constant DIG-CAP
$0A constant LF
$20 constant SP
48 constant ZERO-C
74 constant IO-RC                   \ sysexits EX_IOERR

\ Repeat counts, one per benchmark, chosen so a run lands in tens of
\ milliseconds at tier 0 on this machine: long enough to swamp `mono-ns`, short
\ enough that three runs of seven benchmarks stay under a minute.
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

create RUN-NS RUNS cells allot

variable TEXT-N                     \ bytes of the loaded source actually used
variable LINES-N                    \ bytes of it holding at most MAX-LINES lines
variable SINK                       \ keeps a benchmark's result from being dead

\ ---- output -----------------------------------------------------------------
\ This file requires nothing beyond its benchmark targets, for the reason
\ tools/tier-census.f gives: a tool that loaded lib/string.f for its own output
\ would have compiled it at the engine's tier before the selection above, and
\ every word it reaches would then be missing from the comparison.

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

\ ---- the benchmark bodies ---------------------------------------------------
\ Each leaves something in SINK so the work cannot be dropped as unused.

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

\ A needle that is not there, so the scan runs to the end of the file every
\ time rather than stopping at the first hit in whichever file was passed.
: B-SEARCH ( -- )
   SEARCH-REPS 0 ?do
      TEXT$ s" zzq-absent-needle-zzq" LINT-CONTAINS? if 1 else 0 then SINK !
   loop ;

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

: MEDIAN3 ( -- n )
   RUN-NS 0 cells + @ {: a :}
   RUN-NS 1 cells + @ {: b :}
   RUN-NS 2 cells + @ {: c :}
   a b > if a b else b a then {: hi lo :}
   c lo < if lo exit then
   c hi > if hi exit then
   c ;

: REPORT ( ptr u8 n -- ) {: name:ptr nu :}
   s" B " type
   name nu type
   RUNS 0 ?do RUN-NS i cells + @ NS-PER-US / EMIT-FIELD loop
   MEDIAN3 NS-PER-US / EMIT-FIELD
   LF EMIT-C ;

\ One `xt` per benchmark would need a quotation the checker can call three
\ times; naming the bodies and timing each in its own word costs one line each
\ and keeps every body a plain checked call.
: TIME-HARNESS ( -- ) RUNS 0 ?do mono-ns B-HARNESS mono-ns swap - RUN-NS i cells + ! loop ;
: TIME-ARITH   ( -- ) RUNS 0 ?do mono-ns B-ARITH   mono-ns swap - RUN-NS i cells + ! loop ;
: TIME-BRANCH  ( -- ) RUNS 0 ?do mono-ns B-BRANCH  mono-ns swap - RUN-NS i cells + ! loop ;
: TIME-SEARCH  ( -- ) RUNS 0 ?do mono-ns B-SEARCH  mono-ns swap - RUN-NS i cells + ! loop ;
: TIME-FOLD    ( -- ) RUNS 0 ?do mono-ns B-FOLD    mono-ns swap - RUN-NS i cells + ! loop ;
: TIME-MOVE    ( -- ) RUNS 0 ?do mono-ns B-MOVE    mono-ns swap - RUN-NS i cells + ! loop ;
: TIME-LINES   ( -- ) RUNS 0 ?do mono-ns B-LINES   mono-ns swap - RUN-NS i cells + ! loop ;

public

: MAIN ( -- )
   1 script-argv$ LOAD-SOURCE
   FILL-CELLS
   TIME-HARNESS s" harness" REPORT
   TIME-ARITH   s" arith"   REPORT
   TIME-BRANCH  s" branch"  REPORT
   TIME-SEARCH  s" search"  REPORT
   TIME-FOLD    s" fold"    REPORT
   TIME-MOVE    s" move"    REPORT
   TIME-LINES   s" lines"   REPORT ;

;package

TIER-BENCH:MAIN
