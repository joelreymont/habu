\ json-read-perf-test.f - the JSON reader's six wall-clock ratchet workloads.
\
\ These workloads used to live inside lib/json-read-test.f, whose gate entry runs
\ beside other test files; a wall-clock ratchet measured under that contention
\ reports the contention, not the parser. This package owns them instead, split
\ across two public words. MEASURE runs the warm-up correctness probes and then
\ stores every raw sample - three per workload, eighteen in all. REPORT turns the
\ stored samples into calibrated budgets, one evidence line per workload, and the
\ six pass/fail verdicts. A caller that wants meaningful numbers runs MEASURE
\ while nothing else is running and calls REPORT afterwards. REPORT fails every
\ verdict closed until the whole sample set exists, so a skipped or half-finished
\ MEASURE can never report a pass.
\
\ Run: bin/hb --load lib/json-read-perf-test.f lib/json-read-perf-contract-test.f

require lib/prelude.f
require lib/errors.f
require lib/string.f
require lib/fmt.f
require lib/test/assert.f
require lib/test/budget.f
require lib/json-read.f

package JSON-READ-PERF-TEST
private

\ ---- sample table ---------------------------------------------------------
6 constant WORK-N                     \ ratchet workloads
3 constant SAMPLE-N                   \ timed runs per workload
WORK-N SAMPLE-N * constant SAMPLE-TOTAL
0 constant SMALL-ID
1 constant LONG-ID
2 constant RAW-ID
3 constant ESC-ID
4 constant HIT-ID
5 constant MISS-ID

create SAMPLES SAMPLE-TOTAL cells allot
variable TAKEN   0 TAKEN !            \ raw samples stored by the current MEASURE

: SAMPLES-CLEAR ( -- )
   SAMPLE-TOTAL 0 ?do 0 SAMPLES i cells + ! loop
   0 TAKEN ! ;

: SAMPLE-A ( n n -- ptr n ) {: work:n slot:n :}      \ workload, run -> its sample cell
   work 0 < work WORK-N >= or if E-JRP-RANGE throw then
   slot 0 < slot SAMPLE-N >= or if E-JRP-RANGE throw then
   SAMPLES work SAMPLE-N * slot + cells + ;

: SAMPLE@ ( n n -- n )
   SAMPLE-A @ ;

\ Samples are appended, never addressed: the store index is the count so far, so
\ a dropped or repeated run cannot leave a plausible-looking table behind. The
\ workload argument pins the append order too: a run stored under the wrong
\ workload would land in another workload's row, so it throws instead.
: SAMPLE+ ( n n -- ) {: value:n work:n :}
   TAKEN @ {: idx:n :}
   idx SAMPLE-TOTAL >= if E-JRP-SAMPLE throw then
   idx SAMPLE-N / work <> if E-JRP-SAMPLE throw then
   value SAMPLES idx cells + !
   idx 1+ TAKEN ! ;

: COMPLETE? ( -- bool )
   TAKEN @ SAMPLE-TOTAL = ;

\ ---- workload sizes -------------------------------------------------------
$100 constant CAP                     \ decoded-string scratch buffer
20000 constant SMALL-N                \ small documents per small-document run
13 constant SMALL-TOKENS              \ tokens in one small document, end token included
10000 constant LONG-N                 \ values in the one long array
LONG-N 2 * 1+ constant LONG-U         \ '[' + values + separating commas + ']'
6000 constant STR-N                   \ string decodes per string run
192 constant STR-LEN                  \ decoded bytes per string
STR-LEN 2 + constant RAW-U            \ quoted string of plain bytes
STR-LEN 2 * 2 + constant ESC-U        \ quoted string of two-byte escapes
1000 constant FIND-N                  \ key searches per search run
24 constant KEY-N                     \ members in the searched object
32 constant KEY-LEN                   \ bytes per member key
5 constant MEMBER-OVERHEAD            \ opening/closing quote, colon, value, comma
KEY-LEN MEMBER-OVERHEAD + constant MEMBER-U
KEY-N MEMBER-U * 1+ constant FIND-U   \ '{' + members, the last comma replaced by '}'
$41 constant FIRST-SUFFIX             \ 'A', the last key byte of the first member
FIRST-SUFFIX KEY-N 1- + constant HIT-SUFFIX    \ last member: the search that hits
HIT-SUFFIX 1+ constant MISS-SUFFIX             \ absent key: the search that misses

\ ---- JSON bytes -----------------------------------------------------------
$0A constant LF                       \ the byte "\n" decodes to
$22 constant DQ
$2C constant COMMA
$30 constant ZERO
$3A constant COLON
$5B constant LBRACK
$5C constant BACKSLASH
$5D constant RBRACK
$6B constant K-BYTE                   \ 'k', the filler byte of keys and plain strings
$6E constant N-BYTE                   \ 'n', the escape letter of "\n"
$7B constant LBRACE
$7D constant RBRACE

\ ---- recorded baselines ---------------------------------------------------
\ Numeric scan baselines: five-run medians on parent aa2a169469ad.
67793408 constant SMALL-BASE
2984112 constant LONG-BASE
\ Production-path baselines: medians of five independent three-run medians on
\ parent 83fae24d6628.
109841024 constant RAW-BASE
189988832 constant ESC-BASE
89995280 constant HIT-BASE
90105840 constant MISS-BASE
100 constant PCT-DEN                  \ percent denominator
100 constant EXACT-PCT                \ scan baselines carry no extra headroom
110 constant HEADROOM-PCT             \ ten percent over the production-path medians:
                                      \ wider than the observed timing noise, still
                                      \ narrow enough to reject a real regression

\ ---- workload storage -----------------------------------------------------
create BUF CAP allot
here CELL 1- and CELL swap - CELL 1- and allot   \ reader storage must be cell aligned
create STATE JR:STORAGE-BYTES allot
create LONG-SRC LONG-U allot
create RAW-SRC RAW-U allot
create ESC-SRC ESC-U allot
create FIND-SRC FIND-U allot
create HIT-KEY KEY-LEN allot
create MISS-KEY KEY-LEN allot

: READER ( ptr u8 n -- JR:reader )
   STATE JR:STORAGE-BYTES 2swap JR:INIT ;

: DRAIN ( JR:reader n -- JR:reader ) {: count:n :}
   count 0 ?do JR:NEXT drop loop ;

\ ---- workload: many small documents ---------------------------------------
: DOC$ ( -- ptr u8 n )
   s" [1,2,3,4,5,6,7,8,9,10]" ;

: SMALL-ONE ( -- )
   DOC$ READER
   SMALL-TOKENS DRAIN
   JR:CLOSE ;

: SMALL-RUN ( -- n )
   mono-ns {: start:n :}
   SMALL-N 0 ?do SMALL-ONE loop
   mono-ns start - ;

\ ---- workload: one long value stream --------------------------------------
: BUILD-LONG ( -- )
   LBRACK LONG-SRC c!
   LONG-N 0 ?do
      ZERO LONG-SRC 1 i 2 * + + c!
      i LONG-N 1- < if
         COMMA LONG-SRC 2 i 2 * + + c!
      then
   loop
   RBRACK LONG-SRC LONG-U 1- + c! ;

: LONG-ONE ( -- )
   LONG-SRC LONG-U READER
   LONG-N 3 + DRAIN
   JR:CLOSE ;

: LONG-RUN ( -- n )
   BUILD-LONG
   mono-ns {: start:n :}
   LONG-ONE
   mono-ns start - ;

\ ---- workloads: repeated string decode ------------------------------------
: BUILD-RAW ( -- )
   DQ RAW-SRC c!
   STR-LEN 0 ?do K-BYTE RAW-SRC 1+ i + c! loop
   DQ RAW-SRC RAW-U 1- + c! ;

: BUILD-ESC ( -- )
   DQ ESC-SRC c!
   STR-LEN 0 ?do
      BACKSLASH ESC-SRC 1 i 2 * + + c!
      N-BYTE ESC-SRC 2 i 2 * + + c!
   loop
   DQ ESC-SRC ESC-U 1- + c! ;

: STR-ONE ( ptr u8 n -- )
   READER JR:NEXT drop BUF CAP JR:STR drop JR:CLOSE ;

: RAW-ONE ( -- )
   RAW-SRC RAW-U STR-ONE ;

: ESC-ONE ( -- )
   ESC-SRC ESC-U STR-ONE ;

: RAW-RUN ( -- n )
   BUILD-RAW
   mono-ns {: start:n :}
   STR-N 0 ?do RAW-ONE loop
   mono-ns start - ;

: ESC-RUN ( -- n )
   BUILD-ESC
   mono-ns {: start:n :}
   STR-N 0 ?do ESC-ONE loop
   mono-ns start - ;

\ ---- workloads: repeated object key search --------------------------------
: KEY! ( ptr u8 n -- ) {: key:ptr suffix:n :}
   KEY-LEN 1- 0 ?do K-BYTE key i + c! loop
   suffix key KEY-LEN 1- + c! ;

: BUILD-MEMBER ( n -- ) {: idx:n :}
   idx MEMBER-U * 1+ {: off:n :}
   DQ FIND-SRC off + c!
   KEY-LEN 1- 0 ?do
      K-BYTE FIND-SRC off 1+ + i + c!
   loop
   FIRST-SUFFIX idx +
   FIND-SRC off KEY-LEN + + c!
   DQ FIND-SRC off KEY-LEN 1+ + + c!
   COLON FIND-SRC off KEY-LEN 2 + + + c!
   ZERO FIND-SRC off KEY-LEN 3 + + + c!
   COMMA FIND-SRC off KEY-LEN 4 + + + c! ;

: BUILD-FIND ( -- )
   LBRACE FIND-SRC c!
   KEY-N 0 ?do i BUILD-MEMBER loop
   RBRACE FIND-SRC FIND-U 1- + c!
   HIT-KEY HIT-SUFFIX KEY!
   MISS-KEY MISS-SUFFIX KEY! ;

: FIND-ONE ( ptr u8 -- ) {: key:ptr :}
   FIND-SRC FIND-U READER
   JR:NEXT drop key KEY-LEN JR:FIND-KEY drop JR:CLOSE ;

: HIT-RUN ( -- n )
   BUILD-FIND
   mono-ns {: start:n :}
   FIND-N 0 ?do HIT-KEY FIND-ONE loop
   mono-ns start - ;

: MISS-RUN ( -- n )
   BUILD-FIND
   mono-ns {: start:n :}
   FIND-N 0 ?do MISS-KEY FIND-ONE loop
   mono-ns start - ;

\ ---- warm-up: the correctness probes the baselines were recorded behind ----
: CHECK-STR ( ptr u8 n n -- ) {: want:n :}
   READER
   JR:NEXT JR:T-STR T=
   BUF CAP JR:STR STR-LEN T=
   STR-LEN 0 ?do BUF i + c@ want T= loop
   JR:CLOSE ;

: CHECK-FIND ( ptr u8 bool -- ) {: key:ptr want:bool :}
   FIND-SRC FIND-U READER
   JR:NEXT JR:T-OBJ T=
   key KEY-LEN JR:FIND-KEY
   want if TTRUE else TFALSE then
   JR:CLOSE ;

: WARM-UP ( -- )
   BUILD-RAW
   RAW-SRC RAW-U K-BYTE CHECK-STR
   BUILD-ESC
   ESC-SRC ESC-U LF CHECK-STR
   BUILD-FIND
   HIT-KEY true CHECK-FIND
   MISS-KEY false CHECK-FIND ;

\ ---- taking the samples ---------------------------------------------------
: TAKE-SMALL ( -- )
   SAMPLE-N 0 ?do SMALL-RUN SMALL-ID SAMPLE+ loop ;

: TAKE-LONG ( -- )
   SAMPLE-N 0 ?do LONG-RUN LONG-ID SAMPLE+ loop ;

: TAKE-RAW ( -- )
   SAMPLE-N 0 ?do RAW-RUN RAW-ID SAMPLE+ loop ;

: TAKE-ESC ( -- )
   SAMPLE-N 0 ?do ESC-RUN ESC-ID SAMPLE+ loop ;

: TAKE-HIT ( -- )
   SAMPLE-N 0 ?do HIT-RUN HIT-ID SAMPLE+ loop ;

: TAKE-MISS ( -- )
   SAMPLE-N 0 ?do MISS-RUN MISS-ID SAMPLE+ loop ;

public

\ Warm up on the production path, then time every workload three times. Every
\ sample is kept; nothing here judges a number.
: MEASURE ( -- )
   SAMPLES-CLEAR
   WARM-UP
   TAKE-SMALL
   TAKE-LONG
   TAKE-RAW
   TAKE-ESC
   TAKE-HIT
   TAKE-MISS ;

private

\ ---- budgets and verdicts -------------------------------------------------
: MIN2 ( n n -- n )
   2dup > if swap then drop ;

: MAX2 ( n n -- n )
   2dup < if swap then drop ;

: MEDIAN3 ( n n n -- n ) {: a:n b:n c:n :}
   a b + c +
   a b MIN2 c MIN2 -
   a b MAX2 c MAX2 - ;

: MEDIAN ( n -- n ) {: work:n :}
   work 0 SAMPLE@ work 1 SAMPLE@ work 2 SAMPLE@ MEDIAN3 ;

\ One table for the whole report: a workload's name, the baseline it was
\ recorded at, and the headroom that baseline carries. Nothing else selects on
\ the workload, so no verdict can pair one workload's name with another's budget.
: ROW ( n -- ptr u8 n n n )
   case
      SMALL-ID of s" 20,000 small documents" SMALL-BASE EXACT-PCT endof
      LONG-ID of s" one 10,000-value stream" LONG-BASE EXACT-PCT endof
      RAW-ID of s" repeated raw string decode" RAW-BASE HEADROOM-PCT endof
      ESC-ID of s" repeated escape-heavy decode" ESC-BASE HEADROOM-PCT endof
      HIT-ID of s" repeated object key-search hits" HIT-BASE HEADROOM-PCT endof
      MISS-ID of s" repeated object key-search misses" MISS-BASE HEADROOM-PCT endof
      E-JRP-RANGE throw
   endcase ;

: CAL-PCT ( -- n )                    \ this host's calibration factor, percent
   PCT-DEN TEST-BUDGET:PERF-MS ;

: BUDGET ( n n -- n ) {: base:n pct:n :}   \ baseline, headroom -> calibrated budget
   base pct * PCT-DEN /
   CAL-PCT * PCT-DEN / ;

: PASS? ( n n -- bool ) {: work:n budget:n :}
   COMPLETE? 0= if false exit then    \ an incomplete sample set never passes
   work MEDIAN budget <= ;

\ ---- evidence line --------------------------------------------------------
: SB-TF ( bool -- )
   if s" true" else s" false" then SB-APPEND ;

: LINE-HEAD ( ptr u8 n -- )
   SB-RESET
   s" json-read-perf: " SB-APPEND
   SB-APPEND ;

: LINE-SAMPLES ( n -- ) {: work:n :}
   s"  samples=" SB-APPEND work 0 SAMPLE@ FMT:SB-U
   s" ," SB-APPEND work 1 SAMPLE@ FMT:SB-U
   s" ," SB-APPEND work 2 SAMPLE@ FMT:SB-U ;

: LINE-TAIL ( n n bool -- ) {: work:n budget:n pass:bool :}
   s"  median=" SB-APPEND work MEDIAN FMT:SB-U
   s"  budget=" SB-APPEND budget FMT:SB-U
   s"  stored=" SB-APPEND TAKEN @ FMT:SB-U
   s"  pass=" SB-APPEND pass SB-TF ;

\ Named LINE, not EMIT: a package-private EMIT would hide the engine's `emit`
\ from every later definition in this package.
: LINE ( n n bool -- ) {: work:n budget:n pass:bool :}
   T-LABEL$ LINE-HEAD
   work LINE-SAMPLES
   work budget pass LINE-TAIL
   SB$ type cr ;

: JUDGE ( n -- ) {: work:n :}         \ one workload: its evidence line, then its verdict
   work ROW {: base:n pct:n :}        \ the workload name stays on the stack for T-LABEL
   T-LABEL
   base pct BUDGET {: budget:n :}
   work budget PASS? {: pass:bool :}
   work budget pass LINE
   pass TTRUE ;

public

\ Judge every workload against its own budget, printing one evidence line each.
: REPORT ( -- )
   WORK-N 0 ?do i JUDGE loop ;

;package
