\ json-read-perf-test.f - the JSON reader's six wall-clock ratchet workloads.
\
\ These workloads used to live inside lib/json-read-test.f, whose gate entry runs
\ beside other test files; a wall-clock ratchet measured under that contention
\ reports the contention, not the parser. This package owns them instead, split
\ across two public words. MEASURE runs the warm-up correctness probes and then
\ stores every raw sample - SAMPLE-N per slot, taken in interleaved rounds: each
\ round times every workload once, so a load burst lands across the six
\ workloads instead of inside one workload's whole sample set (the paired
\ discipline of dot habu-pair-and-alternate-60b04c6a). REPORT turns the stored
\ samples into one evidence line per workload and the six pass/fail verdicts -
\ each judged on the FASTEST of its samples, because a run can only be made
\ slower by the rest of the machine, never faster: the fastest run is the
\ closest estimate of the real cost (tools/codegen-compare-core.f's standing
\ discipline), a real regression slows every sample including it, and ambient
\ load has to poison every window of every round to move it. The median it
\ replaces moved whenever two of three consecutive samples were dirty, which is
\ exactly the flake the gate recorded. A caller that wants meaningful numbers
\ runs MEASURE while nothing else is running and calls REPORT afterwards.
\ REPORT fails every verdict closed until the whole sample set exists, so a
\ skipped or half-finished MEASURE can never report a pass.
\
\ WHAT A VERDICT COMPARES. Not nanoseconds. A seventh slot in every round times
\ a FROZEN REFERENCE workload that calls no code under test, and each workload
\ is judged on its fastest divided by the reference's fastest, against a
\ recorded ratio. A nanosecond budget is a claim about the box that recorded it,
\ and it needed a calibration factor to survive meeting any other box - a factor
\ measured by a register spin that, being bound by a different resource than
\ these memory-and-dispatch workloads, moved the wrong way as often as the right
\ one. A ratio needs no such factor: the machine appears in both terms and
\ divides out. See the reference's own header below for why it is shaped the way
\ it is, and the recorded ratios for what is and is not known about their
\ provenance.
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
6 constant WORK-N                     \ judged ratchet workloads
6 constant REF-ID                     \ the frozen reference: the denominator, never judged
WORK-N 1+ constant SLOT-N             \ timed slots per round: the workloads and the reference
5 constant SAMPLE-N                   \ timed rounds per slot
SLOT-N SAMPLE-N * constant SAMPLE-TOTAL
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

: SAMPLE-A ( n n -- ptr n ) {: work:n slot:n :}      \ slot, run -> its sample cell
   work 0 < work SLOT-N >= or if E-JRP-RANGE throw then
   slot 0 < slot SAMPLE-N >= or if E-JRP-RANGE throw then
   SAMPLES work SAMPLE-N * slot + cells + ;

: SAMPLE@ ( n n -- n )
   SAMPLE-A @ ;

\ Samples are appended, never addressed: the store index is the count so far, so
\ a dropped or repeated run cannot leave a plausible-looking table behind. The
\ slot argument pins the append order too - ROUND-MAJOR, one sample of every
\ slot per round, the reference last: a run stored under the wrong slot, or out
\ of round order, throws instead of landing in another slot's row. The reference
\ is bound into that order by the same rule, so a round that timed the workloads
\ and skipped the reference cannot complete a table.
: SAMPLE+ ( n n -- ) {: value:n work:n :}
   TAKEN @ {: idx:n :}
   idx SAMPLE-TOTAL >= if E-JRP-SAMPLE throw then
   idx SLOT-N mod work <> if E-JRP-SAMPLE throw then
   value work idx SLOT-N / SAMPLE-A !
   idx 1+ TAKEN ! ;

: COMPLETE? ( -- bool )
   TAKEN @ SAMPLE-TOTAL = ;

\ ---- how every slot is timed ----------------------------------------------
\ SYMMETRY IS THE RULE HERE, and it was learned the hard way. A verdict divides
\ one measurement by another, so the two must be equally exposed to whatever the
\ rest of the machine is doing. Time the reference as the fastest of ten short
\ sub-runs and the workload as one long run, and the denominator finds a quiet
\ gap while the numerator wears every interruption - the ratio then climbs on a
\ busy box for no reason but the asymmetry. Measured: judged that way, a healthy
\ tree went red on five of six rows under load. It is not enough for the
\ reference to be well measured; it has to be measured THE SAME WAY.
\
\ So every slot - all six workloads and the reference - is the fastest of
\ SLOT-CHUNKS sub-runs, each sub-run a fixed fraction of that slot's work. Same total
\ work as one long run, same estimator for every term, and a load burst has to
\ ruin every sub-run of a slot to move it.
40 constant SLOT-CHUNKS               \ sub-runs per sample, every slot alike

: MIN2 ( n n -- n )
   2dup > if swap then drop ;

\ One sub-run, wall ns.
: CHUNK-ONCE ( [ -- ] -- n )
   {: body :} \ typed-local-lint: allow-bare-local - body carries a quotation effect
   mono-ns {: t0:n :}
   body execute
   mono-ns t0 - ;

\ The fastest of SLOT-CHUNKS sub-runs.
: TIME-SLOT ( [ -- ] -- n )
   {: body :} \ typed-local-lint: allow-bare-local - body carries a quotation effect
   body CHUNK-ONCE
   SLOT-CHUNKS 1 ?do body CHUNK-ONCE MIN2 loop ;

\ ---- workload sizes -------------------------------------------------------
$100 constant CAP                     \ decoded-string scratch buffer
20000 constant SMALL-N                \ small documents per small-document run
SMALL-N SLOT-CHUNKS / constant SMALL-CHUNK-N
13 constant SMALL-TOKENS              \ tokens in one small document, end token included
10000 constant LONG-N                 \ values in the one long array
LONG-N 2 * 1+ constant LONG-U         \ '[' + values + separating commas + ']'
6000 constant STR-N                   \ string decodes per string run
STR-N SLOT-CHUNKS / constant STR-CHUNK-N
192 constant STR-LEN                  \ decoded bytes per string
STR-LEN 2 + constant RAW-U            \ quoted string of plain bytes
STR-LEN 2 * 2 + constant ESC-U        \ quoted string of two-byte escapes
1000 constant FIND-N                  \ key searches per search run
FIND-N SLOT-CHUNKS / constant FIND-CHUNK-N
24 constant KEY-N                     \ members in the searched object
32 constant KEY-LEN                   \ bytes per member key
5 constant MEMBER-OVERHEAD            \ opening/closing quote, colon, value, comma
KEY-LEN MEMBER-OVERHEAD + constant MEMBER-U
KEY-N MEMBER-U * 1+ constant FIND-U   \ '{' + members, the last comma replaced by '}'
$41 constant FIRST-SUFFIX             \ 'A', the last key byte of the first member
FIRST-SUFFIX KEY-N 1- + constant HIT-SUFFIX    \ last member: the search that hits
HIT-SUFFIX 1+ constant MISS-SUFFIX             \ absent key: the search that misses

\ ---- the frozen reference workload ----------------------------------------
\ The denominator every verdict is judged against. It has to move with the
\ machine the way the workloads do, so it is the same KIND of work - a
\ byte-at-a-time load, compare and store over a small resident buffer, which is
\ what every decode and key-search loop above spends its time on - and it has to
\ be independent of the thing being measured, so it calls NO code under test and
\ touches no JSON buffer. That second half is the whole point: a reference that
\ ran the reader would grow exactly when the reader regressed and the ratchet
\ would go blind at the only moment it matters.
\
\ Its constants are its own, deliberately not shared with the workload sizes
\ above. FROZEN means frozen: if this loop's work ever changes, every recorded
\ ratio below silently means something else, so a future edit has to re-record
\ them rather than inherit them. Duplicating a few byte values is the price of
\ making that impossible to do by accident.
\
\ WHY THIS SHAPE, AND NOT A PLAIN SCAN. It was MEASURED, on this box, against
\ the alternatives. A flat byte scan over a resident buffer - the obvious
\ reference, and the first one tried here - made the verdicts WORSE than no
\ reference at all on four of six rows, because this host has eight performance
\ and four efficiency cores and each loop shape has its OWN slowdown between
\ them. Divide a workload by a reference that answers core placement
\ differently and the placement does not cancel, it compounds. That is the same
\ defect that disqualified the register-only calibration spin, in a new costume.
\
\ So the reference copies the SILHOUETTE of the work it normalises: an outer
\ per-item call, a short state set-up, a byte-at-a-time classify-and-copy over a
\ string-sized body, and a teardown - the READER/NEXT/STR/CLOSE sequence the
\ string workloads run, built from plain memory words and calling none of it.
\ Measured against a flat scan across twenty runs, the shaped reference roughly
\ halved the spread of five of the six rows.
\ It is timed by the shared TIME-SLOT above, exactly as the six workloads are.
\ That is not tidiness, it is the correctness condition: see the symmetry note
\ there for what a differently-measured denominator did to a healthy tree.
192 constant REF-LEN                  \ string body per item, the decode workloads' own size
60000 constant REF-ITERS              \ items per sample, the workloads' own range
REF-ITERS SLOT-CHUNKS / constant REF-CHUNK-ITERS
8 constant REF-STATE-N                \ state cells one item sets up and tears down
$22 constant REF-QUOTE                \ the byte that ends a string
$5C constant REF-ESC                  \ the byte that starts an escape
$6B constant REF-FILL                 \ the ordinary body byte
0 constant REF-QUOTES-WANT            \ quotes the body contains
0 constant REF-ESCAPES-WANT           \ escapes the body contains
1 constant REF-CLOSES-WANT            \ teardowns one item performs

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

\ ---- recorded ratios -------------------------------------------------------
100 constant PCT-DEN                  \ percent denominator
10000 constant RATIO-SCALE            \ ratios are recorded in parts per ten thousand

\ WHAT THESE NUMBERS ARE. Each row is a workload's fastest-of-rounds divided by
\ the reference's fastest-of-rounds in the same measurement, x RATIO-SCALE. They
\ are not times and they are not comparable to the wall-clock baselines they
\ replace: a ratio says how much of this machine's byte-moving capacity the
\ workload costs, which is a fact about the tree, where a nanosecond count was a
\ fact about the box that happened to run it.
\
\ PROVENANCE, RECORDED AS FACT. The wall-clock baselines these replace named
\ parents 83fae24d6628 and aa2a169469ad. NEITHER COMMIT RESOLVES IN THIS REPO
\ ANY MORE, so there is no tree to re-measure them on and no way to compare this
\ tree against the one they describe. THE REGRESSION QUESTION AGAINST THE OLD
\ NUMBERS IS THEREFORE UNANSWERABLE, and nothing below should be read as having
\ answered it. What could be checked on the tree that does exist was checked and
\ is written down beside the basis below; continuity to the old numbers is
\ broken, and the first honest ratchet starts here.
\
\ BASIS: see the recorded measurement note in docs/gate.md
\ (json-read ratchet ratios). Re-record only with a fresh basis written the
\ same way - box, date, rounds, observed noise band - never to unblock a merge.
\ RECORDED 2026-08-07, macOS arm64, 8 performance + 4 efficiency cores. Each row
\ is the MEDIAN of 8 healthy measurements taken with the host already carrying a
\ foreign build on half its cores (one-minute load average about 12, calibration
\ spin 158-170 ms against a committed reference of 95). No quiet window was
\ sought: a ratio is supposed to be indifferent to machine state, so the basis
\ exercises that rather than avoiding it.
\
\ MEASURED NOISE, which is what sizes the headroom, at two load levels:
\   - at load ~12 the full spread of every row was 0.4% to 0.6%;
\   - under twelve additional busy loops (load ~32) the worst single observation
\     of any row sat 8.1% above its recorded median (escape-heavy).
\ The tail under load, not the quiet spread, is what the headroom has to cover.
\ HEADROOM-PCT is bounded on both sides by measurement, not chosen for comfort:
\   - it must exceed ~108, or the loaded tail alone reds a row;
\   - it must stay under ~115, or the smallest regression actually demonstrated
\     against this gate stops failing (escape-heavy at +15.5%; raw string +27%,
\     both key searches +22%).
\ 112 sits between them with about four points of margin each way, and both
\ bounds were falsified by experiment: an injected slowdown in the decode loop
\ red the four decode rows 5 times out of 5 while leaving the two numeric rows
\ untouched, and a healthy tree under twelve extra busy loops red nothing, 0 out
\ of 6, on all six rows.
\
\ WHY 0.5% NOISE AND NOT 8%: see SLOT-CHUNKS above. Timing each slot as one long
\ run gave a 7.8-12.8% spread and red healthy trees inside the real gate whenever
\ ambient load rose; cutting every slot into forty short sub-runs and keeping the
\ fastest collapsed that to well under one percent, because a short window has a
\ much better chance of landing between interruptions.
\
\ WHAT THIS GATE DOES NOT CATCH, stated plainly because the previous numbers
\ pretended otherwise: the class reliably separated here starts near fifteen
\ percent, set by the loaded tail rather than by the quiet resolution. On a
\ quiet host the same rows resolve to better than one percent, so a tighter
\ headroom is available to anyone willing to record the basis for one.
6346 constant SMALL-RATIO
11005 constant LONG-RATIO
11138 constant RAW-RATIO
19289 constant ESC-RATIO
8958 constant HIT-RATIO
8973 constant MISS-RATIO
112 constant HEADROOM-PCT

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
create REF-SRC REF-LEN 2 + allot
create REF-DST REF-LEN allot
create REF-STATE REF-STATE-N cells allot

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
   [: SMALL-CHUNK-N 0 ?do SMALL-ONE loop ;] TIME-SLOT ;

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

: LONG-RUN ( -- n )                   \ one parse is already a sub-run's worth of work
   BUILD-LONG
   [: LONG-ONE ;] TIME-SLOT ;

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
   [: STR-CHUNK-N 0 ?do RAW-ONE loop ;] TIME-SLOT ;

: ESC-RUN ( -- n )
   BUILD-ESC
   [: STR-CHUNK-N 0 ?do ESC-ONE loop ;] TIME-SLOT ;

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
   [: FIND-CHUNK-N 0 ?do HIT-KEY FIND-ONE loop ;] TIME-SLOT ;

: MISS-RUN ( -- n )
   BUILD-FIND
   [: FIND-CHUNK-N 0 ?do MISS-KEY FIND-ONE loop ;] TIME-SLOT ;

\ ---- the reference run ----------------------------------------------------
\ A quoted body of ordinary bytes: the raw-string workload's own input shape.
: BUILD-REF ( -- )
   REF-QUOTE REF-SRC c!
   REF-LEN 0 ?do REF-FILL REF-SRC 1+ i + c! loop
   REF-QUOTE REF-SRC REF-LEN 1+ + c! ;

: REF-INIT ( -- )                     \ the state one item sets up
   REF-STATE-N 0 ?do 0 REF-STATE i cells + ! loop ;

\ The classify-and-copy pass. Every branch a decode loop takes is present -
\ end-of-string, escape, ordinary byte - and the counters and the destination
\ store are real memory effects, so nothing here can be folded away.
: REF-SCAN ( -- )
   REF-LEN 0 ?do
      REF-SRC 1+ i + c@ {: b:n :}
      b REF-QUOTE = if REF-STATE @ 1+ REF-STATE ! else
      b REF-ESC = if REF-STATE cell + @ 1+ REF-STATE cell + ! else
         b REF-DST i + c!
      then then
   loop ;

: REF-CLOSE ( -- )
   REF-STATE 2 cells + @ 1+ REF-STATE 2 cells + ! ;

: REF-ONE ( -- )
   REF-INIT REF-SCAN REF-CLOSE ;

: REF-RUN ( -- n )
   BUILD-REF
   [: REF-CHUNK-ITERS 0 ?do REF-ONE loop ;] TIME-SLOT ;

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

\ The reference has no parser to be right about, so what has to be checked is
\ that it DID THE WORK: one item leaves the counters it should and the whole
\ body copied. A reference that got optimised, shortened or skipped away would
\ otherwise surface only as every ratio quietly rising, with no verdict able to
\ say why - the reference is the denominator, so silence there corrupts all six
\ rows at once.
: CHECK-REF ( -- )
   BUILD-REF
   REF-ONE
   REF-STATE @ REF-QUOTES-WANT T=
   REF-STATE cell + @ REF-ESCAPES-WANT T=
   REF-STATE 2 cells + @ REF-CLOSES-WANT T=
   REF-DST c@ REF-FILL T=
   REF-DST REF-LEN 1- + c@ REF-FILL T= ;

: WARM-UP ( -- )
   BUILD-RAW
   RAW-SRC RAW-U K-BYTE CHECK-STR
   BUILD-ESC
   ESC-SRC ESC-U LF CHECK-STR
   BUILD-FIND
   HIT-KEY true CHECK-FIND
   MISS-KEY false CHECK-FIND
   CHECK-REF ;

\ ---- taking the samples ---------------------------------------------------
\ One round: every slot timed once, in the fixed order the store enforces, the
\ reference among them. Rounds repeat SAMPLE-N times, so consecutive samples of
\ ONE slot are separated by a whole round of the other six - a load burst
\ shorter than a round can dirty at most one sample of each slot, and the
\ fastest-of-round judging above it needs every round dirty to move.
\
\ Timing the reference HERE, inside the round, is what makes the ratio mean
\ anything: it meets the same machine, the same core and the same neighbours as
\ the workloads it is dividing, within one round of each of them. A reference
\ measured once before or after the rounds would be a different measurement of a
\ different moment, which is the mistake the spin probe it replaces made.
: TAKE-ROUND ( -- )
   SMALL-RUN SMALL-ID SAMPLE+
   LONG-RUN LONG-ID SAMPLE+
   RAW-RUN RAW-ID SAMPLE+
   ESC-RUN ESC-ID SAMPLE+
   HIT-RUN HIT-ID SAMPLE+
   MISS-RUN MISS-ID SAMPLE+
   REF-RUN REF-ID SAMPLE+ ;

public

\ Warm up on the production path, then time every workload once per round,
\ SAMPLE-N rounds. Every sample is kept; nothing here judges a number.
: MEASURE ( -- )
   SAMPLES-CLEAR
   WARM-UP
   SAMPLE-N 0 ?do TAKE-ROUND loop ;

private

\ ---- budgets and verdicts -------------------------------------------------
\ The judged statistic: the workload's fastest sample. A regression in the
\ parser slows every sample including the fastest; host load slows only the
\ windows it lands in, and the rounds interleave the workloads so it has to
\ land in all of them to move this.
: FASTEST ( n -- n ) {: work:n :}
   work 0 SAMPLE@
   SAMPLE-N 1 ?do work i SAMPLE@ MIN2 loop ;

\ One table for the whole report: a workload's name, the ratio it was recorded
\ at, and the headroom that ratio carries. Nothing else selects on the workload,
\ so no verdict can pair one workload's name with another's ceiling.
: ROW ( n -- ptr u8 n n n )
   case
      SMALL-ID of s" 20,000 small documents" SMALL-RATIO HEADROOM-PCT endof
      LONG-ID of s" one 10,000-value stream" LONG-RATIO HEADROOM-PCT endof
      RAW-ID of s" repeated raw string decode" RAW-RATIO HEADROOM-PCT endof
      ESC-ID of s" repeated escape-heavy decode" ESC-RATIO HEADROOM-PCT endof
      HIT-ID of s" repeated object key-search hits" HIT-RATIO HEADROOM-PCT endof
      MISS-ID of s" repeated object key-search misses" MISS-RATIO HEADROOM-PCT endof
      E-JRP-RANGE throw
   endcase ;

\ The reference's own fastest round: the denominator of every verdict.
: REF-FASTEST ( -- n )
   REF-ID FASTEST ;

\ A workload's cost expressed in units of the reference, RATIO-SCALE to one.
\ Both terms are a fastest-of-rounds, so both are the cleanest estimate of their
\ own real cost, and the machine speed they share divides out. Zero while the
\ reference has no sample yet: PASS? refuses an incomplete table before it ever
\ reads this, so that zero can never be mistaken for an infinitely fast row.
: RATIO ( n -- n ) {: work:n :}
   REF-FASTEST {: ref:n :}
   ref 0= if 0 exit then
   work FASTEST RATIO-SCALE * ref / ;

\ No calibration factor appears here, and that is the point of the row. A
\ wall-clock budget needed one because it was a number about this box; a ratio
\ is a number about the tree, and multiplying it by how fast the box happens to
\ be would put the box back into the answer it was removed from.
: BUDGET ( n n -- n ) {: base:n pct:n :}   \ recorded ratio, headroom -> judged ceiling
   base pct * PCT-DEN / ;

: PASS? ( n n -- bool ) {: work:n budget:n :}
   COMPLETE? 0= if false exit then    \ an incomplete sample set never passes
   work RATIO budget <= ;

\ ---- evidence line --------------------------------------------------------
: SB-TF ( bool -- )
   if s" true" else s" false" then SB-APPEND ;

: LINE-HEAD ( ptr u8 n -- )
   SB-RESET
   s" json-read-perf: " SB-APPEND
   SB-APPEND ;

: LINE-SAMPLES ( n -- ) {: work:n :}
   s"  samples=" SB-APPEND work 0 SAMPLE@ FMT:SB-U
   SAMPLE-N 1 ?do
      s" ," SB-APPEND work i SAMPLE@ FMT:SB-U
   loop ;

\ Both terms of the ratio are printed beside it, so a row that moved can be read
\ back as either the workload slowing or the reference speeding up without
\ re-running anything. The raw times stay too: they are no longer the verdict,
\ but they are still the evidence a future re-record needs.
: LINE-TAIL ( n n bool -- ) {: work:n budget:n pass:bool :}
   s"  fastest=" SB-APPEND work FASTEST FMT:SB-U
   s"  ref=" SB-APPEND REF-FASTEST FMT:SB-U
   s"  ratio=" SB-APPEND work RATIO FMT:SB-U
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
