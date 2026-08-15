\ schedule-lint.f - every SUITE registration must be reachable by a runner, every
\ phase that runner has must be started, and every test source on disk must be
\ reached by one of them.
\
\ THE HOLE THIS CLOSES. test/gate-stdlib-cases.f registers a suite by writing
\ `SUITE <label> <files...> ;SUITE`. Writing it there does not run it. A
\ registration only runs when a slice predicate in test/gate-stdlib-lib.f selects
\ its label AND some phase of test/run.f asks for that slice, or when its files
\ appear in one of the inline gate bodies' GSI lists. The remaining route - the
\ bare (ALL) slice, which selects every label - is spawned by no phase of
\ test/run.f and never has been. So a registration whose label no reachable
\ slice names and whose files no list names is DARK: it looks scheduled, it is
\ counted in the suite inventory, and nothing runs it. Fifty three of them were
\ dark when this lint was written, including seven proof parity gates and the
\ clang reference column's fork case.
\
\ THE SECOND HALF, and why it is a check of its own. "The tail slice selects
\ this label" is only half an answer: the tail slice ran nowhere, because phase
\ $4 was in none of test/run-lib.f's order tables. Five registrations were
\ selected and never run, and both the gate and the first version of this lint
\ were green throughout. A slice is a runner only if a phase asks for it and
\ something starts that phase, so this lint asks both:
\   PHASES     every phase test/run-lib.f has, that it has not retired
\              (TEST:PHASE-RETIRED?), must appear in one of its schedule tables
\              or in its deferred table. A phase in none of them is started by
\              nothing. When such a phase runs a stdlib slice, the report also
\              names the registrations that slice was carrying.
\   LABELS     a registration's label counts as covered only when the REAL
\              predicate - STDLIB-GATE:SLICE-SELECTS?, which is SUITE-RUN? with
\              the label the registry stores - answers yes for a slice that some
\              live phase asks for. A predicate written for a slice no phase
\              names schedules nothing, and now says so.
\
\ WHAT IT CHECKS. A registration is covered when its label is covered as above
\ OR every one of its files is in the scheduled set. Partial coverage is a
\ finding too: a registration with two files and one of them scheduled is half
\ dark, and the half that is dark is the half nobody notices.
\
\ THE THIRD QUESTION, and the one the first two cannot ask. Everything above
\ starts from a REGISTRATION and asks whether anything runs it. A test source
\ that was never registered at all is invisible to that: it is in no SUITE row,
\ so nothing walks it, and in no fork list, so it does not even move the
\ scheduled-file count. Measured on this tree before the check existed, dropping
\ a conforming suite at test/compiler/native-probe-gap.f left every number
\ identical - 223 registrations, 337 scheduled files, 0 findings, byte for byte.
\ "It passes" and "it is scheduled" were already separate facts; this is the
\ third, that it EXISTS and nothing reaches it.
\   REACH       a source is reached when a gate fork list or a SUITE row names
\               it, or when something already reached loads it. The second half
\               is a transitive closure and not an allowlist: most of test/ is
\               support - fixtures a driver spawns, `-lib.f` halves, obligation
\               and schema files a proof requires - and those are reached
\               through their loader, not by being registered themselves. When
\               this landed the closure carried a few hundred roots out to just
\               over a thousand sources; SUMMARY prints both counts. The closure
\               is the whole difference between a check with nine findings and
\               one with a hundred and seventy eight.
\   DISK        every `.f` under test/ that the closure did not reach is a
\               finding, named by path. Not lib/ or tools/: a source there is a
\               library until something makes it a test, while test/ has no
\               other purpose. Widening it is habu-tier-parity-lint-31aaa95b's.
\               `.fs` is out of scope on both sides of that walk - those are
\               Gforth recovery-host sources, which this gate does not schedule
\               and cannot run - though the closure still follows a require into
\               one, so a `.fs` file a habu source loads is counted as reached.
\
\ HOW A SOURCE IS REACHED, structurally. Two shapes, both read off the shared
\ lexer's token table rather than searched for: `require`/`include` followed by
\ a word that names a source on disk, and a string literal any of whose payload
\ words names one. The literal arm covers spawning, where the path is data
\ handed to a runner - `s" test/compiler/ir-id-replay.f" HB-LOAD-OUTCOME` - and
\ it reads payload WORDS, not the whole payload, because a generated line like
\ `s" require test/compiler/ir-id-concurrency.f" SB-APPEND` carries the path
\ inside a longer literal. The alternative was a list of spawning words to match
\ on, which is exactly the hand-written list this lint refuses to keep anywhere
\ else. Both arms demand the file EXIST, so a literal naming a path that is not
\ there grants nothing, and neither arm can see a path in a comment: the lexer
\ consumed those before the table was built.
\
\ WHAT THIS ARM CAN MISS, stated plainly because it grants coverage rather than
\ demanding it. A path assembled at runtime from pieces - a directory in one
\ literal and a basename in another - is invisible here, and its file reports as
\ unreached. That is a false RED and the safe direction: it demands an answer.
\ The answer is a documented pragma, below.
\
\ THE PRAGMA. A source that is deliberately unscheduled says so in itself:
\   \ schedule-lint: allow-unscheduled - <why, naming the spawner or the dot>
\ It is the `typed-local-lint: allow-bare-local` convention this tree already
\ runs 425 times, with one tightening: the marker is read at the head of a `\`
\ comment LINE, so a mention of it inside prose, a string, or a longer sentence
\ is not a pragma. Every use carries its reason on the same line. A pragma is
\ not an allowlist entry - it lives in the file it excuses, and it is the thing
\ a reviewer of that file reads first.
\
\ WHERE THE SETS COME FROM. All of them are derived from the real sources; there
\ is no hand-written list of suites, slices or phases to go stale.
\   slices          test/run-lib.f PHASE-SLICE-TOKEN over every live phase, put
\                   through STDLIB-GATE:SLICE-ID?
\   the predicate   test/gate-stdlib-lib.f, run, not read
\   phase schedule  test/run-lib.f's order tables, read as data
\   scheduled files every .f under test/, the shape
\                   `s" PATH" GSI-FORK-INCLUDE | GSI-INCLUDE | GSI-REQUIRE`
\   reached files   those, plus every SUITE row's members, closed transitively
\                   over require/include and spawn literals
\   disk            every .f WALK-FILES finds under test/
\ All three inline gate bodies live under test/, so walking test/ reaches every
\ list there is. A GSI list placed OUTSIDE test/ would be invisible here and its
\ registration would report as dark - a false RED, which is the safe direction:
\ it demands an answer instead of quietly granting coverage.
\
\ WHY IT LEXES THE REGISTRATIONS INSTEAD OF SEARCHING THEM. The question is
\ structural, so the answer has to be. `SUITE` written inside a comment or a
\ string literal is not a registration; `: SUITE ( -- ) ... ;` defines the opener
\ rather than opening a registration. Every one of those reads as a hit to a text
\ search. The shared source lexer (package LINT-LEX, tools/lint/source-lex.f)
\ consumes `\` line comments, `( ... )` and `.( ... )` bodies, and every string
\ literal body, and it hands a literal's payload back through CONTENT so the
\ payload is reachable in its ROLE and nowhere else. A lexer diagnostic truncates
\ the token table at the defect, so every later registration in that source would
\ be invisible: this lint rejects on LINT-LEX:ERROR? rather than certify a
\ verdict built from half a file. The SELECTOR is not lexed at all any more - it
\ is loaded and asked.
\
\ THE SUITE GRAMMAR IS lib/test/suite.f's, not an approximation of it. `SUITE`
\ parses one name; `SUITE-STDIN` parses a name and then its stdin datum. Every
\ token after that until `;SUITE` becomes an argv entry, and the runner spawns
\ `hb --load <entries...>`, so the entries before the first `--` are the source
\ files and everything after it is script argv. That is why
\ `lib/argv-test.f -- --json ... -- file.f --literal` has ONE file and not two.
\ The opener is matched case-insensitively because the engine looks it up in a
\ case-insensitive dictionary; the closer is matched case-sensitively because
\ suite.f's own `;SUITE?` compares its text with STR=, so `;suite` is an argv
\ entry there and must be one here.
\
\ Run: it is loaded as a library. tools/lint/schedule-lint-test.f drives it over
\ fixtures and then enforces the live tree; the gate runs that file in the
\ lint-tools body (test/gate-stdlib-lint-tools.f).

require lib/errors.f
require lib/string.f
require lib/memory.f
require lib/vector.f
require lib/fs.f
require tools/lint/text.f
require tools/lint/token.f
require tools/lint/lib.f
require tools/lint/source-lex.f
require test/run-lib.f                  \ the phase schedule, read as data
require test/gate-stdlib-lib.f          \ the slice predicate, run as itself

package SCHEDULE-LINT

public

\ Source-defect codes. Each lexer diagnostic stops its scan, so each gets its own
\ name the way tools/lint/shadow-lint.f splits E-SHADOW-UNTERM from
\ E-SHADOW-REGISTRY. Negative so tools/error-code-lint.f keeps them globally
\ unique; -4812..-4815 continue the unclaimed lint-tool gap that already holds
\ -4800..-4811, and the gap ends before lib/errors.f's E-REPORT block at -4900.
\ Public so the fixtures can name the exact refusal they provoke instead of
\ asserting "it threw something".
-4812 constant E-SCHED-QUOTE            \ a string literal ran past end of input
-4813 constant E-SCHED-ROW              \ a `PRIM:`/`PPRIM:` axiom row lacked a header or its closer
\ The residual arm: a diagnostic kind added to LINT-LEX after this consumer was
\ written must reach a named refusal instead of being reported as one of the two
\ defects this lint does understand, and it must never pass silently.
-4814 constant E-SCHED-LEX              \ a lexer diagnostic this lint was never taught
\ A registration the real parser would reject at load: an opener with no label
\ after it, one whose label is not a plain word, or one that runs off the end of
\ the source without its closer. A verdict over a source the gate could not load
\ is worthless, so it stops.
-4815 constant E-SCHED-SYNTAX
\ The runner grew more phases or more slices than this lint sized itself for.
\ Growing past a cap must stop the lint, never silently drop the tail of a table:
\ a phase this lint never looked at is a phase it would have called scheduled.
-4816 constant E-SCHED-CAP

private

\ The closure follows require lines out of test/ into the engine's own sources,
\ so the largest source this reads is no longer the largest TEST: src/core/checker.f
\ is 514K where test/engine-suite.f is 162K. READ-ALL probes past the cap and
\ throws E-FS-CAPACITY rather than returning a truncated file, so a cap set too
\ low stops the lint instead of silently losing that file's references - which is
\ how this number was found to be wrong.
$100000 constant FILE-CAP               \ largest source the closure can reach + headroom
$4000 constant LABEL-CAP                \ packed selected labels
$8000 constant SCHED-CAP                \ packed scheduled paths
\ The closure is the whole load graph, not just test/: about 25.6KB packed for
\ the thousand-odd sources on this tree, so this is 2.5x headroom. Overflow is a
\ throw, never a truncation - a reach set that quietly stopped recording would
\ start calling reached files dark.
$10000 constant REACH-CAP               \ packed reached paths
$100 constant NAME-CAP                  \ one suite label

0 constant UNFILED-SET                  \ labels whose files are not all scheduled
1 constant SCHED-SET                    \ files a gate fork list schedules
2 constant REACH-SET                    \ files some runner reaches, transitively

$40 constant PHASE-CAP                  \ >= TEST:PHASES
$10 constant SLICE-CAP                  \ >= the slices STDLIB-GATE:SLICE-ID? knows

create LABEL-BUF LABEL-CAP allot        \ packed [len][bytes] runs
create SCHED-BUF SCHED-CAP allot
create REACH-BUF REACH-CAP allot
create WORK-BUF FS-PATH-CAP allot       \ the closure's current path, copied out of the set
create PHASE-STARTED PHASE-CAP allot    \ one byte per phase: an order table names it
create PHASE-DEFERRED PHASE-CAP allot   \ one byte per phase: the deferred table names it
create LIVE-SLICE PHASE-CAP cells allot \ slice ids the live phases ask for
create NAME-BUF NAME-CAP allot
create ORIGIN-BUF FS-PATH-CAP allot

variable COVERED?                       \ a reachable slice selects the registration under verdict
variable AUDIT-K                        \ cursor of the carried-registration walk
variable LIVE-SLICE-N
variable PHASE-I                        \ cursor of the phase walks
variable SLICE-I                        \ cursor of the slice walks
variable LABEL-U   variable LABEL-N
variable SCHED-U   variable SCHED-N
variable REACH-U   variable REACH-N
variable REACH-I                        \ closure cursor: entries below it have been scanned
variable SEED-I                         \ cursor of the fork-list seeding walk
variable WORK-U
variable LIT-I     variable LIT-J       \ payload-word walk over one string literal
variable PR-I      variable PR-J        \ line walk of the pragma scan
variable DARK-N                         \ unreached sources found on disk
variable NAME-U
variable ORIGIN-U
variable FILE-A
variable FILE-U
variable PREPARED
variable REPORT?
variable BAD                            \ uncovered registrations
variable SUITE-N                        \ registrations the scan read
variable CUR-LINE                       \ line of the registration under verdict
variable CUR-MISS                       \ uncovered files in that registration
variable CUR-FILES                      \ files it names at all
variable SCAN-I                         \ token cursor of the registration walk
variable HAS-OFF   variable HAS-K   variable HAS-LEN   \ membership walk over a packed run
variable WALK-I                         \ token cursor of the whole-source scans
variable PAST-SEP                       \ the registration walk passed its `--`
variable HIT-LINE                       \ cases-source line of the most recent finding
create HIT-BUF NAME-CAP allot           \ and the label it named
variable HIT-U
-1 REPORT? !                            \ findings print unless a caller silences them

\ The engine's `.` ends its number with a newline, which would break every line
\ below in half. This is the namespace-lint NL-U. digit-buffer render, kept
\ package-local: unsigned, inline, no trailing byte of its own.
$30 constant ZERO-C
create NUM-BUF 32 allot
variable NUM-D

: DEC. ( n -- ) {: v:n :}
   v 0 < if E-STR-BOUNDS throw then
   0 NUM-D !
   v 0= if ZERO-C emit exit then
   v begin dup 0 > while
      dup 10 mod ZERO-C + NUM-BUF NUM-D @ + c!
      NUM-D @ 1+ NUM-D !
      10 /
   repeat drop
   begin NUM-D @ 0 > while
      NUM-D @ 1- NUM-D !
      NUM-BUF NUM-D @ + c@ emit
   repeat ;

\ ---- the two packed [len][bytes] sets ---------------------------------------
\ One algorithm, two buffers. The selectors below are the only place either
\ buffer is named, so add, membership and reset are written once
\ (docs/forth.md § ptr locals: factor the arithmetic, duplicate the buffer
\ touch). A bare `ptr` local would lose the `ptr u8` role, so the base is
\ re-derived at each use rather than bound.

: SET-BASE ( n -- ptr u8 ) {: s:n :}
   s UNFILED-SET = if LABEL-BUF exit then
   s SCHED-SET = if SCHED-BUF exit then
   REACH-BUF ;

: SET-CAP@ ( n -- n ) {: s:n :}
   s UNFILED-SET = if LABEL-CAP exit then
   s SCHED-SET = if SCHED-CAP exit then
   REACH-CAP ;

: SET-U@ ( n -- n ) {: s:n :}
   s UNFILED-SET = if LABEL-U @ exit then
   s SCHED-SET = if SCHED-U @ exit then
   REACH-U @ ;

: SET-U! ( n n -- ) {: v:n s:n :}
   s UNFILED-SET = if v LABEL-U ! exit then
   s SCHED-SET = if v SCHED-U ! exit then
   v REACH-U ! ;

: SET-N@ ( n -- n ) {: s:n :}
   s UNFILED-SET = if LABEL-N @ exit then
   s SCHED-SET = if SCHED-N @ exit then
   REACH-N @ ;

: SET-N! ( n n -- ) {: v:n s:n :}
   s UNFILED-SET = if v LABEL-N ! exit then
   s SCHED-SET = if v SCHED-N ! exit then
   v REACH-N ! ;

: SET-BYTE ( n n -- n ) {: s:n off:n :}
   s SET-BASE off + c@ ;

: SET-RESET ( n -- ) {: s:n :}
   0 s SET-U!
   0 s SET-N! ;

\ Capacity is a loud throw, never a truncation: a set that quietly stopped
\ recording would start answering "nothing schedules this" for files that are
\ scheduled, which is the one answer that turns a green tree red for no reason.
: SET+ ( n ptr u8 n -- ) {: s:n a:ptr u:n :}
   u 0 <= if E-STR-BOUNDS throw then
   u STR-BYTE-MAX > if E-STR-BOUNDS throw then
   s SET-U@ u + 1 + s SET-CAP@ > if E-STR-CAPACITY throw then
   u s SET-BASE s SET-U@ + c!
   a s SET-BASE s SET-U@ + 1 + u BYTE-COPY
   s SET-U@ u + 1 + s SET-U!
   s SET-N@ 1+ s SET-N! ;

: SET-HAS? ( n ptr u8 n -- bool ) {: s:n a:ptr u:n :}
   0 HAS-OFF !
   0 HAS-K !
   begin HAS-K @ s SET-N@ < while
      s HAS-OFF @ SET-BYTE HAS-LEN !
      s SET-BASE HAS-OFF @ + 1 + HAS-LEN @ a u STR= if LINT-TRUE exit then
      HAS-OFF @ HAS-LEN @ + 1 + HAS-OFF !
      HAS-K @ 1+ HAS-K !
   repeat
   LINT-FALSE ;

\ The k-th entry, for a caller that has to report a set rather than test it.
: SET-AT ( n n -- ptr u8 n ) {: s:n k:n :}
   k 0 < if E-STR-BOUNDS throw then
   k s SET-N@ >= if E-STR-BOUNDS throw then
   0 HAS-OFF !
   0 HAS-K !
   begin HAS-K @ k < while
      s HAS-OFF @ SET-BYTE HAS-LEN !
      HAS-OFF @ HAS-LEN @ + 1 + HAS-OFF !
      HAS-K @ 1+ HAS-K !
   repeat
   s HAS-OFF @ SET-BYTE HAS-LEN !
   s SET-BASE HAS-OFF @ + 1 + HAS-LEN @ ;

\ ---- the reach set: what some runner loads -----------------------------------
\ A candidate becomes a member only if it is a source path that EXISTS. The
\ existence test is last because it is a syscall and the two cheap tests reject
\ almost everything: a suffix compare kills ordinary literals, and the
\ membership test kills the repeats, which is most of what a load graph offers -
\ the members absorb several times their own number of references.

: SRC-SUFFIX? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   a u s" .f" LINT-ENDS-WITH? if LINT-TRUE exit then
   a u s" .fs" LINT-ENDS-WITH? ;

: REACH-TRY ( ptr u8 n -- ) {: a:ptr u:n :}
   u 0 <= if exit then
   u FS-PATH-CAP > if exit then
   a u SRC-SUFFIX? 0= if exit then
   REACH-SET a u SET-HAS? if exit then
   a u EXISTS? 0= if exit then
   REACH-SET a u SET+ ;

\ ---- the source under scan ---------------------------------------------------

: FILE-A-FIELD ( -- ptr ptr u8 )
   FILE-A 0 ptr-field ;

: FILE$ ( -- ptr u8 n )
   FILE-A-FIELD @ FILE-U @ ;

: ORIGIN$ ( -- ptr u8 n )
   ORIGIN-BUF ORIGIN-U @ ;

: ORIGIN! ( ptr u8 n -- ) {: a:ptr u:n :}
   u 0 < if E-FS-PATH throw then
   u FS-PATH-CAP > if E-FS-PATH throw then
   a ORIGIN-BUF u BYTE-COPY
   u ORIGIN-U ! ;

: NAME$ ( -- ptr u8 n )
   NAME-BUF NAME-U @ ;

: NAME! ( ptr u8 n -- ) {: a:ptr u:n :}
   u 0 <= if E-SCHED-SYNTAX throw then
   u NAME-CAP > if E-STR-CAPACITY throw then
   a NAME-BUF u BYTE-COPY
   u NAME-U ! ;

\ ---- token questions ---------------------------------------------------------

: LEX-WORD? ( n -- bool ) {: k:n :}
   k LINT-LEX:KIND@ LINT-LEX:WORD = ;

\ Token k is this word, case-insensitively: the engine's dictionary folds case,
\ so `create` and `CREATE` are one name and so are `suite` and `SUITE`.
: LEX-IS ( n ptr u8 n -- bool ) {: k:n a:ptr u:n :}
   k LEX-WORD? 0= if LINT-FALSE exit then
   k LINT-LEX:TOKEN a u LINT-STR=CI ;

: LEX-IS-EXACT ( n ptr u8 n -- bool ) {: k:n a:ptr u:n :}
   k LEX-WORD? 0= if LINT-FALSE exit then
   k LINT-LEX:TOKEN a u LINT-STR= ;

\ A string-literal token. The lexer never tokenizes a literal's body, so the
\ payload is reachable only through CONTENT - which is exactly what makes a
\ quoted label readable in its role and unreadable as code.
: LEX-STRING? ( n -- bool ) {: k:n :}
   k LEX-WORD? 0= if LINT-FALSE exit then
   k LINT-LEX:TOKEN LINT-ESC-STRING-OPENER? if LINT-TRUE exit then
   k LINT-LEX:TOKEN LINT-NORMAL-STRING-OPENER? ;

\ After one of these the engine parses the next word as a NAME and never executes
\ it, so `: SUITE ( -- ) ... ;` in lib/test/suite.f DEFINES the opener instead of
\ opening a registration. This is LINT-LEX's own NAME-POS? rule, applied to the
\ opener this lint reads.
: NAME-POS? ( n -- bool ) {: k:n :}
   k 0= if LINT-FALSE exit then
   k 1- s" :" LEX-IS-EXACT if LINT-TRUE exit then
   k 1- s" '" LEX-IS-EXACT if LINT-TRUE exit then
   k 1- s" [']" LEX-IS-EXACT if LINT-TRUE exit then
   k 1- s" postpone" LEX-IS if LINT-TRUE exit then
   k 1- s" undefine" LEX-IS ;

\ ---- the phase schedule, read out of test/run-lib.f ---------------------------
\ Two byte tables, one per question. STARTED means an order table names the
\ phase; DEFERRED means the deferred table does. Neither is a hand-list: both are
\ filled by walking the runner's own `create ... ,` tables through its accessors.

: PHASE-BOUND ( n -- ) {: i:n :}
   i 0 < if E-SCHED-CAP throw then
   i PHASE-CAP >= if E-SCHED-CAP throw then ;

: STARTED! ( n -- ) {: i:n :}
   i PHASE-BOUND
   1 PHASE-STARTED i + c! ;

: STARTED? ( n -- bool ) {: i:n :}
   i PHASE-BOUND
   PHASE-STARTED i + c@ 0 <> ;

: DEFERRED! ( n -- ) {: i:n :}
   i PHASE-BOUND
   1 PHASE-DEFERRED i + c! ;

: DEFERRED? ( n -- bool ) {: i:n :}
   i PHASE-BOUND
   PHASE-DEFERRED i + c@ 0 <> ;

\ LIVE means the runner has not retired the phase: TR-PHASE-ARGS will build an
\ argv for it, so something is meant to start it.
: LIVE-PHASE? ( n -- bool ) {: i:n :}
   i >IDX TEST:PHASE-RETIRED? 0= ;

: PHASE-TABLES-RESET ( -- )
   0 PHASE-I !
   begin PHASE-I @ PHASE-CAP < while
      0 PHASE-STARTED PHASE-I @ + c!
      0 PHASE-DEFERRED PHASE-I @ + c!
      PHASE-I @ 1+ PHASE-I !
   repeat ;

: MARK-EARLY ( -- )
   0 PHASE-I !
   begin PHASE-I @ TEST:EARLY-HOST-PHASES < while
      PHASE-I @ >IDX TEST:EARLY-HOST-ORDER@ IDX>N STARTED!
      PHASE-I @ 1+ PHASE-I !
   repeat ;

: MARK-LATE ( -- )
   0 PHASE-I !
   begin PHASE-I @ TEST:LATE-PHASES < while
      PHASE-I @ >IDX TEST:LATE-ORDER@ IDX>N STARTED!
      PHASE-I @ 1+ PHASE-I !
   repeat ;

: MARK-CANDIDATE ( -- )
   0 PHASE-I !
   begin PHASE-I @ TEST:CANDIDATE-HOST-PHASES < while
      PHASE-I @ >IDX TEST:CANDIDATE-HOST-ORDER@ IDX>N STARTED!
      PHASE-I @ 1+ PHASE-I !
   repeat ;

: MARK-READY-DIRECT ( -- )
   0 PHASE-I !
   begin PHASE-I @ TEST:READY-DIRECT-PHASES < while
      PHASE-I @ >IDX TEST:READY-DIRECT-ORDER@ IDX>N STARTED!
      PHASE-I @ 1+ PHASE-I !
   repeat ;

: MARK-READY-SHARED ( -- )
   0 PHASE-I !
   begin PHASE-I @ TEST:READY-SHARED-PHASES < while
      PHASE-I @ >IDX TEST:READY-SHARED-ORDER@ IDX>N STARTED!
      PHASE-I @ 1+ PHASE-I !
   repeat ;

: MARK-DIRECT ( -- )
   0 PHASE-I !
   begin PHASE-I @ TEST:DIRECT-PHASES < while
      PHASE-I @ >IDX TEST:DIRECT-ORDER@ IDX>N STARTED!
      PHASE-I @ 1+ PHASE-I !
   repeat ;

: MARK-DEFERRED ( -- )
   0 PHASE-I !
   begin PHASE-I @ TEST:DEFERRED-PHASES < while
      PHASE-I @ >IDX TEST:DEFERRED-ORDER@ IDX>N DEFERRED!
      PHASE-I @ 1+ PHASE-I !
   repeat ;

: MARK-SCHEDULE ( -- )
   PHASE-TABLES-RESET
   MARK-EARLY
   MARK-LATE
   MARK-CANDIDATE
   MARK-READY-DIRECT
   MARK-READY-SHARED
   MARK-DIRECT
   MARK-DEFERRED ;

\ ---- the slices those phases ask for -----------------------------------------
\ LIVE-SLICE holds every slice a live phase names. A label is covered when one of
\ them selects it: a predicate written for a slice NO phase names schedules
\ nothing, whatever it selects. Whether the phases that name a slice are actually
\ started is the phase audit's question, not this list's - keeping the two apart
\ is what lets the audit report say which registrations a dark phase was
\ carrying.

: SLICE-CELL ( ptr a n -- ptr a ) {: base:ptr i:n :}
   i 0 < if E-SCHED-CAP throw then
   i PHASE-CAP >= if E-SCHED-CAP throw then
   base i cells + ;

: LIVE-SLICE+ ( n -- ) {: id:n :}
   id LIVE-SLICE LIVE-SLICE-N @ SLICE-CELL !
   LIVE-SLICE-N @ 1+ LIVE-SLICE-N ! ;

: SLICE-AT ( n -- ) {: i:n :}
   i LIVE-PHASE? 0= if exit then
   i >IDX TEST:STDLIB-SLICE? 0= if exit then
   i >IDX TEST:PHASE-SLICE-TOKEN STDLIB-GATE:SLICE-ID? MATCH option
     none OF exit ENDOF
     some OF ENDOF
   ;MATCH
   LIVE-SLICE+ ;

: COLLECT-SLICES ( -- )
   0 LIVE-SLICE-N !
   0 PHASE-I !
   begin PHASE-I @ TEST:PHASES < while
      PHASE-I @ SLICE-AT
      PHASE-I @ 1+ PHASE-I !
   repeat ;

\ ---- asking the real predicate ------------------------------------------------

: SLICE-HIT? ( ptr u8 n ptr a n -- bool ) {: a:ptr u:n base:ptr n:n :}
   0 SLICE-I !
   begin SLICE-I @ n < while
      a u base SLICE-I @ SLICE-CELL @ STDLIB-GATE:SLICE-SELECTS? if LINT-TRUE exit then
      SLICE-I @ 1+ SLICE-I !
   repeat
   LINT-FALSE ;

public

\ Covered by a slice: some live phase asks for a slice whose predicate selects
\ this label. The predicate is not read here, it is RUN - SLICE-SELECTS? is
\ SUITE-RUN? with the label put through the registry's own store.
: LABEL-COVER? ( ptr u8 n -- bool )
   LIVE-SLICE LIVE-SLICE-N @ SLICE-HIT? ;

private

\ ---- the scheduled set: `s" PATH" GSI-*` -------------------------------------

: SCHEDULER? ( n -- bool ) {: k:n :}
   k s" GSI-FORK-INCLUDE" LEX-IS if LINT-TRUE exit then
   k s" GSI-INCLUDE" LEX-IS if LINT-TRUE exit then
   k s" GSI-REQUIRE" LEX-IS ;

: SCHED-AT ( n -- ) {: k:n :}
   k LEX-STRING? 0= if exit then
   k 1+ LINT-LEX:COUNT >= if exit then
   k 1+ SCHEDULER? 0= if exit then
   SCHED-SET k LINT-LEX:CONTENT SET+ ;

\ ---- the registration grammar ------------------------------------------------

: SUITE-FILE-OPEN? ( n -- bool ) {: k:n :}
   k s" SUITE" LEX-IS if LINT-TRUE exit then
   k s" TEST:SUITE" LEX-IS ;

: SUITE-STDIN-OPEN? ( n -- bool ) {: k:n :}
   k s" SUITE-STDIN" LEX-IS if LINT-TRUE exit then
   k s" TEST:SUITE-STDIN" LEX-IS ;

: SUITE-OPEN? ( n -- bool ) {: k:n :}
   k NAME-POS? if LINT-FALSE exit then
   k SUITE-FILE-OPEN? if LINT-TRUE exit then
   k SUITE-STDIN-OPEN? ;

\ Case-SENSITIVE, because suite.f's own `;SUITE?` compares text with STR=: a
\ token spelled `;suite` is an ordinary argv entry to the real parser, and a
\ closer this lint accepted there would end a registration the gate does not.
: SUITE-CLOSE? ( n -- bool ) {: k:n :}
   k s" ;SUITE" LEX-IS-EXACT if LINT-TRUE exit then
   k s" TEST:;SUITE" LEX-IS-EXACT ;

\ suite.f's PARSE-NAME rejects an empty name and CHECK-NAME rejects a reserved
\ one, so the token after an opener is a plain word or the source does not load.
: LABEL-AT ( n -- ) {: k:n :}
   k LINT-LEX:COUNT >= if E-SCHED-SYNTAX throw then
   k LEX-WORD? 0= if E-SCHED-SYNTAX throw then
   k LEX-STRING? if E-SCHED-SYNTAX throw then
   k SUITE-CLOSE? if E-SCHED-SYNTAX throw then
   k LINT-LEX:TOKEN NAME! ;

\ ---- the verdict -------------------------------------------------------------

: FINDING-HEAD ( -- )
   REPORT? @ 0= if exit then
   s" schedule-lint: " type ORIGIN$ type
   s" :" type CUR-LINE @ DEC.
   s"  SUITE " type NAME$ type
   s"  is unscheduled: no slice predicate selects its label" type cr ;

\ Record which registration a finding named, so a fixture can assert the finding
\ it MEANT to provoke rather than a count that any other defect could also reach.
: HIT! ( -- )
   BAD @ 1+ BAD !
   CUR-LINE @ HIT-LINE !
   NAME-BUF HIT-BUF NAME-U @ BYTE-COPY
   NAME-U @ HIT-U ! ;

\ Every registration whose files are not all scheduled, whether or not a slice
\ covers it. A covered one is not a finding, but it IS what its slice is
\ carrying: if the phase that asks for that slice stops being started, this is
\ the list that goes dark, and the phase report names it.
: UNFILED+ ( -- )
   UNFILED-SET NAME$ SET-HAS? if exit then
   UNFILED-SET NAME$ SET+ ;

: MISS-COUNT ( -- )
   CUR-MISS @ 1+ CUR-MISS !
   CUR-MISS @ 1 <> if exit then
   UNFILED+
   COVERED? @ 0 <> if exit then
   HIT! FINDING-HEAD ;

: MISS-LINE ( ptr u8 n -- ) {: a:ptr u:n :}
   COVERED? @ 0 <> if exit then
   REPORT? @ 0= if exit then
   s" schedule-lint:   " type a u type
   s"  is in no GSI-FORK-INCLUDE / GSI-INCLUDE / GSI-REQUIRE list under test/" type cr ;

\ A registration's members are reach roots whether or not a fork list also names
\ them: the SUITE row IS a runner, and the standalone gate spawns it. This runs
\ before the scheduled-set test below so a member that is dark for the coverage
\ question still seeds the closure for the disk question - the two ask different
\ things of the same path.
: FILE-CHECK ( ptr u8 n -- ) {: a:ptr u:n :}
   CUR-FILES @ 1+ CUR-FILES !
   a u REACH-TRY
   SCHED-SET a u SET-HAS? if exit then
   MISS-COUNT
   a u MISS-LINE ;

\ A registration that names no source file at all spawns `hb --load` with
\ nothing to load: it registers a label and runs no code, which is the same
\ emptiness this lint exists to find, reached from the other side. A slice
\ selecting its label does not save it - there is nothing to run either way.
: EMPTY-CHECK ( -- )
   CUR-FILES @ 0 <> if exit then
   UNFILED+
   HIT!
   REPORT? @ 0= if exit then
   s" schedule-lint: " type ORIGIN$ type
   s" :" type CUR-LINE @ DEC.
   s"  SUITE " type NAME$ type
   s"  names no source file" type cr ;

\ Walk the argv entries of one registration from token `SCAN-I`, which the
\ caller left just past the label (and past the stdin datum for a SUITE-STDIN).
\ Entries before the first `--` are the sources hb loads; everything after it is
\ script argv and names no file. Comment and registry tokens are not entries.
: BODY-ENTRY ( -- )
   SCAN-I @ LEX-WORD? 0= if exit then
   SCAN-I @ s" --" LEX-IS-EXACT if -1 PAST-SEP ! exit then
   PAST-SEP @ 0 <> if exit then
   SCAN-I @ LINT-LEX:TOKEN FILE-CHECK ;

: BODY-WALK ( -- )
   0 CUR-FILES !
   0 PAST-SEP !
   begin SCAN-I @ LINT-LEX:COUNT < while
      SCAN-I @ SUITE-CLOSE? if exit then
      BODY-ENTRY
      SCAN-I @ 1+ SCAN-I !
   repeat
   E-SCHED-SYNTAX throw ;

\ A registration a reachable slice selects is scheduled whatever its files say.
\ Its body is still walked, because what a slice is CARRYING - the members no
\ fork list also schedules - is what the phase report has to name when nothing
\ starts the phase that asks for that slice.
: VERDICT ( -- )
   0 CUR-MISS !
   NAME$ LABEL-COVER? COVERED? !
   BODY-WALK
   EMPTY-CHECK ;

: SUITE-AT ( n -- ) {: k:n :}
   SUITE-N @ 1+ SUITE-N !
   k LINT-LEX:LINE@ CUR-LINE !
   k 1+ LABEL-AT
   k 2 + SCAN-I !
   k SUITE-STDIN-OPEN? if SCAN-I @ 1+ SCAN-I ! then
   VERDICT ;

\ ---- lexing one source -------------------------------------------------------

\ The refusal is the throw; the line below is only its explanation, so a fixture
\ that provokes a refusal on purpose silences it exactly as it silences a
\ deliberate finding. The named code still reaches the caller either way.
: LEX-QUOTE-FAIL ( -- )
   REPORT? @ 0= if E-SCHED-QUOTE throw then
   s" schedule-lint: unterminated string literal in " type ORIGIN$ type
   s"  (line " type LINT-LEX:ERROR-LINE@ DEC. s" )" type cr
   E-SCHED-QUOTE throw ;

: LEX-ROW-FAIL ( -- )
   REPORT? @ 0= if E-SCHED-ROW throw then
   s" schedule-lint: malformed primitive registry row in " type ORIGIN$ type
   s"  (line " type LINT-LEX:ERROR-LINE@ DEC. s" )" type cr
   E-SCHED-ROW throw ;

: LEX-OTHER-FAIL ( -- )
   REPORT? @ 0= if E-SCHED-LEX throw then
   s" schedule-lint: unknown lexer diagnostic " type LINT-LEX:ERROR-KIND@ DEC.
   s"  in " type ORIGIN$ type
   s"  (line " type LINT-LEX:ERROR-LINE@ DEC. s" )" type cr
   E-SCHED-LEX throw ;

\ Fail closed. A diagnostic ends the scan at the defect, so every registration,
\ label and path after that point is invisible; continuing would publish a
\ verdict drawn from half a file.
: LEX-CHECK ( -- )
   LINT-LEX:ERROR? 0= if exit then
   LINT-LEX:ERROR-KIND@ LINT-LEX:MALFORMED-REGISTRY = if LEX-ROW-FAIL then
   LINT-LEX:ERROR-KIND@ LINT-LEX:UNTERMINATED-QUOTE = if LEX-QUOTE-FAIL then
   LEX-OTHER-FAIL ;

: LEX-SOURCE ( ptr u8 n ptr u8 n -- ) {: oa:ptr ou:n a:ptr u:n :}
   oa ou ORIGIN!
   a u LINT-LEX:SOURCE
   LEX-CHECK ;

: READ-INTO ( ptr u8 n -- ) {: a:ptr u:n :}
   a u FILE-A-FIELD @ FILE-CAP READ-ALL FILE-U ! ;

\ ---- the reach closure --------------------------------------------------------
\
\ Breadth-first over the load graph, with the reach set doubling as the queue:
\ REACH-I is the cursor of what has been SCANNED, SET-N@ the count of what has
\ been FOUND, and every path is appended once, so the walk ends exactly when the
\ two meet. New members appended during a scan are behind the cursor and get
\ their turn; a member already present is not re-queued, which is what keeps a
\ cycle (two files that require each other) from spinning.

$20 constant SP-C
$0A constant NL-C

\ Bounds first, byte second: the offset walks below run to the end of their
\ buffer, so the test at the end must answer without reading there.
: BLANK-AT? ( ptr u8 n n -- bool ) {: a:ptr u:n i:n :}
   i u >= if LINT-FALSE exit then
   a i + c@ SP-C <= ;

: TEXT-AT? ( ptr u8 n n -- bool ) {: a:ptr u:n i:n :}
   i u >= if LINT-FALSE exit then
   a i + c@ SP-C > ;

\ Every whitespace-separated word of one literal's payload. `s" lib/x.f -- --json"`
\ offers three words and one of them is a path; `s" require test/y.f"` offers two.
\ Reading the payload whole would see neither.
: PAYLOAD-WORDS ( ptr u8 n -- ) {: a:ptr u:n :}
   0 LIT-I !
   begin LIT-I @ u < while
      begin a u LIT-I @ BLANK-AT? while LIT-I @ 1+ LIT-I ! repeat
      LIT-I @ LIT-J !
      begin a u LIT-J @ TEXT-AT? while LIT-J @ 1+ LIT-J ! repeat
      LIT-J @ LIT-I @ > if a LIT-I @ + LIT-J @ LIT-I @ - REACH-TRY then
      LIT-J @ LIT-I !
   repeat ;

\ `require PATH` / `include PATH`: the engine parses the next word as the path,
\ so the token after the directive is the candidate and a string literal there is
\ not one. Case-insensitive, because the dictionary is.
: LOAD-DIRECTIVE? ( n -- bool ) {: k:n :}
   k s" require" LEX-IS if LINT-TRUE exit then
   k s" include" LEX-IS ;

: REF-DIRECTIVE ( n -- ) {: k:n :}
   k LOAD-DIRECTIVE? 0= if exit then
   k 1+ LINT-LEX:COUNT >= if exit then
   k 1+ LEX-WORD? 0= if exit then
   k 1+ LEX-STRING? if exit then
   k 1+ LINT-LEX:TOKEN REACH-TRY ;

: REF-AT ( n -- ) {: k:n :}
   k REF-DIRECTIVE
   k LEX-STRING? 0= if exit then
   k LINT-LEX:CONTENT PAYLOAD-WORDS ;

: SCAN-REFS ( -- )
   0 WALK-I !
   begin WALK-I @ LINT-LEX:COUNT < while
      WALK-I @ REF-AT
      WALK-I @ 1+ WALK-I !
   repeat ;

\ The cursor's path is copied out before the file is read, because the scan that
\ follows appends to the very buffer SET-AT pointed into.
: WORK! ( ptr u8 n -- ) {: a:ptr u:n :}
   u FS-PATH-CAP > if E-FS-PATH throw then
   a WORK-BUF u BYTE-COPY
   u WORK-U ! ;

: WORK$ ( -- ptr u8 n )
   WORK-BUF WORK-U @ ;

: REACH-ONE ( -- )
   REACH-SET REACH-I @ SET-AT WORK!
   WORK$ READ-INTO
   WORK$ FILE$ LEX-SOURCE
   SCAN-REFS ;

\ The fork lists are the other half of the roots; the SUITE rows seeded
\ themselves as they were judged. A GSI entry naming a path that is not there
\ seeds nothing, because REACH-TRY requires the file to exist - the reach set
\ answers "what is loaded", and a list can name what is gone.
: SEED-REACH ( -- )
   0 SEED-I !
   begin SEED-I @ SCHED-SET SET-N@ < while
      SCHED-SET SEED-I @ SET-AT REACH-TRY
      SEED-I @ 1+ SEED-I !
   repeat ;

: CLOSE-REACH ( -- )
   0 REACH-I !
   begin REACH-I @ REACH-SET SET-N@ < while
      REACH-ONE
      REACH-I @ 1+ REACH-I !
   repeat ;

\ ---- the disk audit -----------------------------------------------------------
\
\ The other end of the same question. A registration nobody runs is dark; so is a
\ source nobody registered, and only this walk can see the second one, because
\ every other set in this file starts from something that was written down.

: PRAGMA$ ( -- ptr u8 n )
   s" \ schedule-lint: allow-unscheduled" ;

: NL-AT? ( n -- bool ) {: i:n :}
   i FILE-U @ >= if LINT-TRUE exit then
   FILE$ drop i + c@ NL-C = ;

: LINE-END ( n -- n ) {: i:n :}
   i PR-J !
   begin PR-J @ NL-AT? 0= while PR-J @ 1+ PR-J ! repeat
   PR-J @ ;

: LINE$ ( n -- ptr u8 n ) {: i:n :}
   FILE$ drop i +
   i LINE-END i - ;

\ The marker at the head of a comment LINE, after indentation and nothing else.
\ Anchoring it there is what separates a pragma from a mention: this very file
\ names the marker in its own header prose and in PRAGMA$ above, and the lint's
\ fixtures quote it inside string literals - none of those sit at a line head,
\ so none of them excuses anything. A plain substring search over the file would
\ have exempted this file from its own rule.
: PRAGMA-LINE? ( n -- bool ) {: i:n :}
   i LINE$ LINT-LTRIM PRAGMA$ LINT-STARTS-WITH? ;

: PRAGMA? ( -- bool )
   0 PR-I !
   begin PR-I @ FILE-U @ < while
      PR-I @ PRAGMA-LINE? if LINT-TRUE exit then
      PR-I @ LINE-END 1+ PR-I !
   repeat
   LINT-FALSE ;

: DARK-FINDING ( ptr u8 n -- ) {: a:ptr u:n :}
   BAD @ 1+ BAD !
   DARK-N @ 1+ DARK-N !
   REPORT? @ 0= if exit then
   s" schedule-lint: " type a u type
   s"  is on disk and no runner reaches it: no gate fork list, no SUITE row, and nothing scheduled loads it" type cr ;

\ The file is read only for the sources the closure did not reach - a handful -
\ so the pragma scan costs nothing on the 300 that are fine. LIVE has already
\ mapped the scan buffer; this walk is private and runs nowhere else.
: DISK-AT ( ptr u8 n -- ) {: a:ptr u:n :}
   a u s" .f" ENDS-WITH? 0= if exit then
   REACH-SET a u SET-HAS? if exit then
   a u READ-INTO
   PRAGMA? if exit then
   a u DARK-FINDING ;

: DISK-WALK ( ptr u8 n -- ) {: a:ptr u:n :}
   a u [: DISK-AT ;] WALK-FILES ;

\ ---- the phase audit ----------------------------------------------------------
\
\ The second half of the question. Every phase the runner has not retired is a
\ phase something is meant to start; if no order table names it and the deferred
\ table does not excuse it, nothing does. That is not a hypothetical: phase $4
\ ran the tail slice, five registrations were selected for it, and it was in no
\ table at all.

: PHASE-CARRIED ( n -- ) {: i:n :}
   i >IDX TEST:PHASE-SLICE-TOKEN STDLIB-GATE:SLICE-ID? MATCH option
     none OF exit ENDOF
     some OF ENDOF
   ;MATCH
   {: id:n :}
   0 AUDIT-K !
   begin AUDIT-K @ UNFILED-SET SET-N@ < while
      UNFILED-SET AUDIT-K @ SET-AT {: a:ptr u:n :}
      a u id STDLIB-GATE:SLICE-SELECTS? if
         s"   it was the only runner for SUITE " type a u type
         s" , whose files no gate fork list schedules" type cr
      then
      AUDIT-K @ 1+ AUDIT-K !
   repeat ;

: PHASE-FINDING ( n -- ) {: i:n :}
   BAD @ 1+ BAD !
   REPORT? @ 0= if exit then
   s" schedule-lint: phase " type i DEC.
   s"   " type i >IDX TEST:PHASE-LABEL type
   s" : no order table in test/run-lib.f starts it and TR-DEFERRED-ORDER does not excuse it" type cr
   i >IDX TEST:STDLIB-SLICE? 0= if exit then
   i PHASE-CARRIED ;

: PHASE-AUDIT-ONE ( n -- ) {: i:n :}
   i LIVE-PHASE? 0= if exit then
   i STARTED? if exit then
   i DEFERRED? if exit then
   i PHASE-FINDING ;

: PHASE-AUDIT ( -- )
   0 PHASE-I !
   begin PHASE-I @ TEST:PHASES < while
      PHASE-I @ PHASE-AUDIT-ONE
      PHASE-I @ 1+ PHASE-I !
   repeat ;

public

\ Findings print by default. A fixture that provokes them on purpose silences
\ them, so a deliberate finding is not mistaken for a red gate in the log; the
\ count is what it asserts either way.
: REPORT-ON ( -- )
   -1 REPORT? ! ;

: REPORT-OFF ( -- )
   0 REPORT? ! ;

\ Map the scan buffer once per process, separately from RESET so a driver that
\ lints several sources pays for the mapping once.
: PREPARE ( -- )
   PREPARED @ 0 <> if exit then
   FILE-CAP MEM:BYTES-ALLOC-LEN MEM:ALLOC-BYTES drop FILE-A-FIELD !
   -1 PREPARED ! ;

: RESET ( -- )
   0 BAD !
   0 SUITE-N !
   0 HIT-LINE !
   0 HIT-U !
   0 DARK-N !
   0 REACH-I !
   0 LIVE-SLICE-N !
   PHASE-TABLES-RESET
   UNFILED-SET SET-RESET
   SCHED-SET SET-RESET
   REACH-SET SET-RESET ;

\ Read the runner's schedule: which phases an order table starts, which the
\ deferred table excuses, and which slices those phases ask for. Every later
\ verdict is against this, so a driver calls it before it judges anything.
: SCHEDULE ( -- )
   MARK-SCHEDULE
   COLLECT-SLICES ;

\ Record every file a gate fork list schedules in one source.
: SCHED-SRC ( ptr u8 n ptr u8 n -- )
   LEX-SOURCE
   0 WALK-I !
   begin WALK-I @ LINT-LEX:COUNT < while
      WALK-I @ SCHED-AT
      WALK-I @ 1+ WALK-I !
   repeat ;

\ Verdict every registration in one source. Both sets must already be complete:
\ a registration is judged against what the caller has recorded so far.
: CASE-SRC ( ptr u8 n ptr u8 n -- )
   LEX-SOURCE
   0 WALK-I !
   begin WALK-I @ LINT-LEX:COUNT < while
      WALK-I @ SUITE-OPEN? if
         WALK-I @ SUITE-AT
         SCAN-I @ WALK-I !
      then
      WALK-I @ 1+ WALK-I !
   repeat ;

: SCHED-FILE ( ptr u8 n -- ) {: a:ptr u:n :}
   PREPARE
   a u READ-INTO
   a u FILE$ SCHED-SRC ;

: CASE-FILE ( ptr u8 n -- ) {: a:ptr u:n :}
   PREPARE
   a u READ-INTO
   a u FILE$ CASE-SRC ;

\ Only .f sources carry gate bodies; anything else under the root is data.
: SCHED-TREE-FILE ( ptr u8 n -- ) {: a:ptr u:n :}
   a u s" .f" ENDS-WITH? 0= if exit then
   a u SCHED-FILE ;

: SCHED-TREE ( ptr u8 n -- ) {: a:ptr u:n :}
   PREPARE
   a u [: SCHED-TREE-FILE ;] WALK-FILES ;

: FINDINGS ( -- n )
   BAD @ ;

: SUITE# ( -- n )
   SUITE-N @ ;

\ The registration the most recent finding named: its line in the cases source
\ and its label. Zero and empty until one is raised.
: HIT-LINE@ ( -- n )
   HIT-LINE @ ;

: HIT-LABEL$ ( -- ptr u8 n )
   HIT-BUF HIT-U @ ;

: SLICE# ( -- n )
   LIVE-SLICE-N @ ;

: SCHED# ( -- n )
   SCHED-SET SET-N@ ;

\ The closure and the disk audit as separate entries, so a fixture can run the
\ shipped words over a fixture tree instead of re-implementing either. REACH-ROOT
\ seeds one root by hand where the live run seeds them from the fork lists and
\ the SUITE rows; everything after it is the production path.
: REACH-ROOT ( ptr u8 n -- )
   PREPARE
   REACH-TRY ;

: REACH-CLOSE ( -- )
   PREPARE
   CLOSE-REACH ;

: DISK-TREE ( ptr u8 n -- ) {: a:ptr u:n :}
   PREPARE
   a u DISK-WALK ;

\ Sources the closure proved some runner loads, and the .f files under test/ it
\ did not reach. A fixture asserts both: the closure is what keeps the disk
\ audit from reporting support files, so a fixture that only counted findings
\ could not tell a working closure from an empty one.
: REACH# ( -- n )
   REACH-SET SET-N@ ;

: DARK# ( -- n )
   DARK-N @ ;

: REACHED? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   REACH-SET a u SET-HAS? ;

\ The live tree: the real schedule, the real predicate run against it, the real
\ gate bodies, the real registrations. Order matters twice over - the
\ registrations are judged against the schedule and the file lists that came
\ before them, and the closure then needs every root those three produced, as the
\ disk audit needs the closure.
: LIVE ( -- )
   PREPARE
   RESET
   SCHEDULE
   s" test/" SCHED-TREE
   s" test/gate-stdlib-cases.f" CASE-FILE
   SEED-REACH
   CLOSE-REACH
   s" test/" DISK-WALK
   PHASE-AUDIT ;

: SUMMARY ( -- )
   s" schedule-lint: " type
   SUITE# DEC. s"  registration(s), " type
   SLICE# DEC. s"  live slice(s), " type
   SCHED# DEC. s"  scheduled file(s), " type
   REACH# DEC. s"  reached source(s), " type
   DARK# DEC. s"  unreached on disk, " type
   FINDINGS DEC. s"  finding(s)" type cr ;

\ Gate entry: a registration nobody runs, a phase nobody starts, or a test source
\ nobody reaches fails the gate.
: STRICT ( -- )
   LIVE
   SUMMARY
   FINDINGS 0 > if 1 throw then ;

;package
