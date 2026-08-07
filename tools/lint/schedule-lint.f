\ schedule-lint.f - every SUITE registration must be reachable by a runner.
\
\ THE HOLE THIS CLOSES. test/gate-stdlib-cases.f registers a suite by writing
\ `SUITE <label> <files...> ;SUITE`. Writing it there does not run it. A
\ registration only runs when a slice predicate in test/gate-stdlib-lib.f selects
\ its label, or when its files appear in one of the inline gate bodies' GSI
\ lists. The remaining route - the bare (ALL) slice, which selects every label -
\ is spawned by no phase of test/run.f and never has been. So a registration
\ whose label no predicate names and whose files no list names is DARK: it looks
\ scheduled, it is counted in the suite inventory, and nothing runs it. Fifty
\ three of them were dark when this lint was written, including seven proof
\ parity gates and the clang reference column's fork case.
\
\ WHAT IT CHECKS. A registration is covered when its label is in the selected set
\ OR every one of its files is in the scheduled set. Partial coverage is a
\ finding too: a registration with two files and one of them scheduled is half
\ dark, and the half that is dark is the half nobody notices.
\
\ WHERE THE TWO SETS COME FROM. Both are derived from the real sources; there is
\ no third hand-written list of suites to go stale.
\   selected labels  test/gate-stdlib-lib.f, the shape `s" LABEL" SUITE-LABEL=`
\   scheduled files  every .f under test/, the shape
\                    `s" PATH" GSI-FORK-INCLUDE | GSI-INCLUDE | GSI-REQUIRE`
\ All three inline gate bodies live under test/, so walking test/ reaches every
\ list there is. A GSI list placed OUTSIDE test/ would be invisible here and its
\ registration would report as dark - a false RED, which is the safe direction:
\ it demands an answer instead of quietly granting coverage.
\
\ WHY IT LEXES INSTEAD OF SEARCHING. The question is structural, so the answer
\ has to be. `SUITE` written inside a comment or a string literal is not a
\ registration; `s" ptx-stdlib" TYPE` selects nothing; `: SUITE ( -- ) ... ;`
\ defines the opener rather than opening a registration. Every one of those
\ reads as a hit to a text search. The shared source lexer (package LINT-LEX,
\ tools/lint/source-lex.f) consumes `\` line comments, `( ... )` and `.( ... )`
\ bodies, and every string literal body, and it hands a literal's payload back
\ through CONTENT so the payload is reachable in its ROLE and nowhere else. A
\ lexer diagnostic truncates the token table at the defect, so every later
\ registration in that source would be invisible: this lint rejects on
\ LINT-LEX:ERROR? rather than certify a verdict built from half a file.
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

private

$80000 constant FILE-CAP                \ largest linted source + headroom (test/engine-suite.f is 162K)
$4000 constant LABEL-CAP                \ packed selected labels
$8000 constant SCHED-CAP                \ packed scheduled paths
$100 constant NAME-CAP                  \ one suite label

0 constant SEL-SET                      \ labels a slice predicate selects
1 constant SCHED-SET                    \ files a gate fork list schedules

create LABEL-BUF LABEL-CAP allot        \ packed [len][bytes] runs
create SCHED-BUF SCHED-CAP allot
create NAME-BUF NAME-CAP allot
create ORIGIN-BUF FS-PATH-CAP allot

variable LABEL-U   variable LABEL-N
variable SCHED-U   variable SCHED-N
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
   s SEL-SET = if LABEL-BUF exit then
   SCHED-BUF ;

: SET-CAP@ ( n -- n ) {: s:n :}
   s SEL-SET = if LABEL-CAP exit then
   SCHED-CAP ;

: SET-U@ ( n -- n ) {: s:n :}
   s SEL-SET = if LABEL-U @ exit then
   SCHED-U @ ;

: SET-U! ( n n -- ) {: v:n s:n :}
   s SEL-SET = if v LABEL-U ! exit then
   v SCHED-U ! ;

: SET-N@ ( n -- n ) {: s:n :}
   s SEL-SET = if LABEL-N @ exit then
   SCHED-N @ ;

: SET-N! ( n n -- ) {: v:n s:n :}
   s SEL-SET = if v LABEL-N ! exit then
   v SCHED-N ! ;

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

\ ---- the selected set: `s" LABEL" SUITE-LABEL=` ------------------------------

: SELECT-AT ( n -- ) {: k:n :}
   k LEX-STRING? 0= if exit then
   k 1+ LINT-LEX:COUNT >= if exit then
   k 1+ s" SUITE-LABEL=" LEX-IS 0= if exit then
   SEL-SET k LINT-LEX:CONTENT SET+ ;

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

: MISS-COUNT ( -- )
   CUR-MISS @ 1+ CUR-MISS !
   CUR-MISS @ 1 = if HIT! FINDING-HEAD then ;

: MISS-LINE ( ptr u8 n -- ) {: a:ptr u:n :}
   REPORT? @ 0= if exit then
   s" schedule-lint:   " type a u type
   s"  is in no GSI-FORK-INCLUDE / GSI-INCLUDE / GSI-REQUIRE list under test/" type cr ;

: FILE-CHECK ( ptr u8 n -- ) {: a:ptr u:n :}
   CUR-FILES @ 1+ CUR-FILES !
   SCHED-SET a u SET-HAS? if exit then
   MISS-COUNT
   a u MISS-LINE ;

\ A registration that names no source file at all spawns `hb --load` with
\ nothing to load: it registers a label and runs no code, which is the same
\ emptiness this lint exists to find, reached from the other side.
: EMPTY-CHECK ( -- )
   CUR-FILES @ 0 <> if exit then
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

: BODY-SKIP ( -- )
   begin SCAN-I @ LINT-LEX:COUNT < while
      SCAN-I @ SUITE-CLOSE? if exit then
      SCAN-I @ 1+ SCAN-I !
   repeat
   E-SCHED-SYNTAX throw ;

\ A registration whose label a slice predicate selects is scheduled whatever its
\ files say, so its body is only skipped to the closer.
: VERDICT ( -- )
   0 CUR-MISS !
   SEL-SET NAME$ SET-HAS? if BODY-SKIP exit then
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
   SEL-SET SET-RESET
   SCHED-SET SET-RESET ;

\ Record every label a slice predicate selects in one source.
: SELECT-SRC ( ptr u8 n ptr u8 n -- )
   LEX-SOURCE
   0 WALK-I !
   begin WALK-I @ LINT-LEX:COUNT < while
      WALK-I @ SELECT-AT
      WALK-I @ 1+ WALK-I !
   repeat ;

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

: SELECT-FILE ( ptr u8 n -- ) {: a:ptr u:n :}
   PREPARE
   a u READ-INTO
   a u FILE$ SELECT-SRC ;

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

: SELECT# ( -- n )
   SEL-SET SET-N@ ;

: SCHED# ( -- n )
   SCHED-SET SET-N@ ;

\ The live tree: the real predicates, the real gate bodies, the real
\ registrations.
: LIVE ( -- )
   PREPARE
   RESET
   s" test/gate-stdlib-lib.f" SELECT-FILE
   s" test/" SCHED-TREE
   s" test/gate-stdlib-cases.f" CASE-FILE ;

: SUMMARY ( -- )
   s" schedule-lint: " type
   SUITE# DEC. s"  registration(s), " type
   SELECT# DEC. s"  selected label(s), " type
   SCHED# DEC. s"  scheduled file(s), " type
   FINDINGS DEC. s"  finding(s)" type cr ;

\ Gate entry: a registration nobody runs fails the gate.
: STRICT ( -- )
   LIVE
   SUMMARY
   FINDINGS 0 > if 1 throw then ;

;package
