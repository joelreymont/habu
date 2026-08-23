\ schedule-lint-test.f - fixtures for the suite-scheduling closure, plus the
\ live-tree enforcement.
\ Run: bin/hb --load tools/lint/schedule-lint-test.f
\
\ The fixtures drive the real scanner over SYNTHETIC registration sources - the
\ same public entry points the live run uses, just handed bytes instead of a path
\ - so what they check is the shipped grammar and not a copy of it. The two
\ things a registration is judged against are NOT synthetic: the slice side is
\ the live runner's phase tables put through the gate's own predicate, so the
\ label a fixture uses to stand for "covered by a slice" is a label the tail
\ slice really selects today. Only the fork-list side is synthetic, because a
\ fixture has to name files that do not exist. Each source is built to
\ fool a text matcher: the label that exists only inside a `\` comment, the label
\ inside a string literal standing in the wrong role, the definition of the
\ opener word itself, the argv tail after `--` that looks like a filename, the
\ stdin datum that looks like one too, and a closer whose case is wrong. A
\ scanner that answered by searching text would pass some of these by accident
\ and fail the rest; the counts and the NAMED finding below pin which.

require lib/errors.f
require lib/string.f
require lib/test.f
require tools/lint/schedule-lint.f

package SCHEDULE-LINT-TEST

private

: ORIGIN$ ( -- ptr u8 n )
   s" fixture-cases.f" ;

\ ---- what the cases are judged against, shared by every case below -----------
\ A label the live tail slice really selects, and one no predicate names. If the
\ first ever stops being selected this fixture stops standing for anything, which
\ is why it is asserted directly below rather than assumed.
: COVERED-LABEL$ ( -- ptr u8 n )
   s" pointer-storage" ;

: DARK-LABEL$ ( -- ptr u8 n )
   s" no-slice-names-this-label" ;

\ Two labels a real predicate really does select, for slices no runner asks for.
\ `clobber-lint` is selected by SUITE-LINT-TOOLS-LABEL? for the lint-tools slice,
\ which only RESIDENT phases name (test/run-lib.f $11, and $17 which is deferred
\ besides); `string-helpers` is selected by SUITE-LINT-LIBS-LABEL? for lint-libs,
\ whose own phase $13 sits in the deferred table and whose splits $1E..$21 are
\ resident. Neither slice can run a registration: a resident phase forks
\ test/run-worker-stdlib.f, which reads the GSI inline bodies and never
\ test/gate-stdlib-cases.f. Each premise is ASSERTED below, not assumed, so a
\ label that stopped being selected fails the fixture instead of quietly making
\ it stand for nothing.
: RESIDENT-LABEL$ ( -- ptr u8 n )
   s" clobber-lint" ;

: DEFERRED-LABEL$ ( -- ptr u8 n )
   s" string-helpers" ;

\ The gate's own token -> slice map, refused rather than guessed at: a fixture
\ asking about a slice this gate does not have would assert nothing.
: SLICE-ID ( ptr u8 n -- n )
   STDLIB-GATE:SLICE-ID? MATCH option
     none OF E-STR-BOUNDS throw ENDOF
     some OF ENDOF
   ;MATCH ;

\ Scheduled files: three real schedulers, one path in the wrong role, one path
\ that exists only inside a line comment.
: SCHED-SRC ( -- ptr u8 n )
   S\" : BODY ( -- )\n   s\q lib/fork-covered.f\q GSI-FORK-INCLUDE\n   s\q lib/half-covered.f\q GSI-INCLUDE\n   s\q lib/setup-covered.f\q GSI-REQUIRE\n   s\q lib/wrong-role.f\q TYPE\n   \\ s\q lib/comment-only.f\q GSI-INCLUDE\n   ;\n" ;

: LOAD-SETS ( -- )
   SCHEDULE-LINT:PREPARE
   SCHEDULE-LINT:REPORT-OFF
   SCHEDULE-LINT:RESET
   SCHEDULE-LINT:SCHEDULE
   ORIGIN$ SCHED-SRC SCHEDULE-LINT:SCHED-SRC ;

\ typed-local-lint: allow-bare-local - q keeps the source-producing effect from
\ the stack signature.
: CASE-COUNT ( [ -- ptr u8 n ] -- n ) {: q :}
   LOAD-SETS
   ORIGIN$ q execute SCHEDULE-LINT:CASE-SRC
   SCHEDULE-LINT:FINDINGS ;

\ ---- the registration sources ------------------------------------------------

\ Positive control by slice (its file is scheduled nowhere, the label carries it
\ because the live tail slice selects it), positive control by fork list (no
\ predicate names the label), and the partial: two files, one scheduled, which
\ MUST report.
: CASES-MIXED ( -- ptr u8 n )
   S\" SUITE pointer-storage\n   lib/never-scheduled.f\n;SUITE\n\nSUITE covered-by-fork\n   lib/fork-covered.f\n;SUITE\n\nSUITE half-dark\n   lib/half-covered.f\n   lib/dark-partner.f\n;SUITE\n" ;

\ The same three registrations, reordered. Coverage is a property of each
\ registration, so the verdict must not depend on the order they are read in.
: CASES-REORDER ( -- ptr u8 n )
   S\" SUITE half-dark\n   lib/dark-partner.f\n   lib/half-covered.f\n;SUITE\n\nSUITE covered-by-fork\n   lib/fork-covered.f\n;SUITE\n\nSUITE pointer-storage\n   lib/never-scheduled.f\n;SUITE\n" ;

\ Two registrations under one label. Each is judged on its own files, so a dark
\ duplicate is a finding of its own and not absorbed by its twin.
: CASES-DUPLICATE ( -- ptr u8 n )
   S\" SUITE dark-twice\n   lib/dark-a.f\n;SUITE\n\nSUITE dark-twice\n   lib/dark-b.f\n;SUITE\n" ;

\ Built to fool a text matcher: an opener in a line comment, an opener inside a
\ string literal, and a label in a string literal - none of them registrations.
: CASES-COMMENT-FOOL ( -- ptr u8 n )
   S\" \\ SUITE comment-only-suite\n\\    lib/dark-a.f\n\\ ;SUITE\ns\q SUITE string-only-suite lib/dark-b.f ;SUITE\q TYPE\nSUITE really-dark\n   lib/dark-c.f\n;SUITE\n" ;

\ The opener in NAME position: after `:` or `'` the engine parses the next word
\ as a name and never executes it, so lib/test/suite.f's own definition of SUITE
\ opens no registration.
: CASES-NAME-POS ( -- ptr u8 n )
   S\" : SUITE ( -- ) parse-name drop ;\n' SUITE drop\nSUITE actually-dark\n   lib/dark-d.f\n;SUITE\n" ;

\ Everything after the first `--` is script argv, so `file.f` there is an
\ argument and not a member. A scanner that collected every .f-looking token
\ would report this registration.
: CASES-ARGV-TAIL ( -- ptr u8 n )
   S\" SUITE argv-suite\n   lib/fork-covered.f -- --json --label NAME -o OUT -- file.f --literal\n;SUITE\n" ;

\ SUITE-STDIN parses a name AND a stdin datum before its argv. The datum here is
\ spelled like a path on purpose: reading it as a member would report.
: CASES-STDIN ( -- ptr u8 n )
   S\" SUITE-STDIN stdin-suite lib/not-a-member.f\n   lib/fork-covered.f -- stdin\n;SUITE\n" ;

\ A registration that names no source at all: it registers a label and runs
\ nothing.
: CASES-EMPTY ( -- ptr u8 n )
   S\" SUITE empty-suite\n;SUITE\n" ;

\ A registration under a label a live predicate selects, for a slice no started
\ non-resident phase asks for. Every text-level fact about it reads as
\ scheduled - the label is in a predicate, the predicate is in a live slice -
\ and nothing runs it.
: CASES-RESIDENT-ONLY ( -- ptr u8 n )
   S\" SUITE clobber-lint\n   lib/never-scheduled.f\n;SUITE\n" ;

: CASES-DEFERRED-ONLY ( -- ptr u8 n )
   S\" SUITE string-helpers\n   lib/never-scheduled.f\n;SUITE\n" ;

\ ---- sources the scanner must refuse -----------------------------------------

: CASES-NO-CLOSER ( -- ptr u8 n )
   S\" SUITE unterminated\n   lib/fork-covered.f\n" ;

\ suite.f compares its closer with STR=, so `;suite` is an ordinary argv entry
\ there and this registration never closes.
: CASES-LOWER-CLOSER ( -- ptr u8 n )
   S\" SUITE lower-closer\n   lib/fork-covered.f\n;suite\n" ;

: CASES-NO-LABEL ( -- ptr u8 n )
   S\" SUITE\n;SUITE\n" ;

\ A quoted label is not a label: the engine's parse-name would take the opener
\ text `s\q` as the name and then read the payload as an argument, which this
\ lint declines to model rather than guess at.
: CASES-QUOTED-LABEL ( -- ptr u8 n )
   S\" SUITE s\q pointer-storage\q\n   lib/dark-e.f\n;SUITE\n" ;

\ A string literal that runs past end of input truncates the token table, so
\ every registration after it is invisible.
: CASES-BAD-QUOTE ( -- ptr u8 n )
   S\" SUITE fine\n   lib/fork-covered.f\n;SUITE\ns\q oops\n" ;

: RUN-NO-CLOSER ( -- )
   [: CASES-NO-CLOSER ;] CASE-COUNT drop ;

: RUN-LOWER-CLOSER ( -- )
   [: CASES-LOWER-CLOSER ;] CASE-COUNT drop ;

: RUN-NO-LABEL ( -- )
   [: CASES-NO-LABEL ;] CASE-COUNT drop ;

: RUN-QUOTED-LABEL ( -- )
   [: CASES-QUOTED-LABEL ;] CASE-COUNT drop ;

: RUN-BAD-QUOTE ( -- )
   [: CASES-BAD-QUOTE ;] CASE-COUNT drop ;

\ ---- the checks --------------------------------------------------------------

: T-SETS ( -- )
   s" only the paths in a GSI role are scheduled" T-LABEL
   LOAD-SETS
   SCHEDULE-LINT:SCHED# 3 T=
   s" the live runner asks for at least one slice" T-LABEL
   SCHEDULE-LINT:SLICE# 0 > TTRUE
   s" a label one of those slices selects is covered" T-LABEL
   COVERED-LABEL$ SCHEDULE-LINT:LABEL-COVER? TTRUE
   s" a label no predicate names is not" T-LABEL
   DARK-LABEL$ SCHEDULE-LINT:LABEL-COVER? TFALSE ;

\ The residency half, which no count above can reach. A slice whose predicate
\ selects the label is not a runner unless a STARTED, NON-RESIDENT phase asks for
\ it, so each case asserts the predicate first and the verdict second: the pair
\ is the whole claim. Deleting either test from SCHEDULE-LINT's SLICE-AT turns
\ both verdicts green while both premises stay true, which is exactly the state
\ this file exists to refuse.
: T-RESIDENT-ONLY ( -- )
   LOAD-SETS
   s" the lint-tools predicate really does select the resident-only label" T-LABEL
   RESIDENT-LABEL$ s" lint-tools" SLICE-ID STDLIB-GATE:SLICE-SELECTS? TTRUE
   s" but no started non-resident phase asks for the lint-tools slice" T-LABEL
   RESIDENT-LABEL$ SCHEDULE-LINT:LABEL-COVER? TFALSE
   s" so a registration under it reports" T-LABEL
   [: CASES-RESIDENT-ONLY ;] CASE-COUNT 1 T=
   SCHEDULE-LINT:HIT-LABEL$ RESIDENT-LABEL$ T$=
   s" the lint-libs predicate really does select the deferred-only label" T-LABEL
   DEFERRED-LABEL$ s" lint-libs" SLICE-ID STDLIB-GATE:SLICE-SELECTS? TTRUE
   s" but the phase that asks for lint-libs is deferred, not started" T-LABEL
   DEFERRED-LABEL$ SCHEDULE-LINT:LABEL-COVER? TFALSE
   s" so a registration under it reports too" T-LABEL
   [: CASES-DEFERRED-ONLY ;] CASE-COUNT 1 T=
   SCHEDULE-LINT:HIT-LABEL$ DEFERRED-LABEL$ T$= ;

: T-MIXED ( -- )
   s" one partial registration reports, the two covered ones do not" T-LABEL
   [: CASES-MIXED ;] CASE-COUNT 1 T=
   s" three registrations were read" T-LABEL
   SCHEDULE-LINT:SUITE# 3 T=
   s" the finding names the partial registration" T-LABEL
   SCHEDULE-LINT:HIT-LABEL$ s" half-dark" T$=
   s" and the line it stands on" T-LABEL
   SCHEDULE-LINT:HIT-LINE@ 9 T= ;

: T-REORDER ( -- )
   s" reordering the registrations does not change the verdict" T-LABEL
   [: CASES-REORDER ;] CASE-COUNT 1 T=
   SCHEDULE-LINT:HIT-LABEL$ s" half-dark" T$=
   s" the finding follows the registration to line 1" T-LABEL
   SCHEDULE-LINT:HIT-LINE@ 1 T= ;

: T-DUPLICATE ( -- )
   s" each dark registration under a shared label reports" T-LABEL
   [: CASES-DUPLICATE ;] CASE-COUNT 2 T=
   SCHEDULE-LINT:SUITE# 2 T= ;

: T-COMMENT-FOOL ( -- )
   s" an opener in a comment or a string literal registers nothing" T-LABEL
   [: CASES-COMMENT-FOOL ;] CASE-COUNT 1 T=
   SCHEDULE-LINT:SUITE# 1 T=
   SCHEDULE-LINT:HIT-LABEL$ s" really-dark" T$= ;

: T-NAME-POS ( -- )
   s" the opener in name position defines a word, it opens nothing" T-LABEL
   [: CASES-NAME-POS ;] CASE-COUNT 1 T=
   SCHEDULE-LINT:SUITE# 1 T=
   SCHEDULE-LINT:HIT-LABEL$ s" actually-dark" T$= ;

: T-ARGV-TAIL ( -- )
   s" tokens after -- are script argv, not members" T-LABEL
   [: CASES-ARGV-TAIL ;] CASE-COUNT 0 T= ;

: T-STDIN ( -- )
   s" the SUITE-STDIN datum is not a member" T-LABEL
   [: CASES-STDIN ;] CASE-COUNT 0 T= ;

: T-EMPTY ( -- )
   s" a registration that names no source reports" T-LABEL
   [: CASES-EMPTY ;] CASE-COUNT 1 T=
   SCHEDULE-LINT:HIT-LABEL$ s" empty-suite" T$= ;

: T-REFUSALS ( -- )
   s" a registration with no closer is refused" T-LABEL
   [: RUN-NO-CLOSER ;] SCHEDULE-LINT:E-SCHED-SYNTAX TTHROWSQ
   s" a lower-case closer closes nothing, so the registration is refused" T-LABEL
   [: RUN-LOWER-CLOSER ;] SCHEDULE-LINT:E-SCHED-SYNTAX TTHROWSQ
   s" an opener with no label is refused" T-LABEL
   [: RUN-NO-LABEL ;] SCHEDULE-LINT:E-SCHED-SYNTAX TTHROWSQ
   s" a quoted label is refused rather than read out of the literal" T-LABEL
   [: RUN-QUOTED-LABEL ;] SCHEDULE-LINT:E-SCHED-SYNTAX TTHROWSQ
   s" an unterminated string literal is refused, not scanned around" T-LABEL
   [: RUN-BAD-QUOTE ;] SCHEDULE-LINT:E-SCHED-QUOTE TTHROWSQ ;

\ ---- the disk audit ----------------------------------------------------------
\
\ The fixture tree under tools/lint/sched-fixture/ is REAL FILES, because the
\ audit's question is about the disk and synthetic bytes cannot answer it. The
\ fixture seeds one root and then runs the shipped closure and the shipped walk -
\ REACH-ROOT, REACH-CLOSE, DISK-TREE are the words LIVE itself calls - so what is
\ under test is the production reference grammar, not a model of it.
\
\ Every file in that tree is built to be judged wrongly by a text search. Three
\ are reached and must stay silent, and each uses a different reference shape: a
\ `require` directive, a bare literal, and a path that is one WORD of a longer
\ literal. Three must report: one named only inside a `\` comment, one named only
\ by a literal in a file that is itself unreached, and one carrying the pragma
\ text in three places that are not a comment-line head. One must stay silent on
\ the strength of a real pragma. Mutating the lint to search text instead of
\ walking the closure flips the first three; mutating the pragma test to a
\ substring search flips the last two.
: FIXTURE-ROOT$ ( -- ptr u8 n )
   s" tools/lint/sched-fixture/root.f" ;

: FIXTURE-TREE$ ( -- ptr u8 n )
   s" tools/lint/sched-fixture/" ;

: FIXTURE-RUN ( -- n )
   SCHEDULE-LINT:PREPARE
   SCHEDULE-LINT:REPORT-OFF
   SCHEDULE-LINT:RESET
   FIXTURE-ROOT$ SCHEDULE-LINT:REACH-ROOT
   SCHEDULE-LINT:REACH-CLOSE
   FIXTURE-TREE$ SCHEDULE-LINT:DISK-TREE
   SCHEDULE-LINT:DARK# ;

: T-REACH ( -- )
   s" the three reaching shapes each carry one neighbour into the closure" T-LABEL
   FIXTURE-RUN drop
   s" a require directive reaches its file" T-LABEL
   s" tools/lint/sched-fixture/linked.f" SCHEDULE-LINT:REACHED? TTRUE
   s" a bare literal reaches the file a spawner would be handed" T-LABEL
   s" tools/lint/sched-fixture/spawned.f" SCHEDULE-LINT:REACHED? TTRUE
   s" a path that is one word of a longer literal is reached too" T-LABEL
   s" tools/lint/sched-fixture/embedded.f" SCHEDULE-LINT:REACHED? TTRUE
   s" a path named only in a line comment is not reached" T-LABEL
   s" tools/lint/sched-fixture/dangling.f" SCHEDULE-LINT:REACHED? TFALSE
   s" a literal naming a file that is not on disk reaches nothing" T-LABEL
   s" tools/lint/sched-fixture/no-such-file.f" SCHEDULE-LINT:REACHED? TFALSE
   s" reach does not flow out of an unreached file" T-LABEL
   s" tools/lint/sched-fixture/orphan-ref.f" SCHEDULE-LINT:REACHED? TFALSE
   s" the root and its three reachable neighbours, and nothing else" T-LABEL
   SCHEDULE-LINT:REACH# 4 T= ;

\ Four of the tree's files are unreached and three of them report, so the fourth
\ is the pragma's work. The REACHED? clause is what makes that inference sound:
\ without it, excused.f could be silent because something reached it, and the
\ count would read the same.
: T-DISK ( -- )
   s" three unreached sources report and the excused one does not" T-LABEL
   FIXTURE-RUN 3 T=
   s" every finding is a disk finding" T-LABEL
   SCHEDULE-LINT:FINDINGS 3 T=
   s" and the excused file is silent on its pragma, not on being reached" T-LABEL
   s" tools/lint/sched-fixture/excused.f" SCHEDULE-LINT:REACHED? TFALSE ;

\ The live tree, through the same entry the gate runs: every registration in
\ test/gate-stdlib-cases.f must be reachable by a slice predicate or a fork list,
\ and every .f under test/ must be reached by some runner.
: T-LIVE ( -- )
   SCHEDULE-LINT:REPORT-ON
   SCHEDULE-LINT:STRICT ;

: MAIN ( -- )
   T-RESET
   T-SETS
   T-RESIDENT-ONLY
   T-MIXED
   T-REORDER
   T-DUPLICATE
   T-COMMENT-FOOL
   T-NAME-POS
   T-ARGV-TAIL
   T-STDIN
   T-EMPTY
   T-REFUSALS
   T-REACH
   T-DISK
   T-REPORT
   T-LIVE ;

MAIN

;package
