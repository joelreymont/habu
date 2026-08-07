\ schedule-lint-test.f - fixtures for the suite-scheduling closure, plus the
\ live-tree enforcement.
\ Run: bin/hb --load tools/lint/schedule-lint-test.f
\
\ The fixtures drive the real scanner over SYNTHETIC sources - the same public
\ entry points the live run uses, just handed bytes instead of a path - so what
\ they check is the shipped grammar and not a copy of it. Each source is built to
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

\ ---- the two set sources, shared by every case below -------------------------
\ Selected labels: one real selection, one label in the wrong role, one label
\ that exists only inside a line comment.
: SEL-SRC ( -- ptr u8 n )
   S\" : SEL? ( -- bool )\n   s\q covered-by-slice\q SUITE-LABEL= if TRUE exit then\n   s\q wrong-role-label\q TYPE\n   \\ s\q comment-only-label\q SUITE-LABEL=\n   FALSE ;\n" ;

\ Scheduled files: three real schedulers, one path in the wrong role, one path
\ that exists only inside a line comment.
: SCHED-SRC ( -- ptr u8 n )
   S\" : BODY ( -- )\n   s\q lib/fork-covered.f\q GSI-FORK-INCLUDE\n   s\q lib/half-covered.f\q GSI-INCLUDE\n   s\q lib/setup-covered.f\q GSI-REQUIRE\n   s\q lib/wrong-role.f\q TYPE\n   \\ s\q lib/comment-only.f\q GSI-INCLUDE\n   ;\n" ;

: LOAD-SETS ( -- )
   SCHEDULE-LINT:PREPARE
   SCHEDULE-LINT:REPORT-OFF
   SCHEDULE-LINT:RESET
   ORIGIN$ SEL-SRC SCHEDULE-LINT:SELECT-SRC
   ORIGIN$ SCHED-SRC SCHEDULE-LINT:SCHED-SRC ;

\ typed-local-lint: allow-bare-local - q keeps the source-producing effect from
\ the stack signature.
: CASE-COUNT ( [ -- ptr u8 n ] -- n ) {: q :}
   LOAD-SETS
   ORIGIN$ q execute SCHEDULE-LINT:CASE-SRC
   SCHEDULE-LINT:FINDINGS ;

\ ---- the registration sources ------------------------------------------------

\ Positive control by slice (its file is scheduled nowhere, the label carries
\ it), positive control by fork list (no predicate names the label), and the
\ partial: two files, one scheduled, which MUST report.
: CASES-MIXED ( -- ptr u8 n )
   S\" SUITE covered-by-slice\n   lib/never-scheduled.f\n;SUITE\n\nSUITE covered-by-fork\n   lib/fork-covered.f\n;SUITE\n\nSUITE half-dark\n   lib/half-covered.f\n   lib/dark-partner.f\n;SUITE\n" ;

\ The same three registrations, reordered. Coverage is a property of each
\ registration, so the verdict must not depend on the order they are read in.
: CASES-REORDER ( -- ptr u8 n )
   S\" SUITE half-dark\n   lib/dark-partner.f\n   lib/half-covered.f\n;SUITE\n\nSUITE covered-by-fork\n   lib/fork-covered.f\n;SUITE\n\nSUITE covered-by-slice\n   lib/never-scheduled.f\n;SUITE\n" ;

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
   S\" SUITE s\q covered-by-slice\q\n   lib/dark-e.f\n;SUITE\n" ;

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
   s" only the label in SUITE-LABEL= role is selected" T-LABEL
   LOAD-SETS
   SCHEDULE-LINT:SELECT# 1 T=
   s" only the paths in a GSI role are scheduled" T-LABEL
   SCHEDULE-LINT:SCHED# 3 T= ;

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

\ The live tree, through the same entry the gate runs: every registration in
\ test/gate-stdlib-cases.f must be reachable by a slice predicate or a fork list.
: T-LIVE ( -- )
   SCHEDULE-LINT:REPORT-ON
   SCHEDULE-LINT:STRICT ;

: MAIN ( -- )
   T-RESET
   T-SETS
   T-MIXED
   T-REORDER
   T-DUPLICATE
   T-COMMENT-FOOL
   T-NAME-POS
   T-ARGV-TAIL
   T-STDIN
   T-EMPTY
   T-REFUSALS
   T-REPORT
   T-LIVE ;

MAIN

;package
