\ stale-status-lint-test.f - checked fixtures for tools/stale-status-lint.f.
\ Run: bin/hb --load tools/stale-status-lint-test.f
\ The require list below is the load path; naming the same libraries again on
\ the command line loads them twice and dies with a duplicate definition.

require lib/date.f
require lib/errors.f
require lib/string.f
require lib/test.f
require lib/memory.f
require lib/fs.f
require lib/fs-mutate.f
require lib/process.f
require tools/lint/text.f
require tools/lint/token.f
require tools/lint/lib.f
require lib/argv.f
require tools/stale-status-lint-core.f

package STALE-STATUS-LINT-TEST
private

4096 constant CAP
32 constant DATE-CAP
1050 constant LONG-LINES
10 constant LF-C

variable ROOT-U
variable STATUS-U
variable LESSONS-U
variable README-U
variable JJ-U
variable JJ-DIR-U
variable MAKI-DIR-U
variable MAKI-STATUS-U
variable TODAY-U
variable SUB-U
variable COUNT

create ROOT-BUF FS-PATH-CAP allot
create STATUS-BUF FS-PATH-CAP allot
create LESSONS-BUF FS-PATH-CAP allot
create README-BUF FS-PATH-CAP allot
create JJ-BUF FS-PATH-CAP allot
create JJ-DIR-BUF FS-PATH-CAP allot
create MAKI-DIR-BUF FS-PATH-CAP allot
create MAKI-STATUS-BUF FS-PATH-CAP allot
create TODAY-BUF DATE-CAP allot
create SUB-BUF FS-PATH-CAP allot
create OUT-BUF CAP allot
create ERR-BUF CAP allot

: COPY! ( ptr u8 n ptr u8 ptr n -- ) {: a:ptr u:n dst:ptr lenp:ptr :}
   u FS-PATH-CAP > if E-FS-PATH throw then
   a dst u BYTE-COPY
   u lenp ! ;

: PATH! ( ptr u8 n ptr u8 n ptr u8 ptr n -- ) {: pa:ptr pu:n na:ptr nu:n dst:ptr lenp:ptr :}
   pa pu na nu dst JOIN-PATH lenp ! ;

: ROOT ( -- ptr u8 n )
   ROOT-BUF ROOT-U @ ;

: STATUS-MD ( -- ptr u8 n )
   STATUS-BUF STATUS-U @ ;

: LESSONS-MD ( -- ptr u8 n )
   LESSONS-BUF LESSONS-U @ ;

: README-MD ( -- ptr u8 n )
   README-BUF README-U @ ;

: JJ-STATUS ( -- ptr u8 n )
   JJ-BUF JJ-U @ ;

: JJ-DIR ( -- ptr u8 n )
   JJ-DIR-BUF JJ-DIR-U @ ;

: MAKI-DIR ( -- ptr u8 n )
   MAKI-DIR-BUF MAKI-DIR-U @ ;

: MAKI-STATUS ( -- ptr u8 n )
   MAKI-STATUS-BUF MAKI-STATUS-U @ ;

: TODAY-STR ( -- ptr u8 n )
   TODAY-BUF TODAY-U @ ;

: SUB$ ( ptr u8 n -- ptr u8 n ) {: a:ptr u:n :}   \ path under the fixture root
   ROOT a u SUB-BUF JOIN-PATH SUB-U !
   SUB-BUF SUB-U @ ;

: SUB-MKDIRS ( ptr u8 n -- )
   SUB$ MAKE-DIRS ;

: SUB-WRITE ( ptr u8 n ptr u8 n -- ) {: p:ptr pu:n body:ptr bodyu:n :}
   p pu SUB$ body bodyu WRITE-ALL ;

: SUB-RM ( ptr u8 n -- )
   SUB$ REMOVE-TREE ;

: +NL ( -- )
   LF-C SB-APPEND-C ;

: EMPTY$ ( -- ptr u8 n )
   SB-RESET
   SB$ ;

: STATUS$ ( ptr u8 n -- ptr u8 n ) {: date:ptr dateu:n :}
   SB-RESET
   s" # Status" SB-APPEND +NL
   +NL
   s" Last verified: " SB-APPEND date dateu SB-APPEND +NL
   s" Certified: 987  Uncheckable: 0  Rejected: 0" SB-APPEND +NL
   SB$ ;

: LESSONS$ ( -- ptr u8 n )
   SB-RESET
   s" Historical 783/0/0 count is allowed here." SB-APPEND +NL
   SB$ ;

: GOOD$ ( -- ptr u8 n )
   SB-RESET
   s" No live count here." SB-APPEND +NL
   SB$ ;

: MAKI-STATUS$ ( -- ptr u8 n )
   SB-RESET
   s" Maki can say 987 certified here because it is fenced from root status lint." SB-APPEND +NL
   SB$ ;

: COUNT$ ( -- ptr u8 n )
   SB-RESET
   s" This stale count says 890 certified in prose." SB-APPEND +NL
   SB$ ;

: TRIPLE$ ( -- ptr u8 n )
   SB-RESET
   s" This stale count says 890/0/0 in prose." SB-APPEND +NL
   SB$ ;

: UNCHECKABLE$ ( -- ptr u8 n )
   SB-RESET
   s" This stale count says 890 uncheckable in prose." SB-APPEND +NL
   SB$ ;

: UPPER-COUNT$ ( -- ptr u8 n )
   SB-RESET
   s" This stale count says 890 CERTIFIED in prose." SB-APPEND +NL
   SB$ ;

: SHORT-COUNT$ ( -- ptr u8 n )
   SB-RESET
   s" This short count says 89 certified in prose." SB-APPEND +NL
   SB$ ;

: EMBEDDED-COUNT$ ( -- ptr u8 n )
   SB-RESET
   s" This embedded count says A890 certified in prose." SB-APPEND +NL
   SB$ ;

: PARTIAL-RATIO$ ( -- ptr u8 n )
   SB-RESET
   s" This partial ratio says 890/0x in prose." SB-APPEND +NL
   SB$ ;

: FENCE$ ( -- ptr u8 n )
   SB-RESET
   s" ```text" SB-APPEND +NL
   s" llm-results: rows=290 certified=195 tests=210" SB-APPEND +NL
   s" ```" SB-APPEND +NL
   SB$ ;

: BOARD$ ( -- ptr u8 n )   \ how a blackboard message quotes one past gate run
   SB-RESET
   s" Gate run on d52cf52a: 890 certified, 0 uncheckable." SB-APPEND +NL
   SB$ ;

: WRITE-LONG-README ( -- )
   README-MD EMPTY$ WRITE-ALL
   0 begin dup LONG-LINES < while
      README-MD GOOD$ APPEND-FILE
      1+
   repeat drop ;

: GOOD-OUT$ ( -- ptr u8 n )
   SB-RESET
   s" stale-status-lint: 0 finding(s)" SB-APPEND +NL
   SB$ ;

: WRITE-STATUS ( ptr u8 n -- )
   STATUS-MD 2swap STATUS$ WRITE-ALL ;

: RESET-FILES ( -- )
   s" 2026-06-16" WRITE-STATUS
   LESSONS-MD LESSONS$ WRITE-ALL
   README-MD GOOD$ WRITE-ALL ;

: PREPARE ( -- )
   CLEANUP-RESET
   s" habu-stale-status" TMPDIR-MKDIR {: a:ptr u:n :}
   a u ROOT-BUF ROOT-U COPY!
   ROOT CLEANUP-TREE+
   ROOT s" STATUS.md" STATUS-BUF STATUS-U PATH!
   ROOT s" LESSONS.md" LESSONS-BUF LESSONS-U PATH!
   ROOT s" README.md" README-BUF README-U PATH!
   ROOT s" .jj-ws/master-test" JJ-DIR-BUF JJ-DIR-U PATH!
   ROOT s" .jj-ws/master-test/STATUS.md" JJ-BUF JJ-U PATH!
   ROOT s" maki" MAKI-DIR-BUF MAKI-DIR-U PATH!
   ROOT s" maki/STATUS.md" MAKI-STATUS-BUF MAKI-STATUS-U PATH!
   RESET-FILES ;

: TODAY-STR! ( ptr u8 n -- ) {: a:ptr u:n :}
   u DATE-CAP > if E-FS-PATH throw then
   a TODAY-BUF u BYTE-COPY
   u TODAY-U ! ;

: RUN-ACT ( -- )
   ROOT STALE-STATUS-LINT:ROOT!
   TODAY-STR STALE-STATUS-LINT:PARSE-TODAY STALE-STATUS-LINT:TODAY!
   STALE-STATUS-LINT:RUN dup COUNT !
   0 > IF 1 throw THEN ;

: CAPTURE ( ptr u8 n -- n n n )
   TODAY-STR!
   0 COUNT !
   OUT-BUF CAP STALE-STATUS-LINT:OUT-BUFFER!
   ERR-BUF CAP STALE-STATUS-LINT:ERR-BUFFER!
   [: RUN-ACT ;] catch {: rc:n :}
   STALE-STATUS-LINT:OUT$ nip STALE-STATUS-LINT:ERR$ nip rc
   STALE-STATUS-LINT:BUFFERS-OFF ;

: CAPTURE-DEFAULT ( -- n n n )
   s" 2026-06-16" CAPTURE ;

: EXPECT-EXIT ( n n n n -- n n ) {: outu:n erru:n code:n expect:n :}
   code expect T=
   outu erru ;

: EXPECT-EXIT-NZ ( n n n -- n n ) {: outu:n erru:n code:n :}
   code 0 T<>
   outu erru ;

: EXPECT-CLEAN ( -- )   \ no findings, and the run reported the count as zero
   CAPTURE-DEFAULT 0 EXPECT-EXIT {: outu:n erru:n :}
   OUT-BUF outu GOOD-OUT$ T$=
   ERR-BUF erru EMPTY$ T$=
   COUNT @ 0 T= ;

: EXPECT-DIRTY ( -- )   \ at least one finding, reported through the count
   CAPTURE-DEFAULT EXPECT-EXIT-NZ {: outu:n erru:n :}
   outu 0 T<>
   erru 0 T=
   COUNT @ 0 > TTRUE ;

: EXPECT-OK ( -- )
   CAPTURE-DEFAULT 0 EXPECT-EXIT {: outu:n erru:n :}
   OUT-BUF outu GOOD-OUT$ T$=
   ERR-BUF erru EMPTY$ T$= ;

: EXPECT-BAD-TODAY ( ptr u8 n ptr u8 n ptr u8 n -- ) {: today:ptr todayu:n code:ptr codeu:n needle:ptr needleu:n :}
   today todayu CAPTURE EXPECT-EXIT-NZ {: outu:n erru:n :}
   erru 0 T=
   OUT-BUF outu code codeu CONTAINS? TTRUE
   needleu 0 > if OUT-BUF outu needle needleu CONTAINS? TTRUE then ;

: EXPECT-BAD ( ptr u8 n ptr u8 n -- ) {: code:ptr codeu:n needle:ptr needleu:n :}
   s" 2026-06-16" code codeu needle needleu EXPECT-BAD-TODAY ;

: EXPECT-ONE ( ptr u8 n -- ) {: needle:ptr needleu:n :}   \ exactly one finding, at this path and line
   s" STALE-STATUS" needle needleu EXPECT-BAD
   COUNT @ 1 T= ;

: TEST-CLEAN ( -- )
   RESET-FILES
   EXPECT-OK ;

: TEST-STALE-DATE ( -- )
   RESET-FILES
   s" 2026-06-15" WRITE-STATUS
   s" STALE-STATUS" s" Last verified is 2026-06-15, expected 2026-06-16" EXPECT-BAD ;

: TEST-BAD-STATUS-DATE ( -- )
   RESET-FILES
   s" 2026-02-29" WRITE-STATUS
   EXPECT-DIRTY ;

: TEST-BAD-TODAY ( -- )
   RESET-FILES
   s" 2026-02-29" s" BAD-TODAY" s" today argument invalid `2026-02-29`" EXPECT-BAD-TODAY ;

: TEST-COUNT-PROSE ( -- )
   RESET-FILES
   README-MD COUNT$ WRITE-ALL
   s" STALE-STATUS" s" README.md:1: count-shaped string" EXPECT-BAD ;

: TEST-COUNT-TRIPLE ( -- )
   RESET-FILES
   README-MD TRIPLE$ WRITE-ALL
   EXPECT-DIRTY ;

: TEST-COUNT-UNCHECKABLE ( -- )
   RESET-FILES
   README-MD UNCHECKABLE$ WRITE-ALL
   EXPECT-DIRTY ;

: TEST-COUNT-UPPERCASE ( -- )
   RESET-FILES
   README-MD UPPER-COUNT$ WRITE-ALL
   EXPECT-DIRTY ;

: TEST-SHORT-COUNT ( -- )
   RESET-FILES
   README-MD SHORT-COUNT$ WRITE-ALL
   EXPECT-CLEAN ;

: TEST-EMBEDDED-COUNT ( -- )
   RESET-FILES
   README-MD EMBEDDED-COUNT$ WRITE-ALL
   EXPECT-CLEAN ;

: TEST-PARTIAL-RATIO ( -- )
   RESET-FILES
   README-MD PARTIAL-RATIO$ WRITE-ALL
   EXPECT-CLEAN ;

: TEST-FENCED-COUNTS ( -- )
   RESET-FILES
   README-MD FENCE$ WRITE-ALL
   EXPECT-CLEAN ;

: TEST-LONG-MARKDOWN ( -- )
   RESET-FILES
   WRITE-LONG-README
   EXPECT-CLEAN ;

: TEST-SKIP-JJ-WS ( -- )
   RESET-FILES
   JJ-DIR MAKE-DIRS
   JJ-STATUS s" 890 certified" WRITE-ALL
   EXPECT-CLEAN ;

: TEST-SKIP-MAKI ( -- )
   RESET-FILES
   MAKI-DIR MAKE-DIRS
   MAKI-STATUS MAKI-STATUS$ WRITE-ALL
   EXPECT-CLEAN ;

: TEST-SKIP-BLACKBOARD ( -- )   \ untracked coordination history is out of scope
   RESET-FILES
   s" .blackboard/channels/general" SUB-MKDIRS
   s" .blackboard/channels/general/msg.md" BOARD$ SUB-WRITE
   EXPECT-CLEAN
   s" .blackboard" SUB-RM ;

: TEST-TRACKED-DOC-COUNT ( -- )   \ the same bytes in a tracked doc still flag
   RESET-FILES
   s" docs" SUB-MKDIRS
   s" docs/notes.md" BOARD$ SUB-WRITE
   s" docs/notes.md:1: count-shaped string" EXPECT-ONE
   s" docs" SUB-RM ;

: TEST-BLACKBOARD-NAME-COUNT ( -- )   \ a file merely named like the board is scanned
   RESET-FILES
   s" blackboard.md" BOARD$ SUB-WRITE
   s" blackboard.md:1: count-shaped string" EXPECT-ONE
   s" blackboard.md" SUB-RM ;

: TEST-NESTED-BLACKBOARD-COUNT ( -- )   \ the skip is root-anchored, not any-depth
   RESET-FILES
   s" notes/.blackboard" SUB-MKDIRS
   s" notes/.blackboard/x.md" BOARD$ SUB-WRITE
   s" notes/.blackboard/x.md:1: count-shaped string" EXPECT-ONE
   s" notes" SUB-RM ;

: TEST-SIBLING-BLACKBOARD-COUNT ( -- )   \ the trailing slash keeps neighbours in scope
   RESET-FILES
   s" .blackboard-archive" SUB-MKDIRS
   s" .blackboard-archive/msg.md" BOARD$ SUB-WRITE
   s" .blackboard-archive/msg.md:1: count-shaped string" EXPECT-ONE
   s" .blackboard-archive" SUB-RM ;

: MAIN ( -- )
   T-RESET
   PREPARE
   TEST-CLEAN
   TEST-STALE-DATE
   TEST-BAD-STATUS-DATE
   TEST-BAD-TODAY
   TEST-COUNT-PROSE
   TEST-COUNT-TRIPLE
   TEST-COUNT-UNCHECKABLE
   TEST-COUNT-UPPERCASE
   TEST-SHORT-COUNT
   TEST-EMBEDDED-COUNT
   TEST-PARTIAL-RATIO
   TEST-FENCED-COUNTS
   TEST-LONG-MARKDOWN
   TEST-SKIP-JJ-WS
   TEST-SKIP-MAKI
   TEST-SKIP-BLACKBOARD
   TEST-TRACKED-DOC-COUNT
   TEST-BLACKBOARD-NAME-COUNT
   TEST-NESTED-BLACKBOARD-COUNT
   TEST-SIBLING-BLACKBOARD-COUNT
   CLEANUP-RUN
   ROOT EXISTS? TFALSE
   T-REPORT
   s" stale-status-lint-test: ok" type cr ;

MAIN
;package
