\ stale-status-lint-test.f - checked fixtures for tools/stale-status-lint.f.
\ Run: bin/hb --load tools/date.f lib/errors.f lib/string.f lib/test.f
\ lib/memory.f lib/fs.f lib/fs-mutate.f lib/process.f
\ tools/lint/text.f tools/lint/token.f tools/lint/lib.f tools/argv.f
\ tools/stale-status-lint-core.f tools/stale-status-lint-test.f

require tools/date.f
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
require tools/argv.f
require tools/stale-status-lint-core.f

4096 constant SST-CAP
32 constant SST-DATE-CAP
1050 constant SST-LONG-LINES
10 constant SST-LF-C

variable SST-ROOT-U
variable SST-STATUS-U
variable SST-LESSONS-U
variable SST-README-U
variable SST-JJ-U
variable SST-JJ-DIR-U
variable SST-MAKI-DIR-U
variable SST-MAKI-STATUS-U
variable SST-CORE-TODAY-U

create SST-ROOT-BUF FS-PATH-CAP allot
create SST-STATUS-BUF FS-PATH-CAP allot
create SST-LESSONS-BUF FS-PATH-CAP allot
create SST-README-BUF FS-PATH-CAP allot
create SST-JJ-BUF FS-PATH-CAP allot
create SST-JJ-DIR-BUF FS-PATH-CAP allot
create SST-MAKI-DIR-BUF FS-PATH-CAP allot
create SST-MAKI-STATUS-BUF FS-PATH-CAP allot
create SST-CORE-TODAY-BUF SST-DATE-CAP allot
create SST-OUT SST-CAP allot
create SST-ERR SST-CAP allot

: SST-COPY! ( ptr u8 n ptr u8 ptr n -- ) {: a:ptr u:n dst:ptr lenp:ptr :}
   u FS-PATH-CAP > if E-FS-PATH throw then
   a dst u BYTE-COPY
   u lenp ! ;

: SST-PATH! ( ptr u8 n ptr u8 n ptr u8 ptr n -- ) {: pa:ptr pu:n na:ptr nu:n dst:ptr lenp:ptr :}
   pa pu na nu dst JOIN-PATH lenp ! ;

: SST-ROOT ( -- ptr u8 n )
   SST-ROOT-BUF SST-ROOT-U @ ;

: SST-STATUS ( -- ptr u8 n )
   SST-STATUS-BUF SST-STATUS-U @ ;

: SST-LESSONS ( -- ptr u8 n )
   SST-LESSONS-BUF SST-LESSONS-U @ ;

: SST-README ( -- ptr u8 n )
   SST-README-BUF SST-README-U @ ;

: SST-JJ ( -- ptr u8 n )
   SST-JJ-BUF SST-JJ-U @ ;

: SST-JJ-DIR ( -- ptr u8 n )
   SST-JJ-DIR-BUF SST-JJ-DIR-U @ ;

: SST-MAKI-DIR ( -- ptr u8 n )
   SST-MAKI-DIR-BUF SST-MAKI-DIR-U @ ;

: SST-MAKI-STATUS ( -- ptr u8 n )
   SST-MAKI-STATUS-BUF SST-MAKI-STATUS-U @ ;

: SST-CORE-TODAY ( -- ptr u8 n )
   SST-CORE-TODAY-BUF SST-CORE-TODAY-U @ ;

: SST-LF ( -- )
   SST-LF-C SB-APPEND-C ;

: SST-EMPTY$ ( -- ptr u8 n )
   SB-RESET
   SB$ ;

: SST-STATUS$ ( ptr u8 n -- ptr u8 n ) {: date:ptr dateu:n :}
   SB-RESET
   s" # Status" SB-APPEND SST-LF
   SST-LF
   s" Last verified: " SB-APPEND date dateu SB-APPEND SST-LF
   s" Certified: 987  Uncheckable: 0  Rejected: 0" SB-APPEND SST-LF
   SB$ ;

: SST-LESSONS$ ( -- ptr u8 n )
   SB-RESET
   s" Historical 783/0/0 count is allowed here." SB-APPEND SST-LF
   SB$ ;

: SST-README-GOOD$ ( -- ptr u8 n )
   SB-RESET
   s" No live count here." SB-APPEND SST-LF
   SB$ ;

: SST-MAKI-STATUS$ ( -- ptr u8 n )
   SB-RESET
   s" Maki can say 987 certified here because it is fenced from root status lint." SB-APPEND SST-LF
   SB$ ;

: SST-README-COUNT$ ( -- ptr u8 n )
   SB-RESET
   s" This stale count says 890 certified in prose." SB-APPEND SST-LF
   SB$ ;

: SST-README-TRIPLE$ ( -- ptr u8 n )
   SB-RESET
   s" This stale count says 890/0/0 in prose." SB-APPEND SST-LF
   SB$ ;

: SST-README-UNCHECKABLE$ ( -- ptr u8 n )
   SB-RESET
   s" This stale count says 890 uncheckable in prose." SB-APPEND SST-LF
   SB$ ;

: SST-README-UPPER-COUNT$ ( -- ptr u8 n )
   SB-RESET
   s" This stale count says 890 CERTIFIED in prose." SB-APPEND SST-LF
   SB$ ;

: SST-README-SHORT-COUNT$ ( -- ptr u8 n )
   SB-RESET
   s" This short count says 89 certified in prose." SB-APPEND SST-LF
   SB$ ;

: SST-README-EMBEDDED-COUNT$ ( -- ptr u8 n )
   SB-RESET
   s" This embedded count says A890 certified in prose." SB-APPEND SST-LF
   SB$ ;

: SST-README-PARTIAL-RATIO$ ( -- ptr u8 n )
   SB-RESET
   s" This partial ratio says 890/0x in prose." SB-APPEND SST-LF
   SB$ ;

: SST-README-FENCE$ ( -- ptr u8 n )
   SB-RESET
   s" ```text" SB-APPEND SST-LF
   s" llm-results: rows=290 certified=195 tests=210" SB-APPEND SST-LF
   s" ```" SB-APPEND SST-LF
   SB$ ;

: SST-WRITE-LONG-README ( -- )
   SST-README SST-EMPTY$ WRITE-ALL
   0 begin dup SST-LONG-LINES < while
      SST-README SST-README-GOOD$ APPEND-FILE
      1+
   repeat drop ;

: SST-GOOD-OUT$ ( -- ptr u8 n )
   SB-RESET
   s" stale-status-lint: 0 finding(s)" SB-APPEND SST-LF
   SB$ ;

: SST-WRITE-STATUS ( ptr u8 n -- )
   SST-STATUS 2swap SST-STATUS$ WRITE-ALL ;

: SST-RESET-FILES ( -- )
   s" 2026-06-16" SST-WRITE-STATUS
   SST-LESSONS SST-LESSONS$ WRITE-ALL
   SST-README SST-README-GOOD$ WRITE-ALL ;

: SST-PREPARE ( -- )
   CLEANUP-RESET
   s" habu-stale-status" TMPDIR-MKDIR {: a:ptr u:n :}
   a u SST-ROOT-BUF SST-ROOT-U SST-COPY!
   SST-ROOT CLEANUP-TREE+
   SST-ROOT s" STATUS.md" SST-STATUS-BUF SST-STATUS-U SST-PATH!
   SST-ROOT s" LESSONS.md" SST-LESSONS-BUF SST-LESSONS-U SST-PATH!
   SST-ROOT s" README.md" SST-README-BUF SST-README-U SST-PATH!
   SST-ROOT s" .jj-ws/master-test" SST-JJ-DIR-BUF SST-JJ-DIR-U SST-PATH!
   SST-ROOT s" .jj-ws/master-test/STATUS.md" SST-JJ-BUF SST-JJ-U SST-PATH!
   SST-ROOT s" maki" SST-MAKI-DIR-BUF SST-MAKI-DIR-U SST-PATH!
   SST-ROOT s" maki/STATUS.md" SST-MAKI-STATUS-BUF SST-MAKI-STATUS-U SST-PATH!
   SST-RESET-FILES ;

: SST-CORE-TODAY! ( ptr u8 n -- ) {: a:ptr u:n :}
   u SST-DATE-CAP > if E-FS-PATH throw then
   a SST-CORE-TODAY-BUF u BYTE-COPY
   u SST-CORE-TODAY-U ! ;

: SST-RUN-CORE-ACT ( -- )
   SST-ROOT SS-ROOT!
   SST-CORE-TODAY SS-PARSE-TODAY SS-TODAY-DAYS !
   STALE-STATUS-LINT ;

: SST-RUN ( ptr u8 n -- n n n )
   SST-CORE-TODAY!
   SST-OUT SST-CAP SS-OUT-BUFFER!
   SST-ERR SST-CAP SS-ERR-BUFFER!
   [: SST-RUN-CORE-ACT ;] catch {: rc:n :}
   SS-OUT$ nip SS-ERR$ nip rc
   SS-BUFFERS-OFF ;

: SST-RUN-DEFAULT ( -- n n n )
   s" 2026-06-16" SST-RUN ;

: SST-EXPECT-EXIT ( n n n n -- n n ) {: outu:n erru:n code:n expect:n :}
   code expect T=
   outu erru ;

: SST-EXPECT-EXIT-NZ ( n n n -- n n ) {: outu:n erru:n code:n :}
   code 0 T<>
   outu erru ;

: SST-EXPECT-CORE-OK ( -- )
   SST-RUN-DEFAULT 0 SST-EXPECT-EXIT {: outu:n erru:n :}
   SST-OUT outu SST-GOOD-OUT$ T$=
   SST-ERR erru SST-EMPTY$ T$=
   SS-BAD @ 0 T= ;

: SST-EXPECT-CORE-BAD ( -- )
   SST-RUN-DEFAULT SST-EXPECT-EXIT-NZ {: outu:n erru:n :}
   outu 0 T<>
   erru 0 T=
   SS-BAD @ 0 > TTRUE ;

: SST-EXPECT-OK ( -- )
   SST-RUN-DEFAULT 0 SST-EXPECT-EXIT {: outu:n erru:n :}
   SST-OUT outu SST-GOOD-OUT$ T$=
   SST-ERR erru SST-EMPTY$ T$= ;

: SST-EXPECT-BAD-TODAY ( ptr u8 n ptr u8 n ptr u8 n -- ) {: today:ptr todayu:n code:ptr codeu:n needle:ptr needleu:n :}
   today todayu SST-RUN SST-EXPECT-EXIT-NZ {: outu:n erru:n :}
   erru 0 T=
   SST-OUT outu code codeu CONTAINS? TTRUE
   needleu 0 > if SST-OUT outu needle needleu CONTAINS? TTRUE then ;

: SST-EXPECT-BAD ( ptr u8 n ptr u8 n -- ) {: code:ptr codeu:n needle:ptr needleu:n :}
   s" 2026-06-16" code codeu needle needleu SST-EXPECT-BAD-TODAY ;

: SST-TEST-CLEAN ( -- )
   SST-RESET-FILES
   SST-EXPECT-OK ;

: SST-TEST-STALE-DATE ( -- )
   SST-RESET-FILES
   s" 2026-06-15" SST-WRITE-STATUS
   s" STALE-STATUS" s" Last verified is 2026-06-15, expected 2026-06-16" SST-EXPECT-BAD ;

: SST-TEST-BAD-STATUS-DATE ( -- )
   SST-RESET-FILES
   s" 2026-02-29" SST-WRITE-STATUS
   SST-EXPECT-CORE-BAD ;

: SST-TEST-BAD-TODAY ( -- )
   SST-RESET-FILES
   s" 2026-02-29" s" BAD-TODAY" s" today argument invalid `2026-02-29`" SST-EXPECT-BAD-TODAY ;

: SST-TEST-COUNT-PROSE ( -- )
   SST-RESET-FILES
   SST-README SST-README-COUNT$ WRITE-ALL
   s" STALE-STATUS" s" README.md:1: count-shaped string" SST-EXPECT-BAD ;

: SST-TEST-COUNT-TRIPLE ( -- )
   SST-RESET-FILES
   SST-README SST-README-TRIPLE$ WRITE-ALL
   SST-EXPECT-CORE-BAD ;

: SST-TEST-COUNT-UNCHECKABLE ( -- )
   SST-RESET-FILES
   SST-README SST-README-UNCHECKABLE$ WRITE-ALL
   SST-EXPECT-CORE-BAD ;

: SST-TEST-COUNT-UPPERCASE ( -- )
   SST-RESET-FILES
   SST-README SST-README-UPPER-COUNT$ WRITE-ALL
   SST-EXPECT-CORE-BAD ;

: SST-TEST-SHORT-COUNT ( -- )
   SST-RESET-FILES
   SST-README SST-README-SHORT-COUNT$ WRITE-ALL
   SST-EXPECT-CORE-OK ;

: SST-TEST-EMBEDDED-COUNT ( -- )
   SST-RESET-FILES
   SST-README SST-README-EMBEDDED-COUNT$ WRITE-ALL
   SST-EXPECT-CORE-OK ;

: SST-TEST-PARTIAL-RATIO ( -- )
   SST-RESET-FILES
   SST-README SST-README-PARTIAL-RATIO$ WRITE-ALL
   SST-EXPECT-CORE-OK ;

: SST-TEST-FENCED-COUNTS ( -- )
   SST-RESET-FILES
   SST-README SST-README-FENCE$ WRITE-ALL
   SST-EXPECT-CORE-OK ;

: SST-TEST-LONG-MARKDOWN ( -- )
   SST-RESET-FILES
   SST-WRITE-LONG-README
   SST-EXPECT-CORE-OK ;

: SST-TEST-SKIP-JJ-WS ( -- )
   SST-RESET-FILES
   SST-JJ-DIR MAKE-DIRS
   SST-JJ s" 890 certified" WRITE-ALL
   SST-EXPECT-CORE-OK ;

: SST-TEST-SKIP-MAKI ( -- )
   SST-RESET-FILES
   SST-MAKI-DIR MAKE-DIRS
   SST-MAKI-STATUS SST-MAKI-STATUS$ WRITE-ALL
   SST-EXPECT-CORE-OK ;

: SST-MAIN ( -- )
   T-RESET
   SST-PREPARE
   SST-TEST-CLEAN
   SST-TEST-STALE-DATE
   SST-TEST-BAD-STATUS-DATE
   SST-TEST-BAD-TODAY
   SST-TEST-COUNT-PROSE
   SST-TEST-COUNT-TRIPLE
   SST-TEST-COUNT-UNCHECKABLE
   SST-TEST-COUNT-UPPERCASE
   SST-TEST-SHORT-COUNT
   SST-TEST-EMBEDDED-COUNT
   SST-TEST-PARTIAL-RATIO
   SST-TEST-FENCED-COUNTS
   SST-TEST-LONG-MARKDOWN
   SST-TEST-SKIP-JJ-WS
   SST-TEST-SKIP-MAKI
   CLEANUP-RUN
   SST-ROOT EXISTS? TFALSE
   T-REPORT
   s" stale-status-lint-test: ok" type cr ;

SST-MAIN
