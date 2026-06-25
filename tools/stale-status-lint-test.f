\ stale-status-lint-test.f - checked fixtures for tools/stale-status-lint.f.
\ Run: bin/hb --load lib/errors.f lib/string.f lib/test.f lib/fs.f lib/fs-mutate.f lib/process.f lib/process-argv.f tools/warm-run.f tools/stale-status-lint-test.f

4096 constant SST-CAP
1050 constant SST-LONG-LINES
10 constant SST-LF-C

variable SST-ROOT-U
variable SST-STATUS-U
variable SST-LESSONS-U
variable SST-README-U
variable SST-JJ-U
variable SST-JJ-DIR-U

create SST-ROOT-BUF FS-PATH-CAP allot
create SST-STATUS-BUF FS-PATH-CAP allot
create SST-LESSONS-BUF FS-PATH-CAP allot
create SST-README-BUF FS-PATH-CAP allot
create SST-JJ-BUF FS-PATH-CAP allot
create SST-JJ-DIR-BUF FS-PATH-CAP allot
create SST-OUT SST-CAP allot
create SST-ERR SST-CAP allot

: SST-COPY! ( ptr u8 n ptr u8 ptr n -- ) {: a:ptr u dst:ptr lenp:ptr :}
   u FS-PATH-CAP > if E-FS-PATH throw then
   a dst u BYTE-COPY
   u lenp ! ;

: SST-PATH! ( ptr u8 n ptr u8 n ptr u8 ptr n -- ) {: pa:ptr pu na:ptr nu dst:ptr lenp:ptr :}
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

: SST-LF ( -- )
   SST-LF-C SB-APPEND-C ;

: SST-EMPTY$ ( -- ptr u8 n )
   SB-RESET
   SB$ ;

: SST-STATUS$ ( ptr u8 n -- ptr u8 n ) {: date:ptr dateu :}
   SB-RESET
   s" # Status" SB-APPEND SST-LF
   SST-LF
   s" Last verified: " SB-APPEND date dateu SB-APPEND SST-LF
   s" Certified: 979  Uncheckable: 0  Rejected: 0" SB-APPEND SST-LF
   SB$ ;

: SST-LESSONS$ ( -- ptr u8 n )
   SB-RESET
   s" Historical 783/0/0 count is allowed here." SB-APPEND SST-LF
   SB$ ;

: SST-README-GOOD$ ( -- ptr u8 n )
   SB-RESET
   s" No live count here." SB-APPEND SST-LF
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

: SST-ARG+ ( ptr u8 n -- )
   >LEN PROC-ARGV+ ;

: SST-PREPARE ( -- )
   CLEANUP-RESET
   s" habu-stale-status" TMPDIR-MKDIR {: a:ptr u :}
   a u SST-ROOT-BUF SST-ROOT-U SST-COPY!
   SST-ROOT CLEANUP-TREE+
   SST-ROOT s" STATUS.md" SST-STATUS-BUF SST-STATUS-U SST-PATH!
   SST-ROOT s" LESSONS.md" SST-LESSONS-BUF SST-LESSONS-U SST-PATH!
   SST-ROOT s" README.md" SST-README-BUF SST-README-U SST-PATH!
   SST-ROOT s" .jj-ws/master-test" SST-JJ-DIR-BUF SST-JJ-DIR-U SST-PATH!
   SST-ROOT s" .jj-ws/master-test/STATUS.md" SST-JJ-BUF SST-JJ-U SST-PATH!
   SST-RESET-FILES ;

: SST-ARGV ( ptr u8 n -- ) {: today:ptr todayu :}
   PROC-ARGV-RESET
   s" tools/stale-status-lint.f" WR-TOOLS-LOAD if
      SST-ROOT  >LEN PROC-ARGV+
      today todayu  >LEN PROC-ARGV+
      exit
   then
   s" --load"  >LEN PROC-ARGV+
   s" tools/date.f"  >LEN PROC-ARGV+
   s" lib/errors.f"  >LEN PROC-ARGV+
   s" lib/string.f"  >LEN PROC-ARGV+
   s" lib/fs.f"  >LEN PROC-ARGV+
   s" tools/lint/text.f"  >LEN PROC-ARGV+ s" tools/lint/token.f" >LEN PROC-ARGV+ s" tools/lint/lib.f" >LEN PROC-ARGV+
   s" tools/argv.f"  >LEN PROC-ARGV+
   s" tools/stale-status-lint.f"  >LEN PROC-ARGV+
   s" --"  >LEN PROC-ARGV+
   SST-ROOT  >LEN PROC-ARGV+
   today todayu  >LEN PROC-ARGV+ ;

: SST-CAPTURE>N ( len len rc -- n n n ) {: outu erru rc :}
   outu LEN>N erru LEN>N rc RC>N ;

: SST-RUN ( ptr u8 n -- n n n )
   SST-ARGV
   WR-TOOLS$  >LEN SST-OUT SST-CAP >LEN SST-ERR SST-CAP >LEN
   1000 >MS RUN-ARGV-CAPTURE SST-CAPTURE>N ;

: SST-RUN-DEFAULT ( -- n n n )
   s" 2026-06-16" SST-RUN ;

: SST-EXPECT-OK ( -- )
   SST-RUN-DEFAULT 0 T=
   {: outu erru :}
   SST-OUT outu SST-GOOD-OUT$ T$=
   SST-ERR erru SST-EMPTY$ T$= ;

: SST-EXPECT-BAD-TODAY ( ptr u8 n ptr u8 n ptr u8 n -- ) {: today:ptr todayu code:ptr codeu needle:ptr needleu :}
   today todayu SST-RUN 0 T<>
   {: outu erru :}
   erru 0 T=
   SST-OUT outu code codeu CONTAINS? TTRUE
   needleu 0 > if SST-OUT outu needle needleu CONTAINS? TTRUE then ;

: SST-EXPECT-BAD ( ptr u8 n ptr u8 n -- ) {: code:ptr codeu needle:ptr needleu :}
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
   s" BAD-STATUS-DATE" s" Last verified invalid `2026-02-29`" SST-EXPECT-BAD ;

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
   s" STALE-STATUS" s" README.md:1: count-shaped string" SST-EXPECT-BAD ;

: SST-TEST-COUNT-UNCHECKABLE ( -- )
   SST-RESET-FILES
   SST-README SST-README-UNCHECKABLE$ WRITE-ALL
   s" STALE-STATUS" s" README.md:1: count-shaped string" SST-EXPECT-BAD ;

: SST-TEST-COUNT-UPPERCASE ( -- )
   SST-RESET-FILES
   SST-README SST-README-UPPER-COUNT$ WRITE-ALL
   s" STALE-STATUS" s" README.md:1: count-shaped string" SST-EXPECT-BAD ;

: SST-TEST-SHORT-COUNT ( -- )
   SST-RESET-FILES
   SST-README SST-README-SHORT-COUNT$ WRITE-ALL
   SST-EXPECT-OK ;

: SST-TEST-EMBEDDED-COUNT ( -- )
   SST-RESET-FILES
   SST-README SST-README-EMBEDDED-COUNT$ WRITE-ALL
   SST-EXPECT-OK ;

: SST-TEST-PARTIAL-RATIO ( -- )
   SST-RESET-FILES
   SST-README SST-README-PARTIAL-RATIO$ WRITE-ALL
   SST-EXPECT-OK ;

: SST-TEST-FENCED-COUNTS ( -- )
   SST-RESET-FILES
   SST-README SST-README-FENCE$ WRITE-ALL
   SST-EXPECT-OK ;

: SST-TEST-LONG-MARKDOWN ( -- )
   SST-RESET-FILES
   SST-WRITE-LONG-README
   SST-EXPECT-OK ;

: SST-TEST-SKIP-JJ-WS ( -- )
   SST-RESET-FILES
   SST-JJ-DIR MAKE-DIRS
   SST-JJ s" 890 certified" WRITE-ALL
   SST-EXPECT-OK ;

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
   CLEANUP-RUN
   SST-ROOT EXISTS? TFALSE
   T-REPORT
   s" stale-status-lint-test: ok" type cr ;

SST-MAIN
