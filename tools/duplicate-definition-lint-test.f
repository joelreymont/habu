\ duplicate-definition-lint-test.f - checked fixtures for duplicate-definition-lint.
\ Run: bin/hb --load lib/errors.f lib/string.f lib/test.f lib/fs.f
\ lib/fs-mutate.f lib/process.f lib/process-argv.f tools/warm-run.f
\ tools/duplicate-definition-lint-test.f

$1000 constant DDLT-BUF-CAP
$2710 constant DDLT-TIMEOUT-MS

create DDLT-ROOT FS-PATH-CAP allot
create DDLT-GOOD FS-PATH-CAP allot
create DDLT-BAD-A FS-PATH-CAP allot
create DDLT-BAD-B FS-PATH-CAP allot
create DDLT-CASE FS-PATH-CAP allot
create DDLT-OUT DDLT-BUF-CAP allot
create DDLT-ERR DDLT-BUF-CAP allot

variable DDLT-ROOT-U
variable DDLT-GOOD-U
variable DDLT-BAD-A-U
variable DDLT-BAD-B-U
variable DDLT-CASE-U

: DDLT-COPY! ( ptr u8 n ptr u8 ptr n -- ) {: a:ptr u dst:ptr lenp:ptr :}
   a dst u BYTE-COPY
   u lenp ! ;

: DDLT-ROOT$ ( -- ptr u8 n )
   DDLT-ROOT DDLT-ROOT-U @ ;

: DDLT-GOOD$ ( -- ptr u8 n )
   DDLT-GOOD DDLT-GOOD-U @ ;

: DDLT-BAD-A$ ( -- ptr u8 n )
   DDLT-BAD-A DDLT-BAD-A-U @ ;

: DDLT-BAD-B$ ( -- ptr u8 n )
   DDLT-BAD-B DDLT-BAD-B-U @ ;

: DDLT-CASE$ ( -- ptr u8 n )
   DDLT-CASE DDLT-CASE-U @ ;

: DDLT-LF ( -- )
   $0A SB-APPEND-C ;

: DDLT-GOOD-SRC$ ( -- ptr u8 n )
   SB-RESET
   s" : OK ( -- n ) 1 ;" SB-APPEND DDLT-LF
   s" variable STATE" SB-APPEND DDLT-LF
   s" 7 constant LIMIT" SB-APPEND DDLT-LF
   SB$ ;

: DDLT-BAD-A-SRC$ ( -- ptr u8 n )
   SB-RESET
   s" : RESET ( -- n ) 1 ;" SB-APPEND DDLT-LF
   s" variable LCH" SB-APPEND DDLT-LF
   SB$ ;

: DDLT-BAD-B-SRC$ ( -- ptr u8 n )
   SB-RESET
   s" : OTHER ( -- n ) 2 ;" SB-APPEND DDLT-LF
   s" : LCH ( -- n ) 3 ;" SB-APPEND DDLT-LF
   SB$ ;

: DDLT-CASE-SRC$ ( -- ptr u8 n )
   SB-RESET
   s" : RESET ( -- n ) 1 ;" SB-APPEND DDLT-LF
   s" : reset ( -- n ) 2 ;" SB-APPEND DDLT-LF
   SB$ ;

: DDLT-PREPARE ( -- )
   CLEANUP-RESET
   s" habu-duplicate-definition-lint" TMPDIR-MKDIR DDLT-ROOT DDLT-ROOT-U DDLT-COPY!
   DDLT-ROOT$ CLEANUP-TREE+
   DDLT-ROOT$ s" good.f" DDLT-GOOD JOIN-PATH DDLT-GOOD-U !
   DDLT-ROOT$ s" bad-a.f" DDLT-BAD-A JOIN-PATH DDLT-BAD-A-U !
   DDLT-ROOT$ s" bad-b.f" DDLT-BAD-B JOIN-PATH DDLT-BAD-B-U !
   DDLT-ROOT$ s" case.f" DDLT-CASE JOIN-PATH DDLT-CASE-U !
   DDLT-GOOD$ DDLT-GOOD-SRC$ WRITE-ALL
   DDLT-BAD-A$ DDLT-BAD-A-SRC$ WRITE-ALL
   DDLT-BAD-B$ DDLT-BAD-B-SRC$ WRITE-ALL
   DDLT-CASE$ DDLT-CASE-SRC$ WRITE-ALL ;

: DDLT-ARG+ ( ptr u8 n -- )
   >LEN PROC-ARGV+ ;

: DDLT-ARGV-LOAD ( -- )
   PROC-ARGV-RESET
   s" tools/duplicate-definition-lint.f" WR-TOOLS-LOAD if exit then
   s" --load" DDLT-ARG+
   s" lib/errors.f" DDLT-ARG+
   s" lib/string.f" DDLT-ARG+
   s" lib/memory.f" DDLT-ARG+
   s" lib/vector.f" DDLT-ARG+
   s" lib/fs.f" DDLT-ARG+
   s" tools/lint/text.f" DDLT-ARG+
   s" tools/lint/token.f" DDLT-ARG+
   s" tools/lint/lib.f" DDLT-ARG+
   s" tools/lint/json-writer.f" DDLT-ARG+
   s" tools/lint/source-lex.f" DDLT-ARG+
   s" tools/duplicate-definition-lint-core.f" DDLT-ARG+
   s" tools/argv.f" DDLT-ARG+
   s" tools/duplicate-definition-lint.f" DDLT-ARG+
   s" --" DDLT-ARG+ ;

: DDLT-CAPTURE>N ( len len n n -- n n n n ) {: outu erru kind code :}
   outu LEN>N erru LEN>N kind code ;

: DDLT-CAPTURE ( -- n n n n )
   WR-TOOLS$ >LEN DDLT-OUT DDLT-BUF-CAP >LEN DDLT-ERR DDLT-BUF-CAP >LEN
   DDLT-TIMEOUT-MS >MS RUN-ARGV-CAPTURE-OUTCOME
   DDLT-CAPTURE>N ;

: DDLT-RUN-GOOD ( -- n n n n )
   DDLT-ARGV-LOAD
   DDLT-GOOD$ DDLT-ARG+
   DDLT-CAPTURE ;

: DDLT-RUN-CROSS ( -- n n n n )
   DDLT-ARGV-LOAD
   DDLT-BAD-A$ DDLT-ARG+
   DDLT-BAD-B$ DDLT-ARG+
   DDLT-CAPTURE ;

: DDLT-RUN-JSON ( -- n n n n )
   DDLT-ARGV-LOAD
   s" --json" DDLT-ARG+
   s" --label" DDLT-ARG+
   s" <stage2-src>" DDLT-ARG+
   DDLT-CASE$ DDLT-ARG+
   DDLT-CAPTURE ;

: DDLT-JSON-WORD-RESET$ ( -- ptr u8 n )
   SB-RESET
   $22 SB-APPEND-C
   s" word" SB-APPEND
   $22 SB-APPEND-C
   $3A SB-APPEND-C
   $22 SB-APPEND-C
   s" reset" SB-APPEND
   $22 SB-APPEND-C
   SB$ ;

: DDLT-EXPECT-EXIT ( n n n n n -- n n ) {: outu erru kind code want :}
   kind PROC-OUTCOME-EXIT T=
   code want T=
   outu erru ;

: DDLT-TEST-GOOD ( -- )
   DDLT-RUN-GOOD 0 DDLT-EXPECT-EXIT {: outu erru :}
   outu 0 T=
   erru 0 T= ;

: DDLT-TEST-CROSS ( -- )
   DDLT-RUN-CROSS 1 DDLT-EXPECT-EXIT {: outu erru :}
   erru 0 T=
   DDLT-OUT outu s" E-DUPLICATE-DEFINITION" CONTAINS? TTRUE
   DDLT-OUT outu s" `LCH`" CONTAINS? TTRUE
   DDLT-OUT outu s" bad-a.f" CONTAINS? TTRUE
   DDLT-OUT outu s" bad-b.f" CONTAINS? TTRUE ;

: DDLT-TEST-JSON ( -- )
   DDLT-RUN-JSON 1 DDLT-EXPECT-EXIT {: outu erru :}
   erru 0 T=
   DDLT-OUT outu s" schema_version" CONTAINS? TTRUE
   DDLT-OUT outu s" E-DUPLICATE-DEFINITION" CONTAINS? TTRUE
   DDLT-OUT outu s" <stage2-src>" CONTAINS? TTRUE
   DDLT-OUT outu DDLT-JSON-WORD-RESET$ CONTAINS? TTRUE ;

: DDLT-MAIN ( -- )
   T-RESET
   DDLT-PREPARE
   DDLT-TEST-GOOD
   DDLT-TEST-CROSS
   DDLT-TEST-JSON
   CLEANUP-RUN
   DDLT-ROOT$ EXISTS? TFALSE
   T-REPORT
   s" duplicate-definition-lint-test: ok" type cr ;

DDLT-MAIN
